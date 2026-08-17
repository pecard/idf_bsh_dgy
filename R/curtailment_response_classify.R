##
## Classificacao missed/delayed/ok de curtailments -- regra partilhada
##
## Usada tanto pela analise de janela de incidentes de fatalidade
## (R/fatality_window_analysis.R, summarise_curtailment_response_window())
## como pela timeline farm-wide de resposta
## (R/curtailment_response_timeline.R) -- as duas usam exatamente a mesma
## definicao de missed/delayed, para serem diretamente comparaveis.
##
## response_flag, por curtailment:
##   "missed"  -- CONFIRMADO que a turbina nao parou (final_status ==
##                partial_or_no_stop -- ha leitura SCADA fiavel no start E no
##                end, e o end nao mostra paragem)
##   "no_data" -- NAO SABEMOS o que aconteceu (final_status == "no_data" --
##                nenhuma leitura SCADA dentro de start_end_gap_sec do sinal,
##                tipicamente por a amostragem SCADA nao coincidir tao perto
##                do trigger). NAO e' o mesmo que "missed": e' ausencia de
##                dado, nao falha confirmada -- ver R/curtailment_response.R
##                (assess_curtailment_response()). Distinguido de "missed"
##                desde 2026-08 -- antes os dois eram somados em "missed",
##                inflacionando essa contagem com casos so' de cobertura SCADA.
##   "delayed" -- parou (start/end validos), mas demorou mais que
##                shutdown_high_cut_sec a atingir o 1º limiar de rpm (o maior
##                valor de shutdown_thresholds)
##   "ok"      -- parou dentro do tempo esperado
##
## Depende de: data.table, R/curtailment_response.R (assess_curtailment_response),
## R/curtailment_shutdown_time.R (time_to_rpm_thresholds) -- fazer source
## destes 2 antes
##
## Uso:
##   source("R/curtailment_response.R")
##   source("R/curtailment_shutdown_time.R")
##   source("R/curtailment_response_classify.R")
##
##   response_dt <- classify_response_flag(
##     curtl_dt, scada_dt,
##     start_end_gap_sec = curtailment_start_end_gap_sec, max_next_gap_sec = curtailment_max_next_gap_sec,
##     drop_pct_threshold = curtailment_drop_pct_threshold, rpm_threshold = safe_shutdown_rpm,
##     shutdown_thresholds = shutdown_time_thresholds, shutdown_high_cut_sec = shutdown_time_high_cut
##   )
##
##   by_flag_dt <- summarise_response_by_flag(response_dt) # n, pct_of_total, pct_of_known (exclui no_data)
##

classify_response_flag <- function(curtl_dt, scada_dt,
                                   start_end_gap_sec = 2, max_next_gap_sec = 20,
                                   drop_pct_threshold = 0.10, rpm_threshold = 1,
                                   shutdown_thresholds = c(2, 1, 0), shutdown_high_cut_sec = 50) {

  assess_dt <- assess_curtailment_response(
    curtl_dt, scada_dt, start_end_gap_sec = start_end_gap_sec,
    max_next_gap_sec = max_next_gap_sec, drop_pct_threshold = drop_pct_threshold,
    rpm_threshold = rpm_threshold
  )

  if (nrow(assess_dt) == 0L) {
    assess_dt[, time_to_first_threshold_sec := numeric()]
    assess_dt[, response_flag := character()]
    return(assess_dt[])
  }

  tt_dt <- time_to_rpm_thresholds(
    curtl_dt, scada_dt, thresholds = shutdown_thresholds, start_end_gap_sec = start_end_gap_sec
  )
  first_threshold <- max(shutdown_thresholds)
  tt_first <- tt_dt[threshold == first_threshold, .(curtailment_id, time_to_first_threshold_sec = time_to_threshold_sec)]

  out <- merge(assess_dt, tt_first, by = "curtailment_id", all.x = TRUE)

  out[, response_flag := data.table::fcase(
    final_status == "partial_or_no_stop", "missed",
    final_status == "no_data", "no_data",
    !is.na(time_to_first_threshold_sec) & time_to_first_threshold_sec > shutdown_high_cut_sec, "delayed",
    default = "ok"
  )]

  out[]
}


## Sumario por response_flag -- contagem, % do total (incluindo no_data) e %
## dos casos CONHECIDOS (missed+delayed+ok, excluindo no_data). no_data fica
## com pct_of_known = NA -- nao faz sentido pedir "que % dos casos e' que
## sao casos sem dado", ja e' o proprio universo excluido.
##
## pct_of_total mostra a dimensao real do no_data (nao esconder que grande
## parte dos curtailments pode nao ter leitura SCADA fiavel perto do
## sinal); pct_of_known responde a pergunta "dos curtailments que
## conseguimos classificar, que % pararam bem / falharam / demoraram" --
## normalmente a leitura mais relevante para o relatorio.

summarise_response_by_flag <- function(response_dt) {

  empty <- data.table::data.table(
    response_flag = character(), n = integer(), pct_of_total = numeric(), pct_of_known = numeric()
  )
  if (nrow(response_dt) == 0L) return(empty)

  by_flag <- response_dt[, .(n = .N), by = response_flag]

  n_total <- sum(by_flag$n)
  n_known <- sum(by_flag[response_flag != "no_data", n])

  by_flag[, pct_of_total := round(100 * n / n_total, 1)]
  by_flag[, pct_of_known := data.table::fifelse(
    response_flag == "no_data", NA_real_,
    round(100 * n / n_known, 1)
  )]

  data.table::setorder(by_flag, -n)
  by_flag[]
}
