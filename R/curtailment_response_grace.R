##
## Variante experimental de classify_response_flag() (R/curtailment_response_classify.R)
## -- testa a hipotese (Paulo, 2026-08, a partir dos plots de exemplo da
## secção "Example Response Profiles") de que alguns curtailments
## classificados "missed" (final_status == partial_or_no_stop -- RPM ainda
## acima do limiar EXATAMENTE no timestamp de fim da propria ordem de
## curtailment) na verdade PARARAM, so' que depois do fim da ordem -- a
## inercia mecanica da turbina pode levar mais tempo a completar a paragem
## do que a duracao da propria ordem de curtailment.
##
## Nao substitui classify_response_flag() na pipeline de producao
## (IDF_analysis.R) -- e' so' para o script de exploracao
## (explore_curtailment_response_grace.R) comparar classificacoes
## side-by-side antes de decidir se isto deve passar a ser o comportamento
## por omissao (e, se sim, com que grace_after_end_sec).
##
## Depende de: data.table, R/curtailment_response.R, R/curtailment_shutdown_time.R,
## R/curtailment_response_classify.R (fazer source destes 3 antes)
##


## grace_after_end_sec: janela adicional (segundos) DEPOIS do "end" da
## ordem de curtailment onde se procura a primeira leitura de SCADA com
## RPM < rpm_threshold. Se encontrada dentro dessa janela, o evento deixa
## de ser "missed" e passa a "delayed" (parou, mas mais tarde que o
## esperado pela duracao da propria ordem) -- coerente com as outras 3
## categorias (missed/delayed/ok/no_data). grace_after_end_sec = 0 reproduz
## exatamente classify_response_flag() (sem reclassificacao nenhuma).
##
## Colunas adicionais no resultado, so' preenchidas para os eventos
## reclassificados: grace_stop_time (1ª leitura SCADA < rpm_threshold
## dentro da janela extra) e grace_stop_sec (esse instante, em segundos
## desde o INICIO do curtailment -- diretamente comparavel a
## time_to_first_threshold_sec).

classify_response_flag_grace <- function(curtl_dt, scada_dt, grace_after_end_sec = 20,
                                         start_end_gap_sec = 2, max_next_gap_sec = 20,
                                         drop_pct_threshold = 0.10, rpm_threshold = 1,
                                         shutdown_thresholds = c(2, 1, 0), shutdown_high_cut_sec = 50) {

  out <- classify_response_flag(
    curtl_dt, scada_dt, start_end_gap_sec = start_end_gap_sec,
    max_next_gap_sec = max_next_gap_sec, drop_pct_threshold = drop_pct_threshold,
    rpm_threshold = rpm_threshold, shutdown_thresholds = shutdown_thresholds,
    shutdown_high_cut_sec = shutdown_high_cut_sec
  )

  tz <- attr(curtl_dt$start, "tzone")
  out[, grace_stop_time := as.POSIXct(NA, tz = tz)]
  out[, grace_stop_sec  := NA_real_]

  if (nrow(out) == 0L || grace_after_end_sec <= 0) return(out[])

  missed_idx <- which(out$response_flag == "missed")
  if (length(missed_idx) == 0L) return(out[])

  rpm_dt <- scada_dt[readingname == "RPM", .(turbine = turbinelabel, datetime, rpm = value)]
  data.table::setkey(rpm_dt, turbine, datetime)

  for (i in missed_idx) {
    ev <- out[i]
    window_end <- ev$end + grace_after_end_sec
    cand <- rpm_dt[turbine == ev$turbine & datetime > ev$end & datetime <= window_end & rpm < rpm_threshold]
    if (nrow(cand) > 0L) {
      data.table::setorder(cand, datetime)
      out[i, `:=`(
        response_flag  = "delayed",
        grace_stop_time = cand$datetime[1],
        grace_stop_sec  = as.numeric(difftime(cand$datetime[1], ev$start, units = "secs"))
      )]
    }
  }

  out[]
}


## Resumo comparativo -- para cada grace_after_end_sec testado, quantos
## eventos ficam em cada response_flag (mesmas colunas de
## summarise_response_by_flag(), R/curtailment_response_classify.R, com
## grace_after_end_sec como coluna extra para comparar lado a lado).

compare_grace_windows <- function(curtl_dt, scada_dt, grace_candidates_sec = c(0, 20, 40, 60, 90, 120),
                                  start_end_gap_sec = 2, max_next_gap_sec = 20,
                                  drop_pct_threshold = 0.10, rpm_threshold = 1,
                                  shutdown_thresholds = c(2, 1, 0), shutdown_high_cut_sec = 50) {

  out <- data.table::rbindlist(lapply(grace_candidates_sec, function(g) {
    dt <- classify_response_flag_grace(
      curtl_dt, scada_dt, grace_after_end_sec = g,
      start_end_gap_sec = start_end_gap_sec, max_next_gap_sec = max_next_gap_sec,
      drop_pct_threshold = drop_pct_threshold, rpm_threshold = rpm_threshold,
      shutdown_thresholds = shutdown_thresholds, shutdown_high_cut_sec = shutdown_high_cut_sec
    )
    by_flag <- summarise_response_by_flag(dt)
    by_flag[, grace_after_end_sec := g]
    by_flag[]
  }))

  data.table::setcolorder(out, c("grace_after_end_sec", "response_flag", "n", "pct_of_total", "pct_of_known"))
  data.table::setorder(out, grace_after_end_sec, response_flag)
  out[]
}
