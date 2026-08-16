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
##   "missed"  -- a turbina nao confirmou ter parado (final_status
##                partial_or_no_stop/no_data -- ver assess_curtailment_response()
##                em R/curtailment_response.R)
##   "delayed" -- parou, mas demorou mais que shutdown_high_cut_sec a atingir
##                o 1º limiar de rpm (o maior valor de shutdown_thresholds)
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
    final_status %in% c("partial_or_no_stop", "no_data"), "missed",
    !is.na(time_to_first_threshold_sec) & time_to_first_threshold_sec > shutdown_high_cut_sec, "delayed",
    default = "ok"
  )]

  out[]
}
