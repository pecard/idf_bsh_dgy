##
## Sumario tabular de volume e janela temporal dos datasets analisados
##
## Da apoio a escrita de relatorios: cada seccao de um relatorio tecnico
## deve ser auto-explicativa quanto ao universo de dados que usa (n de
## linhas, periodo coberto) -- ver CLAUDE.md. Como os varios conjuntos e
## subconjuntos usados ao longo do IDF_analysis.R tem volumes e periodos
## muito distintos entre si (ex: track_dt farm-wide vs. safe_dist_dt so
## para BSH54/BSH62 na janela do SCADA), este modulo produz, para uma lista
## de datasets, uma linha por dataset com n de linhas e janela temporal.
##
## Modulo generico -- nao sabe nada sobre o significado de cada dataset,
## so extrai n_rows/date_from/date_to/n_days a partir da coluna de
## data/hora indicada para cada um. Quem chama e' responsavel por agrupar
## os datasets certos em cada lista (ex: uma lista para o panorama geral do
## parque, outra para a analise especifica de turbinas).
##
## Depende de: data.table
##
## Uso:
##   source("R/dataset_summary.R")
##
##   general_data_summary_dt <- summarise_dataset_extent(list(
##     Tracks       = list(dt = track_dt,  date_col = "timestamp"),
##     Curtailments = list(dt = curtl_dt,  date_col = "start"),
##     SCADA        = list(dt = scada_dt,  date_col = "datetime"),
##     Heartbeats   = list(dt = heartb_dt, date_col = "timestamp")
##   ))
##
##   turbine_data_summary_dt <- summarise_dataset_extent(list(
##     Curtailments_BSH54_BSH62 = list(dt = curtl_scada_dt, date_col = "start"),
##     Shutdown_time            = list(dt = tt_dt,          date_col = "start"),
##     Safe_distance             = list(dt = safe_dist_dt,   date_col = "start"),
##     Fatality_tracks           = list(dt = fatality_tracks_dt, date_col = "first_time")
##   ))
##

summarise_dataset_extent <- function(dt_list) {

  res <- lapply(names(dt_list), function(nm) {
    entry    <- dt_list[[nm]]
    dt       <- entry$dt
    date_col <- entry$date_col
    dates    <- dt[[date_col]]
    has_dates <- length(dates) > 0L && any(!is.na(dates))

    data.table::data.table(
      dataset   = nm,
      n_rows    = nrow(dt),
      date_from = if (has_dates) min(dates, na.rm = TRUE) else as.POSIXct(NA),
      date_to   = if (has_dates) max(dates, na.rm = TRUE) else as.POSIXct(NA),
      n_days    = if (has_dates) {
        round(as.numeric(difftime(max(dates, na.rm = TRUE), min(dates, na.rm = TRUE), units = "days")), 1)
      } else {
        NA_real_
      }
    )
  })

  data.table::rbindlist(res)[]
}
