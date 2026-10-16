##
## Disponibilidade das unidades IDF para VARIAS turbinas/unidades em simultaneo
## (relatorio dedicado, run_idf_availability.R): heartbeats cruzados com
## curtailments e SCADA, por unidade IDF, mais relacao turbina <-> unidade
## primaria/secundaria.
##
## Relacao turbina <-> IDF (idf_relation_*): ficheiro MANUAL (colunas
## `Turbine ID`, `Primary IDF`, `Secondary IDF(s)`, o mesmo formato das
## matrizes ACWA) quando existir; senao GEOMETRICA -- por turbina, as
## unidades cujo buffer (idf_op_detection_range) cobre >= min_pct_coverage %
## do buffer da turbina, ordenadas por % (a 1a e' a primaria, as restantes
## secundarias).
##
## Unidades sao identificadas internamente pelo rotulo "IDF<NN>" (numero da
## unidade, R/idf_labels.R) e mostradas com o nome completo do heartbeat
## (ex: "GW32-22").
##
## Depende de: data.table, R/idf_labels.R, R/availability_daylight.R,
## R/offline_curtailment_check.R, R/monthly_technical_summary.R,
## R/fatality_window_analysis.R (summarise_availability_window(),
## summarise_offline_evidence_window())
##

## 1. Relacao turbina <-> unidade IDF (primaria/secundaria) ----

## coverage_dt: compute_turbine_idf_coverage(), R/turbine_idf_coverage.R
## (turbine, idf "IDF<NN>", pct_coverage). Devolve: turbine, idf, role
## ("Primary"/"Secondary"), rank (1 = primaria), pct_coverage, relation_source.
idf_relation_geometric <- function(coverage_dt, min_pct_coverage = 20, max_secondary = Inf, turbines = NULL) {
  empty <- data.table::data.table(
    turbine = character(), idf = character(), role = character(), rank = integer(),
    pct_coverage = numeric(), relation_source = character()
  )
  if (is.null(coverage_dt) || nrow(coverage_dt) == 0L) return(empty)

  dt <- data.table::copy(data.table::as.data.table(coverage_dt))[, .(turbine, idf, pct_coverage)]
  if (!is.null(turbines)) dt <- dt[turbine %in% turbines]
  dt <- dt[pct_coverage >= min_pct_coverage]
  if (nrow(dt) == 0L) return(empty)

  data.table::setorder(dt, turbine, -pct_coverage, idf)
  dt[, rank := seq_len(.N), by = turbine]
  dt <- dt[rank <= 1L + max_secondary]
  dt[, role := data.table::fifelse(rank == 1L, "Primary", "Secondary")]
  dt[, relation_source := "geometric"]
  data.table::setcolorder(dt, c("turbine", "idf", "role", "rank", "pct_coverage", "relation_source"))
  dt[]
}

## manual_dt: lido do xlsx -- `Turbine ID`, `Primary IDF`, `Secondary IDF(s)`
## (varias unidades separadas por virgula). Os rotulos das unidades podem
## vir em qualquer formato ("IDF-13", "GW19-13"): normalizados para o
## rotulo interno "IDF<NN>" (idf_internal_label()). coverage_dt (opcional)
## acrescenta a % geometrica de cada par.
idf_relation_manual <- function(manual_dt, coverage_dt = NULL) {
  m <- data.table::copy(data.table::as.data.table(manual_dt))
  data.table::setnames(m, old = c("Turbine ID", "Primary IDF", "Secondary IDF(s)"),
                       new = c("turbine", "primary", "secondary_raw"), skip_absent = TRUE)
  if (!"secondary_raw" %in% names(m)) m[, secondary_raw := NA_character_]

  rows <- lapply(seq_len(nrow(m)), function(i) {
    sec <- character()
    if (!is.na(m$secondary_raw[i]) && nzchar(m$secondary_raw[i])) sec <- trimws(unlist(strsplit(m$secondary_raw[i], ",")))
    units <- c(m$primary[i], sec)
    units <- units[!is.na(units) & nzchar(units)]
    if (length(units) == 0L) return(NULL)
    data.table::data.table(
      turbine = as.character(m$turbine[i]), idf = idf_internal_label(units),
      role = c("Primary", rep("Secondary", length(units) - 1L)),
      rank = seq_along(units)
    )
  })
  out <- data.table::rbindlist(rows)
  if (nrow(out) == 0L) {
    return(data.table::data.table(turbine = character(), idf = character(), role = character(), rank = integer(),
                                  pct_coverage = numeric(), relation_source = character()))
  }
  out <- unique(out, by = c("turbine", "idf"))
  out[, pct_coverage := NA_real_]
  if (!is.null(coverage_dt) && nrow(coverage_dt) > 0L) {
    cv <- unique(data.table::as.data.table(coverage_dt)[, .(turbine, idf, pct_coverage)], by = c("turbine", "idf"))
    out[cv, on = c("turbine", "idf"), pct_coverage := i.pct_coverage]
  }
  out[, relation_source := "manual"]
  out[]
}

## Resolve a relacao: manual quando existir (por turbina); turbinas sem
## linha na matriz manual ficam com a relacao geometrica.
idf_relation_resolve <- function(coverage_dt, manual_dt = NULL, min_pct_coverage = 20, max_secondary = Inf, turbines = NULL) {
  geo <- idf_relation_geometric(coverage_dt, min_pct_coverage, max_secondary, turbines)
  if (is.null(manual_dt)) return(geo)
  man <- idf_relation_manual(manual_dt, coverage_dt)
  if (!is.null(turbines)) man <- man[turbine %in% turbines]
  out <- data.table::rbindlist(list(man, geo[!turbine %in% man$turbine]), use.names = TRUE)
  data.table::setorder(out, turbine, rank)
  out[]
}

## Acrescenta `unit`: nome completo da unidade (idf_display_map, R/idf_labels.R)
## ou o rotulo interno se nao houver codigo de heartbeat conhecido (essa
## unidade fica "Not evaluated").
idf_relation_add_unit <- function(relation_dt, idf_display_map) {
  out <- data.table::copy(data.table::as.data.table(relation_dt))
  out[, unit := relabel_idf(idf, idf_display_map)]
  out[]
}


## 2. Analise de disponibilidade, todas as unidades de uma vez ----

## relation_dt: com `unit` (idf_relation_add_unit()). heartb_dt$idf = nome
## completo da unidade. track_dt: para atribuir cada curtailment a unidade
## que o disparou (R/offline_curtailment_check.R).
idf_availability_analysis <- function(relation_dt, heartb_dt, curtl_dt, scada_dt, track_dt,
                                      period_from, period_to, lat, lon, tz,
                                      offline_gap_min = 60, online_grace_min = 30, data_range = NULL) {
  units <- unique(relation_dt$unit)
  idf_turbines_dt <- unique(relation_dt[, .(idf = unit, turbine)])

  avail <- summarise_availability_window(
    heartb_dt, units, period_from, period_to, lat, lon, tz,
    offline_gap_min = offline_gap_min, online_grace_min = online_grace_min, data_range = data_range
  )
  evidence <- summarise_offline_evidence_window(
    heartb_dt, units, NA_character_, period_from, period_to,
    curtl_dt, scada_dt, lat, lon, tz,
    offline_gap_min = offline_gap_min, online_grace_min = online_grace_min,
    track_dt = track_dt, idf_turbines_dt = idf_turbines_dt, data_range = data_range
  )

  by_idf <- avail$by_idf
  evaluated <- by_idf[!is.na(offline_mins_total)]
  net_overall <- if (nrow(evaluated) > 0L) {
    summarise_net_availability(summarise_availability_overall(evaluated), evidence$overall)
  } else NULL
  net_by_idf <- if (nrow(evaluated) > 0L) summarise_net_availability_by_idf(evaluated, evidence$by_idf) else NULL

  list(availability = avail, evidence = evidence, net_overall = net_overall, net_by_idf = net_by_idf,
       units = units, idf_turbines_dt = idf_turbines_dt)
}


## 3. Tabelas ----

## Tempo offline por unidade: 1 linha por unidade, com os papeis (turbinas
## para as quais e' primaria/secundaria) e as 3 percentagens do relatorio
## (raw, net/confirmada, sem evidencia). Unidades sem heartbeats ficam
## NA / "Not evaluated" (nunca 0%).
idf_unit_summary <- function(relation_dt, analysis) {
  by_idf <- data.table::copy(analysis$availability$by_idf)
  net <- analysis$net_by_idf
  ev_by_idf <- analysis$evidence$by_idf

  pct <- function(m, d) data.table::fifelse(!is.na(d) & d > 0, round(100 * m / d, 1), NA_real_)

  noev <- ev_by_idf[classification == "No evidence (heartbeat and SCADA both missing)", .(idf, no_evidence_mins = total_mins)]

  out <- by_idf[, .(unit = idf, daylight_mins_total, offline_mins_total, offline_pct = monitoring_period_pct,
                    availability_cat = as.character(availability_cat))]
  if (!is.null(net)) out <- merge(out, net[, .(unit = idf, comm_failure_mins, net_offline_mins)], by = "unit", all.x = TRUE)
  else out[, `:=`(comm_failure_mins = NA_real_, net_offline_mins = NA_real_)]
  out <- merge(out, noev[, .(unit = idf, no_evidence_mins)], by = "unit", all.x = TRUE)
  out[!is.na(offline_mins_total) & is.na(no_evidence_mins), no_evidence_mins := 0]
  out[, `:=`(
    net_offline_pct = pct(net_offline_mins, daylight_mins_total),
    no_evidence_pct = pct(no_evidence_mins, daylight_mins_total)
  )]
  out[is.na(availability_cat), availability_cat := "Not evaluated"]

  roles <- relation_dt[, .(
    primary_for   = paste(sort(unique(turbine[role == "Primary"])), collapse = ", "),
    secondary_for = paste(sort(unique(turbine[role == "Secondary"])), collapse = ", ")
  ), by = .(unit)]
  out <- merge(out, roles, by = "unit", all.x = TRUE)
  data.table::setorder(out, -offline_pct, unit, na.last = TRUE)
  out[]
}

## Alerta de falha persistente: unidades com % offline > threshold_pct do
## tempo diurno do periodo analisado (basis = "raw": offline por heartbeat;
## "net": depois de retirar os gaps com curtailment disparado pela propria
## unidade). Unidades sem nenhum heartbeat no periodo entram sempre, com o
## estado "No heartbeat records" (nao ha' dados para as avaliar -- nao e'
## o mesmo que uma % medida).
idf_alert_table <- function(unit_dt, threshold_pct = 50, basis = c("raw", "net")) {
  basis <- match.arg(basis)
  col <- if (basis == "raw") "offline_pct" else "net_offline_pct"
  dt <- data.table::copy(unit_dt)
  dt[, status := data.table::fcase(
    availability_cat == "Not evaluated", "No heartbeat records in the period",
    !is.na(get(col)) & get(col) > threshold_pct, sprintf("Persistent failure (> %s%% of daylight time)", format(threshold_pct)),
    default = NA_character_
  )]
  out <- dt[!is.na(status), .(unit, primary_for, secondary_for, offline_pct, net_offline_pct, no_evidence_pct, status)]
  data.table::setorder(out, status, -offline_pct, na.last = TRUE)
  out[]
}

## Turbinas SEM nenhuma unidade a funcionar: todas as unidades que a
## protegem (primaria + secundarias) estao em alerta (falha persistente ou
## sem heartbeats).
idf_turbine_alert_table <- function(relation_dt, alert_dt) {
  if (nrow(relation_dt) == 0L) return(data.table::data.table())
  rel <- merge(relation_dt[, .(turbine, unit, role, rank)], alert_dt[, .(unit, alert = TRUE)], by = "unit", all.x = TRUE)
  rel[is.na(alert), alert := FALSE]
  out <- rel[, .(
    n_units = .N, n_units_in_alert = sum(alert),
    units = paste(unit[order(rank)], collapse = ", ")
  ), by = turbine]
  out[n_units_in_alert == n_units][order(turbine)][, .(turbine, units, n_units)]
}

## Relacao turbina <-> unidade com o estado de cada unidade (tabela principal)
idf_relation_table <- function(relation_dt, unit_dt) {
  out <- merge(relation_dt[, .(turbine, role, rank, unit, pct_coverage, relation_source)],
               unit_dt[, .(unit, offline_pct, availability_cat)], by = "unit", all.x = TRUE)
  data.table::setorder(out, turbine, rank)
  out[, .(turbine, role, unit, pct_coverage, relation_source, offline_pct, availability_cat)]
}


## 4. Calendario: varias unidades -> varias figuras (N unidades por figura) ----

split_units <- function(units, per_figure = 6L) {
  units <- unique(units)
  if (length(units) == 0L) return(list())
  split(units, ceiling(seq_along(units) / per_figure))
}

## Devolve uma lista de list(p = ggplot, h = altura em polegadas)
idf_calendar_figures <- function(slot_grid_fun, unit_chunks, slot_mins = 30, date_breaks = "7 days", turbine_label = NULL) {
  lapply(unit_chunks, function(u) {
    grid <- slot_grid_fun(u)
    # titulo com o(s) codigo(s) da(s) turbina(s) analisada(s)
    title <- sprintf("IdentiFlight offline evidence%s (%s to %s)",
                     if (is.null(turbine_label) || !nzchar(turbine_label)) "" else paste0(" - ", turbine_label),
                     format(min(grid$date), "%d %b %Y"), format(max(grid$date), "%d %b %Y"))
    list(p = plot_offline_evidence_slots(grid, slot_mins = slot_mins, date_breaks = date_breaks, title = title),
         h = min(9, 1.9 + 1.25 * length(u)))
  })
}


## 5. Identificadores de turbina (shapefile) -- letras + digitos, zero-pad a
## 2 digitos ("T9" -> "T09"); o que nao bate com o padrao fica como esta.
## Mesma logica de run_incident.R / IDF_analysis.R.
normalise_turbine_ids <- function(raw_id) {
  raw_id <- as.character(raw_id)
  m <- regmatches(raw_id, regexec("^([A-Za-z]+)([0-9]+)$", raw_id))
  vapply(seq_along(raw_id), function(i) {
    g <- m[[i]]
    if (length(g) < 3) return(raw_id[i])
    paste0(g[2], sprintf("%02d", as.integer(g[3])))
  }, character(1))
}


## 6. Leitura LEVE dos tracks, so' para atribuir curtailments a unidades ----
##
## A atribuicao curtailment -> unidade (attribute_curtailment_unit(),
## R/offline_curtailment_check.R) so' precisa de 3 colunas dos TrackReport
## (TrackId, DateTimeStamp, TowerNumber) e so' dos tracks que dispararam
## curtailments -- em vez de ler as 12 colunas de todos os tracks (dezenas
## de milhoes de linhas) e gravar uma cache enorme. track_ids = NULL le tudo.
read_track_units <- function(databases_dirs, pattern, tz, farm_pattern = NULL, track_ids = NULL) {
  files <- list_files_multi_dir(databases_dirs, pattern, farm_pattern)
  if (length(files) == 0L) return(data.table::data.table(track_id = character(), timestamp = as.POSIXct(character()), idf = integer()))
  if (!is.null(track_ids)) track_ids <- unique(as.character(track_ids))

  dt <- data.table::rbindlist(lapply(files, function(f) {
    x <- data.table::fread(f, select = c("TrackId", "DateTimeStamp", "TowerNumber"), na.strings = "NULL", header = TRUE)
    data.table::setnames(x, c("track_id", "timestamp", "idf"))
    x[, track_id := as.character(track_id)]
    if (!is.null(track_ids)) x <- x[track_id %in% track_ids]
    x
  }))
  # fonte ja' em hora LOCAL: force_tz (mesmo criterio de R/read_tracks.R)
  dt[, timestamp := lubridate::force_tz(timestamp, tz)]
  unique(dt)
}
