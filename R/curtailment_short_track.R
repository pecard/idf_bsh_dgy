##
## Curtailments de short-tracks (poucos pontos, perto da turbina) ----
##
## Migrado de scripts_IDF/curtailments_short-track.R.
##
## Um "short track" e' um track com poucos pontos registados
## (< shorttrack_min_points) -- pouca informacao acumulada para o algoritmo
## confirmar especie/trajetoria antes de decidir um curtailment. Interessa
## sobretudo quando isso acontece A CURTA DISTANCIA da turbina
## (< shorttrack_eval_range): e' onde a limitacao de informacao pode ter
## mais peso na decisao.
##
## Correcoes face ao script original (ver PROJECT_CHECKPOINT/CLAUDE.md --
## mesma familia de bugs recorrentes ja documentados no projeto):
##   - O script original usava curtl_dt$x2d, coluna que ja nao existe no
##     curtl_dt atual (foi substituida por track_dt$dist/dist3d na
##     refatoracao do pipeline). Aqui recalcula-se x2d = distancia 2D no 1º
##     registo do track, o mesmo conceito ja usado em
##     R/curtailment_safe_distance.R (compute_safe_distance()), mas SEM
##     depender de SCADA -- so' precisa de track_dt + curtl_dt, corre mesmo
##     para turbinas fora de turbinas_scada.
##   - O script original usava track_dt$count (nº de pontos do track) tal
##     como gravado na leitura ORIGINAL nao filtrada (R/read_tracks.R
##     computa count = .N por track_id sobre track_dt_unfilt; o filtro de
##     datas em "0. Filter data" do IDF_analysis.R subset as LINHAS de
##     track_dt mas nao recalcula count) -- para um track que atravessa a
##     fronteira do periodo do relatorio, esse count fica inflacionado
##     (inclui pontos fora da janela em analise). Aqui recalcula-se sempre
##     n_points a partir do track_dt (ja filtrado) que e' passado a funcao.
##   - O join original (many-to-many por track_id, entre curtl_dt e uma
##     versao "unica por especie" de track_dt) duplicava linhas para tracks
##     multi-ID sem necessidade -- x2d/n_points sao propriedades do track
##     inteiro, nao da especie, por isso aqui o join e' 1:1 por track_id.
##   - O join usa update-join (out[track_meta, on=, `:=`(...)]) em vez de
##     merge(): se curtl_dt alguma vez tiver (por exemplo, cache .fst antiga
##     de uma versao anterior do pipeline) uma coluna n_points/x2d propria,
##     merge() renomeia SILENCIOSAMENTE para "x2d.x"/"x2d.y" em vez de dar
##     erro, e o codigo abaixo que espera a coluna "x2d" falha com "object
##     'x2d' not found" -- update-join sobrescreve a coluna existente em vez
##     de a duplicar com sufixo, portanto nao tem este modo de falha.
##
## Depende de: data.table
##
## Uso:
##   source("R/curtailment_short_track.R")
##
##   short_track_dt <- classify_short_track_curtailments(
##     track_dt, curtl_dt, min_points = shorttrack_min_points, eval_range_m = shorttrack_eval_range
##   )
##   short_track_summary <- summarise_short_track_curtailments(track_dt, short_track_dt, shorttrack_min_points)
##   short_track_by_species <- summarise_short_track_by_species(short_track_dt, prioritysp)
##

classify_short_track_curtailments <- function(track_dt, curtl_dt, min_points, eval_range_m) {

  track_meta <- track_dt[, .(n_points = .N, x2d = dist[1]), by = track_id]
  track_meta[, track_id_chr := as.character(track_id)]

  out <- data.table::copy(curtl_dt)
  out[, track_id_chr := as.character(track_id)]
  ## update-join: sobrescreve n_points/x2d se ja existirem em out, em vez de
  ## os duplicar com sufixo ".x"/".y" (ver nota no cabecalho do ficheiro)
  out[track_meta, on = "track_id_chr", `:=`(n_points = i.n_points, x2d = i.x2d)]
  out[, track_id_chr := NULL]

  stopifnot(all(c("n_points", "x2d") %in% names(out)))

  out[, is_short_track     := !is.na(n_points) & n_points < min_points]
  out[, is_close            := !is.na(x2d) & x2d < eval_range_m]
  out[, short_track_close  := is_short_track & is_close]

  out[]
}


## Sumario geral: total de tracks curtos farm-wide (independentemente de
## terem disparado curtailment), quantos curtailments foram de short-tracks
## perto da turbina, e que fraccao isso representa do total de curtailments
summarise_short_track_curtailments <- function(track_dt, short_track_dt, min_points) {

  n_points_by_track <- track_dt[, .(n_points = .N), by = track_id]
  total_short_tracks <- n_points_by_track[, sum(n_points < min_points)]

  n_total_curt  <- nrow(short_track_dt)
  n_short_close <- if (n_total_curt > 0L) short_track_dt[, sum(short_track_close, na.rm = TRUE)] else 0L

  data.table::data.table(
    metric = c(
      "Total short tracks (farm-wide, < min_points)",
      "Short-track curtailments (close range)",
      "Short-track curtailments (% of total curtailments)"
    ),
    value = c(
      total_short_tracks,
      n_short_close,
      if (n_total_curt > 0L) round(100 * n_short_close / n_total_curt, 2) else NA_real_
    )
  )
}


## Curtailments de short-tracks perto da turbina, so' para especies
## prioritarias -- contagem + frequencia relativa (dentro deste
## subconjunto), para se ver que especies prioritarias estao mais expostas
## a esta limitacao
summarise_short_track_by_species <- function(short_track_dt, prioritysp) {

  dt <- short_track_dt[short_track_close == TRUE & species %in% prioritysp]

  if (nrow(dt) == 0L) {
    return(data.table::data.table(species = character(), n = integer(), pct = numeric()))
  }

  out <- dt[, .(n = .N), by = species]
  out[, pct := round(100 * n / sum(n), 1)]
  data.table::setorder(out, -n)
  out[]
}
