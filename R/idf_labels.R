##
## Rotulos das unidades IDF nos relatorios
##
## O codigo oficial de uma unidade IDF e' o nome que vem dos heartbeats
## (instance_name), "GW<turbina que aloja a unidade>-<numero>" no
## Zarafshan (ex: "GW32-22"), "BSH55-09" no Bash, "DZH01-01" no Djangeldy --
## e' esse o nome que os clientes pedem nos relatorios. Internamente, o que
## liga as fontes entre si e' o NUMERO da unidade (ultimos digitos: 22),
## igual a track_dt$idf (TowerNumber) e ao rotulo "IDF<NN>" do shapefile
## ("IDF22"). Este ficheiro faz a ponte: tabela numero <-> codigo completo
## <-> rotulo interno, e um relabel() para as tabelas/graficos de saida.
##
## Depende de: data.table (idf_unit_number() vem de R/offline_curtailment_check.R;
## definido aqui so' se ainda nao existir)
##
## Uso:
##   source("R/idf_labels.R")
##   idf_display_map <- build_idf_label_map(heartb_dt_unfilt$idf, heartbeat_idf_units)
##   relabel_idf(c("IDF22", "IDF24"), idf_display_map)  # -> "GW32-22" "GW25-24"
##   idf_label_table(idf_display_map)                    # tabela para xlsx/anexo
##

if (!exists("idf_unit_number", mode = "function")) {
  idf_unit_number <- function(idf_label) as.integer(sub(".*?([0-9]+)$", "\\1", as.character(idf_label)))
}

## Rotulo interno "IDF<NN>" (2 digitos) de um codigo/rotulo qualquer
idf_internal_label <- function(code) sprintf("IDF%02d", idf_unit_number(code))

## Mapa rotulo interno -> codigo completo. codes: codigos brutos dos
## heartbeats (qualquer vetor, com repeticoes). settings_units: vetor
## heartbeat_idf_units dos settings (codigos brutos, com ou sem names
## "IDF<NN>") -- tem PRECEDENCIA sobre o que se deduz dos heartbeats.
## Devolve um vetor nomeado: names = "IDF<NN>", values = codigo completo.
build_idf_label_map <- function(codes, settings_units = NULL) {
  codes <- unique(as.character(codes))
  codes <- codes[!is.na(codes) & nzchar(codes) & !is.na(idf_unit_number(codes))]

  map <- stats::setNames(codes, idf_internal_label(codes))
  dup <- names(map)[duplicated(names(map))]
  if (length(dup) > 0L) {
    message("AVISO: mais de um codigo de heartbeat para o mesmo numero de unidade IDF (",
            paste(unique(dup), collapse = ", "), ") -- fica o primeiro; confirmar nos settings (heartbeat_idf_units).")
    map <- map[!duplicated(names(map))]
  }

  if (!is.null(settings_units) && length(settings_units) > 0L) {
    su <- stats::setNames(as.character(settings_units), idf_internal_label(settings_units))
    map <- c(su, map[!names(map) %in% names(su)])
  }

  map[order(names(map))]
}

## Substitui rotulos internos pelo codigo completo; o que nao esta no mapa
## (ou ja' e' um codigo completo) fica como esta.
relabel_idf <- function(x, map) {
  if (is.null(x)) return(x)
  was_factor <- is.factor(x)
  ch <- as.character(x)
  hit <- match(ch, names(map))
  ch[!is.na(hit)] <- unname(map[hit[!is.na(hit)]])
  if (was_factor) factor(ch, levels = unique(c(unname(map[match(levels(x), names(map))][!is.na(match(levels(x), names(map)))]), ch))) else ch
}

## Aplica relabel_idf() a colunas de um data.frame/data.table (copia)
relabel_idf_cols <- function(df, map, cols = "idf") {
  if (is.null(df)) return(df)
  out <- data.table::copy(data.table::as.data.table(df))
  for (cl in intersect(cols, names(out))) data.table::set(out, j = cl, value = relabel_idf(out[[cl]], map))
  out[]
}

## Tabela de correspondencias (para xlsx de anexo; cabecalhos em ingles)
idf_label_table <- function(map) {
  data.table::data.table(
    unit_number    = idf_unit_number(names(map)),
    internal_label = names(map),
    full_name      = unname(map)
  )
}

## Tabela completa de correspondencias de UM parque: une as 3 fontes por
## numero de unidade -- heartbeats (codigos completos), settings
## (heartbeat_idf_units, precedencia) e shapefile (rotulos como estao no
## ficheiro). Cabecalhos/valores em ingles (vai para xlsx).
##   source: "settings" | "heartbeats" | "shapefile only (no heartbeat code)"
idf_label_map_for_farm <- function(farm_code, heartbeat_codes = character(), settings_units = NULL, shapefile_ids = character()) {
  map <- build_idf_label_map(heartbeat_codes, settings_units)
  shp <- as.character(shapefile_ids)
  shp <- shp[!is.na(shp) & !is.na(idf_unit_number(shp))]
  shp_tbl <- data.table::data.table(unit_number = idf_unit_number(shp), shapefile_label = shp)
  shp_tbl <- shp_tbl[!duplicated(unit_number)]

  full <- data.table::data.table(unit_number = idf_unit_number(names(map)), full_name = unname(map))
  settings_numbers <- if (is.null(settings_units)) integer() else idf_unit_number(settings_units)

  all_numbers <- sort(unique(c(full$unit_number, shp_tbl$unit_number)))
  out <- data.table::data.table(project = farm_code, unit_number = all_numbers)
  out <- merge(out, full, by = "unit_number", all.x = TRUE)
  out <- merge(out, shp_tbl, by = "unit_number", all.x = TRUE)
  out[, internal_label := sprintf("IDF%02d", unit_number)]
  out[, source := data.table::fcase(
    unit_number %in% settings_numbers, "settings",
    !is.na(full_name),                  "heartbeats",
    default = "shapefile only (no heartbeat code)"
  )]
  data.table::setcolorder(out, c("project", "unit_number", "internal_label", "full_name", "shapefile_label", "source"))
  data.table::setorder(out, project, unit_number)
  out[]
}
