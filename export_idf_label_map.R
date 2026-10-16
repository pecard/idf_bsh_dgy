##
## Exporta a lista COMPLETA de correspondencias de unidades IDF por projeto:
## numero da unidade <-> rotulo interno ("IDF<NN>") <-> nome completo do
## heartbeat (ex: "GW32-22", "BSH55-09", "DZH01-01") <-> rotulo no shapefile.
## O nome completo e' o usado nos relatorios (R/idf_labels.R).
##
## Le: settings de cada parque (heartbeat_idf_units, idf_filename), a CACHE
## de heartbeats ja' construida por cada projeto (cache/<farm_code>/heartb_dt_unfilt.fst
## -- corre primeiro o relatorio desse parque se a cache nao existir) e o
## shapefile das unidades IDF.
##
## Uso: dar Source (Ctrl+Shift+S) na raiz do projeto. Gera
## outputs/idf_label_map_<AAAAMMDD>.xlsx (em ingles).
##

library(data.table); library(sf); library(fst); library(writexl)
source("R/idf_labels.R")
folder_input <- "inputs"

## 1 settings por parque (o do incidente serve de settings do ZRF)
settings_by_farm <- c(
  ZRF = "userSettings_ZRF_T94_20261001.R",
  BSH = "userSettings_BSH.R",
  DGY = "userSettings_DGY.R"
)

tables <- lapply(names(settings_by_farm), function(farm) {
  env <- new.env()
  ok <- tryCatch({ sys.source(file.path(folder_input, settings_by_farm[[farm]]), envir = env); TRUE },
                 error = function(e) { message(farm, ": erro a ler settings -- ", conditionMessage(e)); FALSE })
  if (!ok) return(NULL)

  code <- if (exists("farm_code", envir = env)) get("farm_code", envir = env) else farm
  su <- if (exists("heartbeat_idf_units", envir = env)) get("heartbeat_idf_units", envir = env) else NULL

  cache_file <- file.path("cache", code, "heartb_dt_unfilt.fst")
  codes <- if (file.exists(cache_file)) {
    fst::read_fst(cache_file, columns = "idf", as.data.table = TRUE)$idf
  } else {
    message(farm, ": cache de heartbeats nao encontrada (", cache_file, ") -- so' settings/shapefile."); character()
  }

  shp_ids <- character()
  if (exists("idf_filename", envir = env)) {
    shp_file <- file.path(folder_input, get("idf_filename", envir = env))
    if (file.exists(shp_file)) {
      shp <- sf::read_sf(shp_file)
      col <- if (exists("idf_source_id_col", envir = env) && get("idf_source_id_col", envir = env) %in% names(shp)) get("idf_source_id_col", envir = env) else intersect(c("imaging_he", "Name"), names(shp))[1]
      if (!is.na(col)) shp_ids <- as.character(shp[[col]])
    }
  }

  idf_label_map_for_farm(code, codes, su, shp_ids)
})
map_all <- rbindlist(tables)

summary_dt <- map_all[, .(
  units_total = .N,
  with_full_name = sum(!is.na(full_name)),
  shapefile_only = sum(source == "shapefile only (no heartbeat code)")
), by = project]

dir.create("outputs", showWarnings = FALSE)
out_path <- file.path("outputs", paste0("idf_label_map_", format(Sys.Date(), "%Y%m%d"), ".xlsx"))
writexl::write_xlsx(list(Mapping = map_all, Summary = summary_dt), out_path)
message("Gravado: ", out_path)
print(summary_dt)
