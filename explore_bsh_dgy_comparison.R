##
## Comparacao BSH vs DGY -- nota de recomendacao para repor a operacao normal
## (reduzir/remover curtailment protetivo) em DGY
##
## Pedido do Paulo (2026-09): DGY so' tem 4 unidades IDF, cobrindo um
## subconjunto pequeno das suas 79 turbinas -- o historico de monitorizacao
## proprio do DGY e' demasiado curto/estreito para sustentar sozinho um
## argumento de reposicao de operacao normal perante os financiadores. A
## discussao e' a mesma do BSH (parque a ~50km, mesma comunidade de aves,
## mesmas especies prioritarias -- ver prioritysp em userSettings_BSH.R vs
## userSettings_DGY.R, praticamente identicas), mas os argumentos para DGY
## tem de ser INDIRETOS -- por analogia/comparacao com o BSH, onde ha'
## historico extenso (deteções, terreno/DEM, curtailments, resposta).
##
## Esta nota usa 2 linhas de evidencia (uma 3a, o raptor survey de nidificacao
## do DGY, fica para depois -- Paulo, 2026-09: "descrevo mais tarde este
## relatório... Esse topico fica para o final", ver placeholder na secção
## "Raptor Breeding Survey" do template):
##
##   1. Tracks -- presenca/ausencia (0/1) semanal de cada especie prioritaria,
##      BSH vs DGY, na MESMA janela de 12 meses calendario (bins de 7 dias
##      ancorados a' MESMA data de inicio nos 2 parques, para o eixo semanal
##      ficar alinhado sazonalmente) -- mostra se o padrao de uso sazonal e'
##      semelhante nos 2 parques mesmo com dados limitados do DGY.
##   2. Topografia -- classificacao de terreno (ridge/complex/flat, a partir
##      do DEM) e padrao semanal de uso do espaco por classe de terreno,
##      calculados PARA OS 2 PARQUES (o DGY TEM DEM proprio -- Paulo, 2026-09:
##      "DGY tem DEM proprio: G:/.../data-raw/DGY_dem_copernicus30m.tif" --
##      dem_filename ja' definido em userSettings_DGY.R, so' faltava o
##      ficheiro em databases_dir, que so' existe na maquina do Paulo, nao
##      neste sandbox) -- reutiliza directamente R/turbine_terrain_classification.R
##      e R/track_terrain_temporal.R, ja' usados na secção 11 do relatorio anual.
##
## NAO faz parte do pipeline automatico dos relatorios (nunca chamado a
## partir de IDF_analysis.R/IDF_monthly_report.R) -- e' um script AUTONOMO,
## mesma logica de "le e faz cache dos seus proprios dados de raiz" de
## run_incident_zrshan.R (fonte de 2 settings files, um por parque, na MESMA
## sessao -- por isso os objetos de cada parque sao guardados em listas
## bsh/dgy, nao deixados como track_dt/wtg/etc. globais, que a 2a chamada de
## import_farm_data() sobrescreveria).
##
## Sem R disponivel no sandbox onde este script foi escrito -- nao foi
## corrido nem testado aqui. Correr no RStudio do Paulo (mesma maquina onde
## IDF_analysis.R ja' correu para os 2 parques -- reutiliza a cache/<farm>/
## existente, nao relê os brutos a nao ser que forces).
##
## Uso: editar force_reread_cache abaixo se necessario, e dar Source a este
## ficheiro (source("explore_bsh_dgy_comparison.R")).
##


##
## PACKAGES ----
##

packages <- c(
  'data.table', 'sf', 'terra', 'RANN', 'ggplot2', 'lubridate',
  'janitor', 'rstudioapi', 'writexl', 'readxl', 'rmarkdown', 'flextable', 'svDialogs'
)
for (p in packages) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

tryCatch({
  active_doc_path <- rstudioapi::getActiveDocumentContext()$path
  if (isTRUE(nzchar(active_doc_path))) setwd(dirname(active_doc_path))
}, error = function(e) {
  message(sprintf(
    "Aviso: nao foi possivel mudar para a pasta do script via rstudioapi (%s) -- a usar a working directory atual: %s",
    conditionMessage(e), getwd()
  ))
})

folder_input <- "inputs"

source("scripts/check_username.R")
source("R/write_utils.R")
source("R/read_utils.R")
source("R/read_tracks.R")
source("R/data_cache.R")
source("R/turbine_terrain_classification.R")
source("R/track_terrain_temporal.R")
source("R/report.R")

folder_output <- file.path("outputs", "comparison", format(Sys.time(), "%Y%m%d"))
dir.create(folder_output, showWarnings = FALSE, recursive = TRUE)

if (!exists("force_reread_cache")) force_reread_cache <- FALSE
if (!exists("generate_report")) generate_report <- TRUE

username <- check_username()
code_version <- {
  run_git <- function(args) tryCatch(system2("git", args, stdout = TRUE, stderr = FALSE), error = function(e) character(0))
  hash <- run_git(c("log", "-1", "--format=%h"))
  if (length(hash) == 0L || !nzchar(hash[1])) {
    "unknown (not a git checkout, or git unavailable)"
  } else {
    commit_date <- run_git(c("log", "-1", "--format=%ci"))
    dirty <- length(run_git(c("status", "--porcelain"))) > 0L
    out <- if (length(commit_date) > 0L && nzchar(commit_date[1])) sprintf("%s (%s)", hash[1], commit_date[1]) else hash[1]
    if (dirty) paste0(out, " + uncommitted local changes") else out
  }
}
analysis_date <- format(Sys.time(), "%Y-%m-%d")


##
## 1. Import por parque (le/faz cache de tracks + turbinas + terreno) ----
##
## Cada settings file, ao ser sourced, define ini/end/prioritysp/wtg_filename/
## dem_filename/databases_dir/crs_projection_plannar/farm_code/etc. como
## globais (mesma convencao de IDF_analysis.R/run_incident_zrshan.R) -- por
## isso esta funcao tem de EXTRAIR tudo o que precisa desses globais e
## guarda-los no objeto devolvido ANTES de a proxima chamada (outro parque)
## sourcing outro settings file os sobrescrever.
##
## dem_file so' e' usado SE o ficheiro existir -- no DGY, dem_filename ja'
## esta' definido em userSettings_DGY.R ("DGY_dem_copernicus30m.tif"), o
## Paulo confirmou (2026-09) que o ficheiro real esta' em databases_dir na
## sua maquina (G:/O meu disco/Programacao/r/Bsh_Dgy_WPP/data-raw) -- nao
## precisa de mais nenhuma configuracao aqui, so' de estar la' no disco.

import_farm_data <- function(settings_file, farm_label) {

  message(sprintf("\n===== A importar dados: %s (%s) =====", farm_label, settings_file))

  ## Limpa variaveis opcionais de settings anteriores (so' alguns parques as
  ## definem -- ex: idf_source_id_col/idf_installed_units so' no DGY) para
  ## nao herdar valores do outro parque ao correr o script 2x na mesma sessao.
  rm(list = intersect(c("idf_source_id_col", "idf_installed_units", "wtg_source_id_col", "farm_pattern"), ls(globalenv())), envir = globalenv())
  source(file.path(folder_input, settings_file))

  databases_dirs <- unique(c(databases_dir, if (exists("databases_dir_alt")) databases_dir_alt))
  folder_cache <- file.path("cache", farm_code)

  ## current = NULL sempre (NAO "if (exists(...)) ... else NULL" como em
  ## IDF_analysis.R) -- este script corre os 2 parques na MESMA sessao R, e
  ## exists("track_dt_unfilt") encontraria/reutilizaria um objeto global
  ## deixado por uma corrida ANTERIOR (o BSH, ao processar o DGY a seguir,
  ## ou um IDF_analysis.R corrido antes na mesma sessao) -- reuse_or_load_cache()
  ## ficaria a devolver os dados do parque ERRADO. Forcar NULL aqui vai
  ## sempre a' cache em disco farm-specific (folder_cache, definido acima a
  ## partir de farm_code), nunca a um objeto em memoria de outro parque.
  ## So' tracks -- as 2 linhas de evidencia desta nota (presenca semanal por
  ## especie, e uso do espaco por classe de terreno) usam so' track_dt;
  ## curtailments nao entram em nenhuma das duas, por isso nao sao lidos
  ## aqui (evita 1 reuse_or_load_cache()/leitura desnecessaria por parque).
  track_dt_unfilt <- reuse_or_load_cache(
    NULL,
    "track_dt_unfilt", file.path(folder_cache, "track_dt_unfilt.fst"),
    function() read_tracks_data(databases_dirs, trackreport_pattern, tz = proj_timezone, farm_pattern = if (exists("farm_pattern")) farm_pattern else NULL),
    force_reread = force_reread_cache, tz = proj_timezone
  )

  track_dt <- as.data.table(track_dt_unfilt)[timestamp >= ini & timestamp <= end]

  ## Mesma normalizacao de InternalNa (2 digitos, ex: "DZH01") de
  ## IDF_analysis.R, secção "0. Import data" -- necessaria para track_dt$turbine
  ## e wtg$InternalNa/turbine_terrain_dt$wtg_id ficarem comparaveis.
  wtg <- sf::read_sf(file.path(folder_input, wtg_filename)) |> sf::st_transform(crs_projection_plannar)
  wtg_source_id_col_local <- if (exists("wtg_source_id_col")) wtg_source_id_col else "InternalNa"
  wtg$InternalNa <- {
    raw_id <- wtg[[wtg_source_id_col_local]]
    m <- regmatches(raw_id, regexec("^([A-Za-z]+)([0-9]+)$", raw_id))
    vapply(seq_along(raw_id), function(i) {
      g <- m[[i]]
      if (length(g) < 3) return(raw_id[i])
      paste0(g[2], sprintf("%02d", as.integer(g[3])))
    }, character(1))
  }

  dem_file <- file.path(databases_dir, dem_filename)
  terrain_dt <- NULL
  track_terrain_dt <- NULL
  weekly_terrain_dt <- NULL
  if (file.exists(dem_file)) {
    terrain_dt <- compute_turbine_terrain_metrics(wtg, dem_file, radius_inner_m = 250, radius_outer_m = 500)
    terrain_dt <- classify_terrain(terrain_dt)
    track_terrain_dt <- assign_track_terrain_class(track_dt, wtg, terrain_dt)
    weekly_terrain_dt <- summarise_tracks_by_week_terrain(track_terrain_dt, terrain_dt)
  } else {
    message(sprintf(
      "import_farm_data(%s): dem_file nao encontrado (%s) -- classificacao de terreno saltada para este parque.",
      farm_label, dem_file
    ))
  }

  ## n_idf_total = unidades IDF do shapefile do parque (idf_filename), pela
  ## mesma convencao de IDF_analysis.R: coluna idf_source_id_col (por omissao
  ## "imaging_he") e, se definido, filtrado por idf_installed_units (DGY: o
  ## shapefile tem 2 registos extra "DZH-23" de cobertura planeada, nao
  ## instalados). NAO usar heartbeat_idf_units -- no BSH so' lista 8 unidades
  ## (proposito mais estreito: fallback de heartbeats), enquanto o BSH tem 43.
  idf_shp <- sf::read_sf(file.path(folder_input, idf_filename))
  idf_ids <- idf_shp[[if (exists("idf_source_id_col")) idf_source_id_col else "imaging_he"]]
  if (exists("idf_installed_units")) idf_ids <- idf_ids[idf_ids %in% idf_installed_units]
  n_idf_total <- length(unique(idf_ids))
  message(sprintf("%s: %d unidades IDF no shapefile; %d com tracks na janela.", farm_label, n_idf_total, data.table::uniqueN(track_dt$idf)))

  list(
    farm_label = farm_label, farm_code = farm_code,
    ini = ini, end = end, tz = proj_timezone,
    prioritysp = prioritysp,
    track_dt = track_dt, wtg = wtg,
    n_turbines_total = nrow(wtg), n_idf_total = n_idf_total,
    dem_file = dem_file, terrain_dt = terrain_dt,
    track_terrain_dt = track_terrain_dt, weekly_terrain_dt = weekly_terrain_dt
  )
}

bsh <- import_farm_data("userSettings_BSH.R", "BSH")
dgy <- import_farm_data("userSettings_DGY.R", "DGY")


##
## 2. Presenca/ausencia semanal (0/1) por especie prioritaria, BSH vs DGY ----
##
## Janela de comparacao = interseccao dos 2 periodos disponiveis (limitada
## pelo DGY, o mais curto -- ver header). Bins de 7 dias ancorados a'
## MESMA data (compare_start) nos 2 parques, para a semana N significar as
## MESMAS datas de calendario em ambos -- ao contrario de
## summarise_tracks_by_week_terrain() (R/track_terrain_temporal.R), que
## ancora cada parque ao seu PROPRIO 1o dia de dados (correto para uma
## analise DENTRO de 1 parque, mas alinharia mal 2 parques com datas de
## inicio diferentes -- Julho do BSH cairia contra Agosto do DGY, por ex).

compare_start <- max(bsh$ini, dgy$ini)
compare_end   <- min(bsh$end, dgy$end)
message(sprintf(
  "\nJanela de comparacao (limitada pelo periodo mais curto -- DGY): %s a %s (%.1f semanas)",
  format(compare_start, "%Y-%m-%d"), format(compare_end, "%Y-%m-%d"),
  as.numeric(difftime(compare_end, compare_start, units = "weeks"))
))

## Especies selecionadas para a comparacao (Paulo, 2026-11): so' estas 7,
## na ordem indicada (e' a ordem de apresentacao nos graficos/tabela).
priority_species_compare <- c(
  'White-Tailed-Eagle', 'Steppe-Eagle', 'Golden-Eagle', 'Egyptian-Vulture',
  'Cinereous-Vulture', 'Bearded-Vulture', 'Saker-Falcon'
)
species_label <- function(x) gsub("-", " ", x)

## tz TEM de ser passado explicitamente e usado em TODO as.Date(POSIXct)
## abaixo -- as.Date() sem tz= assume UTC, o que desloca a data 1 dia para
## tras em Asia/Samarkand (UTC+5); este exato bug ja' apareceu 2x antes
## neste projeto (report_start em build_daylight_calendar(),
## R/availability_daylight.R, e a logica de recorte dia/noite) -- ver
## comentario no topo dessa funcao.
weekly_species_presence <- function(track_dt, species_list, compare_start, compare_end, farm_label, tz) {

  compare_start_date <- as.Date(compare_start, tz = tz)
  week_start_dates <- seq(compare_start_date, as.Date(compare_end, tz = tz), by = 7)

  grid <- data.table::CJ(species = species_list, week_start = week_start_dates)

  dt <- track_dt[spec %in% species_list & timestamp >= compare_start & timestamp <= compare_end, .(spec, timestamp)]
  if (nrow(dt) > 0) {
    dt[, week_start := compare_start_date + (as.integer(as.Date(timestamp, tz = tz) - compare_start_date) %/% 7L) * 7L]
    present_dt <- unique(dt[, .(species = spec, week_start)])
    present_dt[, present := 1L]
  } else {
    present_dt <- data.table::data.table(species = character(), week_start = as.Date(character()), present = integer())
  }

  out <- merge(grid, present_dt, by = c("species", "week_start"), all.x = TRUE)
  out[is.na(present), present := 0L]
  out[, farm := farm_label]
  out[, present_lbl := factor(ifelse(present == 1L, "Present", "Absent"), levels = c("Absent", "Present"))]
  data.table::setorder(out, species, week_start)
  out[]
}

bsh_weekly_species <- weekly_species_presence(bsh$track_dt, priority_species_compare, compare_start, compare_end, "BSH", tz = bsh$tz)
dgy_weekly_species <- weekly_species_presence(dgy$track_dt, priority_species_compare, compare_start, compare_end, "DGY", tz = dgy$tz)
weekly_species_both <- data.table::rbindlist(list(bsh_weekly_species, dgy_weekly_species))

## Resumo por especie: % de semanas com presenca em cada parque, na mesma
## janela -- tabela central do argumento (padrao sazonal semelhante mesmo
## com dados limitados do DGY).
summarise_species_presence_comparison <- function(weekly_species_both) {
  out <- weekly_species_both[, .(n_weeks_present = sum(present), n_weeks_total = .N), by = .(species, farm)]
  out[, pct_weeks_present := round(100 * n_weeks_present / n_weeks_total, 1)]
  wide <- data.table::dcast(out, species ~ farm, value.var = c("n_weeks_present", "n_weeks_total", "pct_weeks_present"))
  wide <- wide[match(priority_species_compare, species)][!is.na(species)]
  wide[, species := species_label(species)]
  wide[]
}

species_presence_summary <- summarise_species_presence_comparison(weekly_species_both)

## Grafico: 1 painel por especie, com 2 linhas (BSH em cima, DGY em baixo),
## 1 tile por semana. Presente = cor do parque (2 cores distintas); Ausente =
## cinzento claro. Serie anual alinhada (mesmo eixo x nos 2 parques).
farm_colours <- c(BSH = "#2a78d6", DGY = "#e8830c")

plot_species_presence_paired <- function(weekly_species_both, date_breaks = "4 weeks") {
  dt <- data.table::copy(weekly_species_both)[species %in% priority_species_compare]
  dt[, species_f := factor(species_label(species), levels = species_label(priority_species_compare))]
  dt[, farm_f := factor(farm, levels = c("DGY", "BSH"))]  # DGY primeiro => BSH fica por cima
  dt[, fill_key := factor(ifelse(present == 1L, farm, "Absent"), levels = c("BSH", "DGY", "Absent"))]
  ggplot(dt, aes(x = week_start, y = farm_f, fill = fill_key)) +
    geom_tile(colour = "white", linewidth = 0.3) +
    facet_grid(species_f ~ ., switch = "y") +
    scale_fill_manual(name = NULL, values = c(farm_colours, Absent = "#e8e8e8"),
                      breaks = c("BSH", "DGY", "Absent"),
                      labels = c("BSH present", "DGY present", "Absent"), drop = FALSE) +
    scale_x_date(date_breaks = date_breaks, date_labels = "%d %b %Y", expand = c(0, 0)) +
    labs(x = "Week starting", y = NULL, title = "Weekly presence of priority species") +
    theme_minimal(base_size = 9) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6.5),
      axis.text.y = element_text(size = 7),
      strip.text.y.left = element_text(angle = 0, hjust = 1, size = 7.5, face = "bold"),
      strip.placement = "outside",
      legend.position = "bottom",
      panel.grid = element_blank(),
      panel.spacing.y = unit(4, "pt")
    )
}

p_species_presence <- plot_species_presence_paired(weekly_species_both)

## Intensidade: media de tracks/dia por IDF, Egyptian Vulture e Steppe Eagle.
## Por semana (mesmos bins de 7 dias de cima): n tracks distintos (track_id)
## / n dias da semana dentro da janela / n unidades IDF do parque -- a divisao
## pelo n de IDF controla a diferenca de esforco de deteccao (DGY 4 vs BSH).
## n_idf_total vem do shapefile de IDF de cada parque (ver import_farm_data()).
intensity_species <- c('Egyptian-Vulture', 'Steppe-Eagle')

weekly_species_intensity <- function(track_dt, species_list, compare_start, compare_end, farm_label, tz, n_idf) {
  compare_start_date <- as.Date(compare_start, tz = tz)
  compare_end_date <- as.Date(compare_end, tz = tz)
  week_start_dates <- seq(compare_start_date, compare_end_date, by = 7)
  grid <- data.table::CJ(species = species_list, week_start = week_start_dates)
  grid[, n_days := pmin(7L, as.integer(compare_end_date - week_start) + 1L)]

  dt <- track_dt[spec %in% species_list & timestamp >= compare_start & timestamp <= compare_end, .(spec, track_id, timestamp)]
  if (nrow(dt) > 0) {
    dt[, week_start := compare_start_date + (as.integer(as.Date(timestamp, tz = tz) - compare_start_date) %/% 7L) * 7L]
    cnt <- dt[, .(n_tracks = data.table::uniqueN(track_id)), by = .(species = spec, week_start)]
  } else {
    cnt <- data.table::data.table(species = character(), week_start = as.Date(character()), n_tracks = integer())
  }
  out <- merge(grid, cnt, by = c("species", "week_start"), all.x = TRUE)
  out[is.na(n_tracks), n_tracks := 0L]
  out[, tracks_per_day := n_tracks / n_days]
  out[, tracks_per_day_per_idf := tracks_per_day / n_idf]
  out[, farm := farm_label]
  out[, n_idf_ref := n_idf]
  data.table::setorder(out, species, week_start)
  out[]
}

bsh_weekly_intensity <- weekly_species_intensity(bsh$track_dt, intensity_species, compare_start, compare_end, "BSH", bsh$tz, bsh$n_idf_total)
dgy_weekly_intensity <- weekly_species_intensity(dgy$track_dt, intensity_species, compare_start, compare_end, "DGY", dgy$tz, dgy$n_idf_total)
weekly_intensity_both <- data.table::rbindlist(list(bsh_weekly_intensity, dgy_weekly_intensity))

## Mesma organizacao do grafico de presenca: 1 painel por especie, BSH e DGY
## (2 cores) uma sobre a outra, eixo x semanal comum.
plot_species_intensity_paired <- function(weekly_intensity_both, date_breaks = "4 weeks") {
  dt <- data.table::copy(weekly_intensity_both)
  dt[, species_f := factor(species_label(species), levels = species_label(intensity_species))]
  dt[, farm := factor(farm, levels = c("BSH", "DGY"))]
  ggplot(dt, aes(x = week_start, y = tracks_per_day_per_idf, fill = farm)) +
    geom_col(width = 6, position = "identity") +
    facet_grid(species_f + farm ~ ., scales = "free_y", switch = "y") +
    scale_fill_manual(name = NULL, values = farm_colours) +
    scale_x_date(date_breaks = date_breaks, date_labels = "%d %b %Y", expand = c(0, 0)) +
    labs(x = "Week starting", y = "Mean tracks per day per IDF unit",
         title = "Weekly track intensity, normalised by number of IDF units") +
    theme_minimal(base_size = 9) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6.5),
      axis.text.y = element_text(size = 6.5),
      strip.text.y.left = element_text(angle = 0, hjust = 1, size = 7.5),
      strip.placement = "outside",
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}

p_species_intensity <- plot_species_intensity_paired(weekly_intensity_both)

## Resumo por especie/parque (media sobre a janela) para a tabela do relatorio.
species_intensity_summary <- weekly_intensity_both[, .(
  n_tracks = sum(n_tracks),
  mean_tracks_per_day = round(sum(n_tracks) / sum(n_days), 3),
  mean_tracks_per_day_per_idf = round(sum(n_tracks) / sum(n_days) / unique(n_idf_ref), 3)
), by = .(species, farm)]
species_intensity_summary[, species := species_label(species)]


##
## 3. Topografia -- classificacao de terreno + uso semanal por classe ----
##
## Reutiliza directamente R/turbine_terrain_classification.R e
## R/track_terrain_temporal.R (secção 11 do relatorio anual) -- ja' calculado
## para os 2 parques dentro de import_farm_data() acima (bsh$terrain_dt,
## dgy$terrain_dt), SE o respetivo dem_file existir.

terrain_class_summary_bsh <- if (!is.null(bsh$terrain_dt)) summarise_terrain_class_counts(bsh$terrain_dt) else NULL
terrain_class_summary_dgy <- if (!is.null(dgy$terrain_dt)) summarise_terrain_class_counts(dgy$terrain_dt) else NULL

terrain_class_comparison <- if (!is.null(terrain_class_summary_bsh) && !is.null(terrain_class_summary_dgy)) {
  a <- data.table::copy(terrain_class_summary_bsh)[, farm := "BSH"]
  b <- data.table::copy(terrain_class_summary_dgy)[, farm := "DGY"]
  wide <- data.table::dcast(data.table::rbindlist(list(a, b)), terrain_class ~ farm, value.var = c("n_turbines", "pct_turbines"))
  wide[]
} else NULL

p_terrain_map_bsh <- if (!is.null(bsh$terrain_dt)) plot_turbine_terrain_map(bsh$wtg, bsh$terrain_dt, show_labels = FALSE) else NULL
p_terrain_map_dgy <- if (!is.null(dgy$terrain_dt)) plot_turbine_terrain_map(dgy$wtg, dgy$terrain_dt, show_labels = FALSE) else NULL

p_terrain_weekly_bsh <- if (!is.null(bsh$weekly_terrain_dt)) plot_tracks_by_week_terrain(bsh$weekly_terrain_dt, metric = "pct_turbines_active") else NULL
p_terrain_weekly_dgy <- if (!is.null(dgy$weekly_terrain_dt)) plot_tracks_by_week_terrain(dgy$weekly_terrain_dt, metric = "pct_turbines_active") else NULL


##
## 3b. Raptor breeding survey 2026 (BSH + DGY) ----
##
## Dados extraidos do relatorio "Breeding Raptor Nest Monitoring 2026" por
## scripts/extract_breeding_survey.py para inputs/breeding_survey_2026.xlsx
## (Tabelas 8-16 do relatorio). Inclui TODAS as especies registadas -- ninhos
## com especie atribuida (activos ou nao) + ninhos de peneireiro em pilares
## OHTL. So' lido se o ficheiro existir.

breeding_file <- file.path(folder_input, "breeding_survey_2026.xlsx")
breeding_species_summary <- NULL
breeding_turbine_distances <- NULL
breeding_sheets <- list()

if (file.exists(breeding_file)) {
  rd <- function(sh) data.table::as.data.table(readxl::read_xlsx(breeding_file, sheet = sh))
  b_inv  <- rd("Nest_inventory")
  b_succ <- rd("Breeding_success")
  b_pyl  <- rd("Pylon_kestrel_nests")
  b_tur  <- rd("Nests_vs_turbines")
  b_prev <- rd("Previous_year_comparison")

  outcome_class <- function(x) data.table::fifelse(
    grepl("^Successful", x), "Successful",
    data.table::fifelse(x == "Failed", "Failed",
      data.table::fifelse(x == "Not started", "Not started", "Unknown")))
  b_succ[, outcome := outcome_class(breeding_success)]
  b_pyl[,  outcome := outcome_class(breeding_success)]

  ## Ninhos com especie atribuida (inventario), por parque
  sp_inv <- b_inv[!is.na(species), .(
    nests_recorded = .N, nests_active = sum(active_nest == "Yes")
  ), by = .(farm, species)]
  sp_out <- b_succ[, .(
    successful = sum(outcome == "Successful"), failed = sum(outcome == "Failed"),
    not_started = sum(outcome == "Not started"), unknown = sum(outcome == "Unknown")
  ), by = .(farm, species)]
  ## Distancia minima ao aerogerador mais proximo (Tabela 16 do relatorio --
  ## so' ninhos ocupados)
  sp_dist <- b_tur[, .(min_distance_m = min(distance_m, na.rm = TRUE)), by = .(farm, species)]

  breeding_species_summary <- merge(sp_inv, sp_out, by = c("farm", "species"), all.x = TRUE)
  breeding_species_summary <- merge(breeding_species_summary, sp_dist, by = c("farm", "species"), all.x = TRUE)
  for (col in c("successful", "failed", "not_started", "unknown")) {
    data.table::set(breeding_species_summary, which(is.na(breeding_species_summary[[col]])), col, 0L)
  }

  ## Linha propria para os 50 ninhos de peneireiro-comum em pilares da OHTL
  ## (BSH) -- fora do inventario por ponto de observacao.
  pyl_row <- b_pyl[, .(
    farm = "BSH", species = "Common Kestrel (OHTL pylons)", nests_recorded = .N, nests_active = NA_integer_,
    successful = sum(outcome == "Successful"), failed = sum(outcome == "Failed"),
    not_started = sum(outcome == "Not started"), unknown = sum(outcome == "Unknown"),
    min_distance_m = NA_real_
  )]
  breeding_species_summary <- data.table::rbindlist(list(breeding_species_summary, pyl_row), use.names = TRUE)
  breeding_species_summary[, farm := factor(farm, levels = c("DGY", "BSH"))]
  data.table::setorder(breeding_species_summary, farm, -nests_recorded, species)
  breeding_species_summary[, farm := as.character(farm)]

  breeding_turbine_distances <- data.table::copy(b_tur)[order(farm, distance_m)][
    , .(farm, species, nest_id, nearest_turbine, distance_m, n_turbines_2km)]

  breeding_sheets <- list(
    Breeding_species_summary  = breeding_species_summary,
    Breeding_nest_inventory   = b_inv,
    Breeding_success          = b_succ[, !"outcome"],
    Breeding_pylon_kestrels   = b_pyl[, !"outcome"],
    Breeding_nests_turbines   = b_tur,
    Breeding_previous_year    = b_prev
  )
} else {
  message(sprintf("Aviso: %s nao encontrado -- seccao de breeding survey saltada.", breeding_file))
}


##
## 4. Anexo xlsx ----
##

xlsx_comparison_name <- sprintf("BSH_DGY_comparison_%s.xlsx", analysis_date)
comparison_sheets <- list(
  Species_presence_summary  = species_presence_summary,
  Species_weekly_BSH        = bsh_weekly_species[, .(species, week_start, present)],
  Species_weekly_DGY        = dgy_weekly_species[, .(species, week_start, present)],
  Intensity_summary         = species_intensity_summary,
  Intensity_weekly          = weekly_intensity_both[, .(species, farm, week_start, n_days, n_tracks, tracks_per_day, tracks_per_day_per_idf)],
  Terrain_class_comparison  = terrain_class_comparison,
  Terrain_class_BSH         = bsh$terrain_dt,
  Terrain_class_DGY         = dgy$terrain_dt,
  Terrain_weekly_BSH        = bsh$weekly_terrain_dt,
  Terrain_weekly_DGY        = dgy$weekly_terrain_dt
)
comparison_sheets <- c(comparison_sheets, breeding_sheets)
comparison_sheets <- comparison_sheets[!vapply(comparison_sheets, is.null, logical(1))]
write_xlsx_local(comparison_sheets, file.path(folder_output, xlsx_comparison_name))


##
## 5. Relatorio Word (report/bsh_dgy_comparison_template.rmd) ----
##

if (generate_report) {

  comparison_params <- list(
    title = "BSH-DGY Comparison -- Recommendation Note for Restoring Normal Operation at DGY",
    project_ref = "BSH / DGY",
    report_start = format(compare_start, "%Y-%m-%d"),
    report_end = format(compare_end, "%Y-%m-%d"),
    analysis_date = analysis_date,
    username = username,
    code_version = code_version,

    n_turbines_bsh = bsh$n_turbines_total, n_idf_bsh = bsh$n_idf_total,
    n_turbines_dgy = dgy$n_turbines_total, n_idf_dgy = dgy$n_idf_total,

    priority_species_compare = paste(species_label(priority_species_compare), collapse = ", "),
    species_presence_summary = species_presence_summary,
    species_presence_plot = p_species_presence,
    species_intensity_plot = p_species_intensity,
    species_intensity_summary = species_intensity_summary,

    terrain_class_comparison = terrain_class_comparison,
    terrain_map_plot_bsh = p_terrain_map_bsh,
    terrain_map_plot_dgy = p_terrain_map_dgy,
    terrain_weekly_plot_bsh = p_terrain_weekly_bsh,
    terrain_weekly_plot_dgy = p_terrain_weekly_dgy,

    breeding_species_summary = breeding_species_summary,
    breeding_turbine_distances = breeding_turbine_distances,

    xlsx_comparison = xlsx_comparison_name
  )

  output_docx <- file.path(folder_output, sprintf("BSH_DGY_Comparison_%s.docx", analysis_date))
  build_idf_report(
    output_docx, comparison_params,
    template = "report/bsh_dgy_comparison_template.rmd",
    reference_docx = file.path(folder_input, "Mod.001.05_template_documentos_gerais.docx")
  )
  message(sprintf("\nRelatorio gerado: %s", output_docx))
}
