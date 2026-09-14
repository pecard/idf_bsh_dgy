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
  'janitor', 'rstudioapi', 'writexl', 'rmarkdown', 'flextable', 'svDialogs'
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

  ## n_idf_total = length(heartbeat_idf_units), NAO length(unique(track_dt$idf))
  ## -- esta ultima so' contaria unidades que produziram pelo menos 1
  ## deteção na janela filtrada, subestimando o total real se alguma
  ## unidade ficou sem nenhum track no periodo. heartbeat_idf_units e' a
  ## lista canonica de unidades do parque (4 no DGY, coincide com o numero
  ## que o Paulo já' referiu -- "DGY conta com apenas 4 unidades IDF"). Nota
  ## (ver userSettings_BSH.R): para o BSH esta lista tem um proposito mais
  ## estreito (fallback de heartbeats/janela de fatalidade) e PODE nao
  ## cobrir todas as unidades reais do parque -- suficiente para a tabela de
  ## contexto desta nota comparativa, mas nao usar como fonte definitiva se
  ## precisares do total exato (esse calculo, em IDF_analysis.R, usa a
  ## matriz manual turbina<->IDF, nao carregada por este script).
  list(
    farm_label = farm_label, farm_code = farm_code,
    ini = ini, end = end, tz = proj_timezone,
    prioritysp = prioritysp,
    track_dt = track_dt, wtg = wtg,
    n_turbines_total = nrow(wtg), n_idf_total = length(heartbeat_idf_units),
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

## Uniao das 2 listas de especies prioritarias -- praticamente identicas
## (mesma regiao/taxonomia), DGY acrescenta so' Greater-Spotted-Eagle
## (userSettings_DGY.R, comentario "Mesmas listas do Bash"; confirmar se a
## composicao real de especies do DGY justifica mais alguma diferenca).
priority_species_compare <- sort(union(bsh$prioritysp, dgy$prioritysp))

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
  data.table::setorder(wide, species)
  wide[]
}

species_presence_summary <- summarise_species_presence_comparison(weekly_species_both)

## Grafico: 1 tile por especie x semana, Presente/Ausente, facetado por
## parque (2 paineis empilhados) -- mesma paleta 2 cores de
## plot_offline_evidence_slots() (R/availability_daylight.R) para "Online"/
## fundo, mantendo o estilo visual do resto do projeto.
plot_species_presence_heatmap <- function(weekly_species_both, date_breaks = "4 weeks") {
  ggplot(weekly_species_both, aes(x = week_start, y = species, fill = present_lbl)) +
    geom_tile(colour = "white", linewidth = 0.3) +
    facet_wrap(~farm, ncol = 1) +
    scale_fill_manual(name = NULL, values = c(Absent = "#e8e8e8", Present = "#2a78d6"), drop = FALSE) +
    scale_x_date(date_breaks = date_breaks, date_labels = "%d %b %Y", expand = c(0, 0)) +
    labs(x = "Week starting", y = NULL, title = "Weekly presence of priority species") +
    theme_minimal(base_size = 9) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6.5),
      axis.text.y = element_text(size = 7),
      legend.position = "bottom",
      panel.grid = element_blank()
    )
}

p_species_presence <- plot_species_presence_heatmap(weekly_species_both)


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
## 4. Anexo xlsx ----
##

xlsx_comparison_name <- sprintf("BSH_DGY_comparison_%s.xlsx", analysis_date)
comparison_sheets <- list(
  Species_presence_summary  = species_presence_summary,
  Species_weekly_BSH        = bsh_weekly_species[, .(species, week_start, present)],
  Species_weekly_DGY        = dgy_weekly_species[, .(species, week_start, present)],
  Terrain_class_comparison  = terrain_class_comparison,
  Terrain_class_BSH         = bsh$terrain_dt,
  Terrain_class_DGY         = dgy$terrain_dt,
  Terrain_weekly_BSH        = bsh$weekly_terrain_dt,
  Terrain_weekly_DGY        = dgy$weekly_terrain_dt
)
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

    priority_species_compare = paste(priority_species_compare, collapse = ", "),
    species_presence_summary = species_presence_summary,
    species_presence_plot = p_species_presence,

    terrain_class_comparison = terrain_class_comparison,
    terrain_map_plot_bsh = p_terrain_map_bsh,
    terrain_map_plot_dgy = p_terrain_map_dgy,
    terrain_weekly_plot_bsh = p_terrain_weekly_bsh,
    terrain_weekly_plot_dgy = p_terrain_weekly_dgy,

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
