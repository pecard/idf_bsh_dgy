##
## Incident report -- Zarafshan WPP (ZRSHAN)
##
## Relatorio de incidente UNICO (nao o relatorio anual/mensal completo de
## IDF_analysis.R/IDF_monthly_report.R) -- turbina T35, Egyptian-Vulture,
## incidente reportado em 2026-05-11 ("5/11/2026" lido como M/D/Y -- D/M/Y
## daria 2026-11-05, no futuro relativamente a hoje; CONFIRMAR com o Paulo
## se a leitura esta correta antes de dar como definitivo), janela de 15
## dias antes (incluindo) essa data.
##
## Combina, tudo a partir de dados lidos de raiz (sem reutilizar cache de
## nenhum outro parque):
##   1. Turbina/IDF coverage (geometrico, a partir dos 2 shapefiles --
##      gera a "IDF Coverage Matrix" pedida, nao ha matriz manual ainda)
##   2. Disponibilidade das unidades IDF de interesse (janela do incidente
##      + baseline global do periodo) -- R/fatality_window_analysis.R
##   3. Tracks candidatos ao incidente (proximidade a T35 na janela) --
##      R/fatality_track_investigation.R
##   4. Latencia de resposta / eventos sem resposta a curtailments, T35,
##      TODO o periodo (nao so a janela do incidente) -- "overall", para
##      contexto -- R/curtailment_response_latency.R
##   5. Atividade de Egyptian-Vulture: min. individuos por bin de 2min,
##      farm-wide, todo o periodo (secção 6.4 do relatorio anual, aqui so
##      para esta especie) + abundancia pre/pos-incidente (ja' incluida no
##      ponto 2) -- R/track_min_individuals.R
##   6. Tracks candidatos duplicados/fragmentados na janela do incidente,
##      restrito as unidades IDF de interesse -- R/track_harmonization.R
##      (modulo exploratorio, 2026-08 -- ver esse ficheiro para a logica)
##
## Gera um .docx com o MESMO template da empresa usado no BSH/DGY (ver
## R/report.R), a partir de um Rmd mais pequeno dedicado a este incidente
## (report/incident_report_template.rmd), nao o report_template.rmd
## completo (que tem muitas secções sem sentido para um incidente unico --
## clustering de turbinas, safe distance, 3D coverage, etc.).
##
## Uso:
##   1) Confirmar/ajustar inputs/userSettings_ZRF.R (nomes de ficheiro,
##      colunas dos shapefiles, formato dos ficheiros brutos -- ver notas
##      "A CONFIRMAR" nesse ficheiro).
##   2) Ajustar force_reread_cache abaixo (FALSE por omissao -- so TRUE na
##      1a corrida, ou depois de dados novos).
##   3) Dar Source A ESTE FICHEIRO.
##

##
## PACKAGES ----
## Lista mais pequena que IDF_analysis.R (esse carrega tambem geosphere/
## ggTimeSeries/cluster/gt/skimr/vtable, sem consumidor aqui) -- so o que
## as funcoes reutilizadas neste script precisam. ggplot2/sf/magrittr/
## dplyr/janitor precisam de estar ANEXADOS (library()), nao so
## instalados -- varias funcoes em R/ chamam ggplot()/geom_*()/
## st_transform()/%>%/clean_names()/mutate()/bind_rows() sem qualificar
## com o nome do pacote (ex: R/read_tracks.R, R/read_curtailments.R,
## R/read_heartbeats.R -- sem janitor/dplyr anexados, a leitura falha logo
## no 1º ficheiro com "could not find function 'clean_names'").
##

packages <- c(
  'data.table', 'sf', 'magrittr', 'dplyr', 'janitor', 'ggplot2', 'lubridate',
  'readxl', 'writexl', 'plotly', 'htmlwidgets', 'rmarkdown', 'flextable', 'fst', 'scales',
  'suncalc', # usado (namespaced) por R/availability_daylight.R, chamado de dentro de R/fatality_window_analysis.R
  'terra', 'RANN', # usados por R/coverage_3d_topography.R (malha 3D)
  'webshot2' # screenshot estatico (.png) dos plots 3D interativos, para o .docx -- precisa de Chrome/Edge instalado
)
for (p in packages) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

options(error = function() message("Skipping failed step"))


##
## SETTINGS ----
##

tryCatch({
  active_doc_path <- rstudioapi::getActiveDocumentContext()$path
  if (isTRUE(nzchar(active_doc_path))) setwd(dirname(active_doc_path))
}, error = function(e) {
  message(sprintf(
    "Aviso: nao foi possivel mudar para a pasta do script via rstudioapi (%s) -- a usar a working directory atual: %s",
    conditionMessage(e), getwd()
  ))
})

## Falha cedo e com uma mensagem clara se a working directory nao ficou na
## raiz do projeto -- rstudioapi::getActiveDocumentContext()$path so'
## resolve de forma fiavel quando o script corre via "Source" (ou um
## source("run_incident_zrshan.R") escrito na consola); selecionar TUDO e
## correr como bloco (Ctrl+A + Run) envia o texto direto para a consola sem
## passar por source(), o path vem vazio, o setwd() acima e' saltado, e
## qualquer caminho relativo (inputs/..., R/..., cache/...) passa a
## resolver contra a working directory ANTERIOR, nao a pasta do projeto --
## sem este check, isso so' aparece muito mais tarde como um erro confuso
## tipo "Cannot open <shapefile>; The file doesn't seem to exist", mesmo
## com o ficheiro genuinamente presente em inputs/ (caso real, 2026-08).
folder_input <- "inputs"
if (!dir.exists(folder_input) || !dir.exists("R")) {
  stop(sprintf(
    paste(
      "Working directory nao esta na raiz do projeto (esperava encontrar",
      "'%s' e 'R' aqui): %s\nCorre setwd() explicitamente para a pasta do",
      "projeto (ex: setwd(\"C:/Users/pcardoso/R/idf_bsh_dgy\")) antes de",
      "dar source a este ficheiro, ou usa Source (Ctrl+Shift+S) em vez de",
      "selecionar tudo e correr como bloco."
    ),
    folder_input, getwd()
  ))
}
source(file.path(folder_input, "userSettings_ZRF.R"))

folder_output <- file.path("outputs", paste0(format(Sys.time(), "%Y%m%d"), "_", farm_code))
dir.create(folder_output, showWarnings = FALSE, recursive = TRUE)

folder_cache <- file.path("cache", farm_code)

username <- tryCatch(Sys.info()[["user"]], error = function(e) "unknown")

## Mesma logica de IDF_analysis.R -- identifica o commit exato (e se havia
## alteracoes locais nao commitadas) que produziu este relatorio especifico
get_code_version <- function() {
  run_git <- function(args) tryCatch(system2("git", args, stdout = TRUE, stderr = FALSE), error = function(e) character(0))
  hash <- run_git(c("log", "-1", "--format=%h"))
  if (length(hash) == 0L || !nzchar(hash[1])) return("unknown (not a git checkout, or git unavailable)")
  commit_date <- run_git(c("log", "-1", "--format=%ci"))
  dirty <- length(run_git(c("status", "--porcelain"))) > 0L
  out <- if (length(commit_date) > 0L && nzchar(commit_date[1])) sprintf("%s (%s)", hash[1], commit_date[1]) else hash[1]
  if (dirty) paste0(out, " + uncommitted local changes") else out
}
code_version <- get_code_version()


##
## 1. Import data ----
##

source("R/read_utils.R")
source("R/read_tracks.R")
source("R/read_curtailments.R")
source("R/read_scada.R")
source("R/read_heartbeats.R")
source("R/data_cache.R")
source("R/write_utils.R")

## Pasta UNICA e dedicada -- ao contrario de BSH/DGY, so 1 elemento em
## databases_dirs e farm_pattern fica NULL (list_files_multi_dir(), R/
## read_utils.R, so aplica esse filtro quando ha mais de 1 parque na mesma
## pasta, o que nao e' o caso aqui)
databases_dirs <- databases_dir

if (!exists("force_reread_cache")) force_reread_cache <- FALSE

track_dt_unfilt <- reuse_or_load_cache(
  if (exists("track_dt_unfilt")) track_dt_unfilt else NULL,
  "track_dt_unfilt", file.path(folder_cache, "track_dt_unfilt.fst"),
  function() read_tracks_data(databases_dirs, trackreport_pattern, tz = proj_timezone, farm_pattern = NULL),
  force_reread = force_reread_cache, tz = proj_timezone
)
curtl_dt_unfilt <- reuse_or_load_cache(
  if (exists("curtl_dt_unfilt")) curtl_dt_unfilt else NULL,
  "curtl_dt_unfilt", file.path(folder_cache, "curtl_dt_unfilt.fst"),
  function() read_curtailments_data(databases_dirs, curtailments_pattern, tz = proj_timezone, farm_pattern = NULL),
  force_reread = force_reread_cache, tz = proj_timezone
)
scada_dt_unfilt <- reuse_or_load_cache(
  if (exists("scada_dt_unfilt")) scada_dt_unfilt else NULL,
  "scada_dt_unfilt", file.path(folder_cache, "scada_dt_unfilt.fst"),
  function() read_scada_data(databases_dirs, scada_pattern, tz = proj_timezone, farm_pattern = NULL),
  force_reread = force_reread_cache, tz = proj_timezone
)
heartb_dt_unfilt <- reuse_or_load_cache(
  if (exists("heartb_dt_unfilt")) heartb_dt_unfilt else NULL,
  "heartb_dt_unfilt", file.path(folder_cache, "heartb_dt_unfilt.fst"),
  function() read_heartbeats_data(databases_dirs, heartbeats_pattern, tz = proj_timezone, farm_pattern = NULL),
  force_reread = force_reread_cache, tz = proj_timezone
)

## Verificacao rapida -- os 3 vocabularios que run_incident_zrshan.R ASSUME
## coincidirem (ver notas "A CONFIRMAR" em userSettings_ZRF.R): especie,
## turbina, unidade IDF. Corre sempre, mesmo sem force_reread_cache, para
## apanhar um mismatch logo no inicio em vez de secções silenciosamente
## vazias mais a frente. heartbeat_idf_units usa agora o codigo BRUTO do
## heartbeat (ver userSettings_ZRF.R) -- este check compara like-for-like,
## sem normalizacao nenhuma.
message("Especies encontradas em track_dt_unfilt: ", paste(sort(unique(track_dt_unfilt$spec)), collapse = ", "))
message("Turbinas encontradas em track_dt_unfilt (NearestTurbine3d): ", paste(sort(unique(track_dt_unfilt$turbine)), collapse = ", "))
message("Unidades IDF encontradas em track_dt_unfilt: ", paste(sort(unique(track_dt_unfilt$idf)), collapse = ", "))
message("Turbinas encontradas em curtl_dt_unfilt: ", paste(sort(unique(curtl_dt_unfilt$turbine)), collapse = ", "))
message("Unidades IDF encontradas em heartb_dt_unfilt: ", paste(sort(unique(heartb_dt_unfilt$idf)), collapse = ", "))
if (!"Egyptian-Vulture" %in% track_dt_unfilt$spec) {
  message("AVISO: 'Egyptian-Vulture' nao encontrada em track_dt_unfilt$spec -- confirmar vocabulario de especies deste parque.")
}
if (!fatality_incidents$turbine %in% track_dt_unfilt$turbine) {
  message("AVISO: turbina '", fatality_incidents$turbine, "' nao encontrada em track_dt_unfilt$turbine.")
}
if (!all(heartbeat_idf_units %in% heartb_dt_unfilt$idf)) {
  message("AVISO: nem todas as heartbeat_idf_units (", paste(heartbeat_idf_units, collapse = ", "),
          ") foram encontradas em heartb_dt_unfilt$idf -- confirmar o codigo bruto exato no ficheiro (ver userSettings_ZRF.R).")
}

filter_ini <- ini
filter_end <- end

curtl_dt <- as.data.table(curtl_dt_unfilt)[start >= filter_ini & start <= filter_end]

track_dt <- as.data.table(track_dt_unfilt)[timestamp >= filter_ini & timestamp <= filter_end]
track_dt[, count := .N, by = track_id]

scada_dt <- scada_dt_unfilt # NAO FILTRAR -- mesma convencao de IDF_analysis.R

## Filtra pelo codigo BRUTO (heartbeat_idf_units, values -- ver
## userSettings_ZRF.R), depois RELABEL para "IDF<NN>" (names) -- assim o
## resto do pipeline (disponibilidade por janela/baseline, calendario,
## xlsx de anexo, texto do relatorio) trabalha sempre com o rotulo
## legivel, sem cada consumidor ter de conhecer o formato bruto do
## heartbeat. CONFIRMADO pelo Paulo (2026-08): este era o mismatch de
## formato que fazia a disponibilidade (secção 2.3) e o
## Global_availability_by_idf virem sempre vazios -- heartbeat_idf_units
## antes assumia "IDF22" quando o ficheiro real usa "GW32-22".
heartb_dt <- as.data.table(heartb_dt_unfilt)[
  idf %in% heartbeat_idf_units & timestamp >= filter_ini & timestamp <= filter_end
]
heartb_dt[, idf := names(heartbeat_idf_units)[match(idf, heartbeat_idf_units)]]


##
## 2. Turbine/IDF coverage (geometrico) -- gera a IDF Coverage Matrix ----
##

wtg <- sf::read_sf(file.path(folder_input, wtg_filename)) %>% st_transform(crs_projection_plannar)

## Normalizacao de ID -- mesma logica de IDF_analysis.R (letras+digitos,
## zero-pad a 2 digitos). "T35" ja' bate certo com este padrao sem
## alteracoes (letra "T" + digitos).
wtg$InternalNa <- {
  raw_id <- wtg[[wtg_source_id_col]]
  m <- regmatches(raw_id, regexec("^([A-Za-z]+)([0-9]+)$", raw_id))
  vapply(seq_along(raw_id), function(i) {
    g <- m[[i]]
    if (length(g) < 3) return(raw_id[i])
    paste0(g[2], sprintf("%02d", as.integer(g[3])))
  }, character(1))
}

idf <- sf::read_sf(file.path(folder_input, idf_filename))

## Normalizacao para "IDF<NN>" (2 digitos) -- toma sempre a sequencia de
## DIGITOS NO FIM da string como o numero da unidade IDF, o que cobre
## tanto "IDF-22" (assumido inicialmente) como "GW35-24" (formato
## CONFIRMADO pelo Paulo, 2026-08, no instance_name do heartbeat --
## "GW<turbina que aloja a unidade>-<numero da unidade IDF>", ver
## normalizacao de heartb_dt_unfilt$idf acima) e um valor ja' sem hifen
## (ex: "22"), sem precisar de saber de antemao qual dos formatos o
## shapefile usa -- conferir a mensagem "Unidades IDF encontradas no
## shapefile" abaixo. Digitos-no-fim (nao "apos o ultimo hifen") por ser
## idempotente -- ver nota sobre sessão R persistente na normalizacao de
## heartb_dt_unfilt$idf acima.
idf_source_id_col <- if (exists("idf_source_id_col")) idf_source_id_col else "imaging_he"
idf$imaging_he <- sprintf("IDF%02d", as.integer(sub(".*([0-9]+)$", "\\1", idf[[idf_source_id_col]])))
idf <- sf::st_transform(idf, crs_projection_plannar)

message("Unidades IDF encontradas no shapefile (apos normalizacao): ", paste(sort(unique(idf$imaging_he)), collapse = ", "))

## Verificacao cruzada -- as unidades de interesse (heartbeat_idf_units)
## devem aparecer, com o codigo certo, em CADA uma das outras fontes que
## descrevem a MESMA unidade fisica (ver a explicacao completa dos 3
## codigos em userSettings_ZRF.R): o shapefile (rotulo "IDF<NN>", igual a
## names(heartbeat_idf_units)) e o TrackReport/TowerNumber (so' o numero,
## sem prefixo). Corre sempre, para uma futura mudanca de unidades/parque
## nao voltar a passar despercebida como o mismatch original (2026-08).
idf_unit_numbers <- as.integer(sub(".*-", "", heartbeat_idf_units)) # ex: 22, 24, 26, 71 -- mesmo formato de track_dt$idf/TowerNumber
if (!all(names(heartbeat_idf_units) %in% idf$imaging_he)) {
  message(sprintf(
    "AVISO: nem todos os rotulos IDF de heartbeat_idf_units (%s) foram encontrados no shapefile identiflight.shp apos normalizacao (%s) -- confirmar o codigo real dessa unidade no shapefile.",
    paste(names(heartbeat_idf_units), collapse = ", "), paste(sort(unique(idf$imaging_he)), collapse = ", ")
  ))
}
if (!all(idf_unit_numbers %in% suppressWarnings(as.integer(track_dt_unfilt$idf)))) {
  message(sprintf(
    "AVISO: nem todos os numeros de unidade IDF de heartbeat_idf_units (%s) foram encontrados em track_dt_unfilt$idf/TowerNumber (%s).",
    paste(idf_unit_numbers, collapse = ", "), paste(sort(unique(track_dt_unfilt$idf)), collapse = ", ")
  ))
}

source("R/turbine_idf_coverage.R")

turbine_idf_coverage_dt      <- compute_turbine_idf_coverage(wtg, idf, buffer_m = idf_op_detection_range, wtg_id_col = "InternalNa", idf_id_col = "imaging_he")
turbine_idf_coverage_wide_dt <- pivot_turbine_idf_coverage_wide(turbine_idf_coverage_dt)

## Nao ha matriz manual ainda para este parque -- este bloco escreve so a
## versao GEOMETRICA (calculada a partir dos 2 shapefiles), que e' a "IDF
## Coverage Matrix" pedida. Se mais tarde existir uma matriz manual em
## inputs/<turbine_idf_matrix_filename>, este MESMO bloco passa a incluir
## automaticamente a comparacao geometrico-vs-manual, sem alterar nada aqui.
turbine_idf_matrix_file <- file.path(folder_input, turbine_idf_matrix_filename)
if (file.exists(turbine_idf_matrix_file)) {
  turbine_idf_manual_dt <- readxl::read_xlsx(turbine_idf_matrix_file)
  turbine_idf_comparison_dt <- compare_turbine_idf_matrix(turbine_idf_manual_dt, turbine_idf_coverage_dt)
  write_xlsx_local(
    list(Geometric_long = turbine_idf_coverage_dt, Geometric_wide = turbine_idf_coverage_wide_dt,
         Manual_matrix = turbine_idf_manual_dt, Comparison = turbine_idf_comparison_dt),
    file.path(folder_output, "turbine_idf_coverage.xlsx")
  )
} else {
  message("Matriz manual turbina<->IDF nao encontrada (", turbine_idf_matrix_file, ") -- gravada so a matriz GEOMETRICA.")
  write_xlsx_local(
    list(Geometric_long = turbine_idf_coverage_dt, Geometric_wide = turbine_idf_coverage_wide_dt),
    file.path(folder_output, "turbine_idf_coverage.xlsx")
  )
}

## Resumo so' para a turbina do incidente (para o Rmd, secção "Turbine /
## IDF Unit Coverage") -- formato LONGO (1 linha por unidade IDF que
## geometricamente cobre esta turbina, com a respetiva % de sobreposicao),
## nao o formato wide (esse so' fazia sentido a comparar varias turbinas
## lado a lado, sem interesse aqui com uma unica turbina de incidente).
## A tabela wide completa (todas as turbinas) continua no xlsx de anexo acima.
coverage_turbine_of_interest_dt <- turbine_idf_coverage_dt[turbine == fatality_incidents$turbine]


##
## 2b. 3D Coverage (topography-corrected, DEM) -- so a turbina do incidente
##     (pedido do Paulo, 2026-08 -- ver nota em userSettings_ZRF.R sobre
##     porque nao corre farm-wide aqui) ----
##

dem_file <- file.path(databases_dir, dem_filename)

if (identical(wtg_3d_coverage, "all")) {
  message(sprintf("wtg_3d_coverage = \"all\" -- vao ser analisadas todas as %d turbinas do shapefile wtg.", nrow(wtg)))
} else if (length(setdiff(wtg_3d_coverage, wtg$InternalNa)) > 0L) {
  message("AVISO: turbinas em wtg_3d_coverage nao encontradas no shapefile wtg (apos normalizacao): ",
          paste(setdiff(wtg_3d_coverage, wtg$InternalNa), collapse = ", "))
}

if (file.exists(dem_file)) {

  source("R/coverage_3d_topography.R")

  ## usa track_dt_unfilt (nao o track_dt filtrado por ini/end) -- mesma
  ## convencao de IDF_analysis.R secção 5.2: a amostra de deteções para
  ## estimar cobertura beneficia do historico completo, nao so do periodo
  ## do relatorio (aqui coincidem de qualquer forma, ja que ini/end cobre
  ## toda a serie disponivel para este parque)
  cov_all <- run_coverage_3d_all_turbines(
    wtg, track_dt_unfilt, dem_file,
    radius = coverage_cylinder_wider_radius, cyl_height = coverage_cylinder_height,
    step_xy = coverage_mesh_step_xy, step_z = coverage_mesh_step_z,
    prox_thresh_m = coverage_prox_thresh_m,
    wtg_sel = wtg_3d_coverage,
    min_sample_records = coverage_min_sample_records
  )

  summary_cov <- summarise_mesh_coverage(lapply(cov_all, `[[`, "coverage"))

  write_xlsx_local(
    list(By_turbine = summary_cov$by_turbine, By_turbine_risk_band = summary_cov$by_turbine_risk_band),
    file.path(folder_output, "coverage_3d_summary.xlsx")
  )

  ## Plots interativos (plotly, html) -- cobertura + o inverso (nos da
  ## malha "air" SEM deteções dentro de prox_thresh_m) -- gravados a parte
  ## (Word nao suporta plotly interativo), MAIS uma versao screenshot (.png,
  ## via webshot2/Chrome headless) da turbina do incidente, para poder ser
  ## embebida diretamente no .docx (pedido do Paulo, 2026-08, secção "3D
  ## Coverage" do relatorio)
  coverage3d_png_paths <- save_coverage_3d_plots(
    cov_all, file.path(folder_output, "coverage_3d"),
    radius = coverage_cylinder_wider_radius, cyl_height = coverage_cylinder_height,
    screenshot = TRUE
  )

  coverage3d_covered_png     <- coverage3d_png_paths[[fatality_incidents$turbine]]$covered
  coverage3d_not_covered_png <- coverage3d_png_paths[[fatality_incidents$turbine]]$not_covered

} else {
  message("DEM nao encontrado (", dem_file, ") -- secção 3D Coverage saltada.")
  summary_cov <- NULL
  coverage3d_covered_png     <- NULL
  coverage3d_not_covered_png <- NULL
}


##
## 3+2. Fatality investigation (tracks + disponibilidade + resposta na
##      janela + abundancia pre/pos) -- reutiliza R/fatality_track_investigation.R
##      e R/fatality_window_analysis.R tal como IDF_analysis.R secção 4 ----
##

source("R/fatality_track_investigation.R")

fatality_tracks_dt <- investigate_fatality_incidents(
  fatality_incidents, track_dt, curtl_dt, wtg,
  proximity_threshold_m = track_proximity_threshold_m,
  height_threshold_m = if (exists("curtailment_trigger_height_m")) curtailment_trigger_height_m else NULL
)
fatality_summary <- summarise_fatality_tracks(fatality_tracks_dt, top_n = 10)

## Exemplos de RPM para a secção "Top Candidate Tracks" do relatorio -- o 1º
## track (mais perto da turbina, fatality_tracks_dt ja vem ordenado por
## min_dist_m) com signal "no_curtailment_lost_near_turbine" e o 1º com
## "curtailment_lost_near_turbine", pedido do Paulo (2026-08) para ilustrar
## visualmente os 2 sinais mais criticos da tabela "Candidate Tracks by
## Signal" (secção 2.1) -- ver plot_fatality_track_rpm(),
## R/fatality_track_investigation.R
##
## IMPORTANTE: filtra por "signal" (que ja' incorpora o limiar de altura
## height_threshold_m, quando definido), NAO por "triggered_curtailment"
## sozinho -- um track sem curtailment mas SEMPRE acima da altura de risco
## (ex: 400m AGL) e' corretamente "far_from_turbine", nao um candidato
## real, e nao deve ser escolhido como "exemplo de no curtailment" so'
## porque e' o mais proximo HORIZONTALMENTE entre TODOS os sem curtailment
## (caso real, 2026-08 -- via Paulo: track 690799BA-..., sempre acima de
## 400m, estava a ser escolhido apesar de "far_from_turbine").
fatality_example_no_curtailment_dt <- fatality_tracks_dt[signal == "no_curtailment_lost_near_turbine"][1]
fatality_example_curtailment_dt    <- fatality_tracks_dt[signal == "curtailment_lost_near_turbine"][1]

p_fatality_example_no_curtailment <- if (nrow(fatality_example_no_curtailment_dt) > 0 && !is.na(fatality_example_no_curtailment_dt$track_id)) {
  plot_fatality_track_rpm(
    fatality_example_no_curtailment_dt, scada_dt, curtl_dt,
    window_before_min = fatality_example_window_before_min, window_after_min = fatality_example_window_after_min,
    title = sprintf("Example -- No Curtailment Triggered (track %s)", fatality_example_no_curtailment_dt$track_id)
  )
} else NULL

p_fatality_example_curtailment <- if (nrow(fatality_example_curtailment_dt) > 0 && !is.na(fatality_example_curtailment_dt$track_id)) {
  plot_fatality_track_rpm(
    fatality_example_curtailment_dt, scada_dt, curtl_dt,
    window_before_min = fatality_example_window_before_min, window_after_min = fatality_example_window_after_min,
    title = sprintf("Example -- Curtailment Triggered (track %s)", fatality_example_curtailment_dt$track_id)
  )
} else NULL

source("R/availability_daylight.R")
source("R/curtailment_response.R")
source("R/curtailment_response_latency.R")
source("R/curtailment_forensic_trace.R") # plot_curtailment_events_rpm() -- reutilizado na secção "Curtailment Response & Latency -- Overall" abaixo
source("R/track_min_individuals.R")
source("R/fatality_window_analysis.R")

fatality_windows <- summarise_fatality_windows(
  fatality_incidents, heartb_dt,
  curtl_dt = curtl_dt,
  scada_dt = if (exists("scada_dt")) scada_dt else data.table::data.table(),
  manual_matrix_dt = if (exists("turbine_idf_manual_dt")) turbine_idf_manual_dt else NULL,
  lat = proj_lat, lon = proj_lon, tz = proj_timezone,
  offline_gap_min = heartbeat_offline_gap_min, online_grace_min = heartbeat_interval_min,
  start_end_gap_sec = curtailment_start_end_gap_sec, decline_pct_threshold = curtailment_latency_decline_pct,
  buffer_after_end_sec = shutdown_time_buffer_sec, cutin_rpm = curtailment_cutin_rpm,
  fallback_idf_units = names(heartbeat_idf_units), # heartb_dt$idf ja' vem relabeled para "IDF<NN>" (ver acima)
  track_dt = track_dt, post_days = fatality_post_incident_days,
  min_indiv_bin_min = min_individuals_bin_min, min_indiv_merge_dist_m = min_individuals_merge_dist_m,
  global_avail_from = ini, global_avail_to = end,
  global_response_from = scada_ini, global_response_to = scada_end
)

fatality_window_availability_dt <- data.table::rbindlist(lapply(names(fatality_windows), function(id) {
  dt <- fatality_windows[[id]]$availability$by_idf
  if (nrow(dt) == 0L) return(NULL)
  dt[, incident_id := id]; dt[]
}), fill = TRUE)

fatality_window_response_dt <- data.table::rbindlist(lapply(names(fatality_windows), function(id) {
  dt <- fatality_windows[[id]]$curtailment_response$detail
  if (nrow(dt) == 0L) return(NULL)
  dt[, incident_id := id]; dt[]
}), fill = TRUE)

fatality_window_response_summary_dt <- data.table::rbindlist(lapply(names(fatality_windows), function(id) {
  dt <- fatality_windows[[id]]$curtailment_response$summary
  if (nrow(dt) == 0L) return(NULL)
  dt[, incident_id := id]; dt[]
}), fill = TRUE)

fatality_global_availability_dt <- data.table::rbindlist(lapply(names(fatality_windows), function(id) {
  dt <- fatality_windows[[id]]$availability_global$by_idf
  if (is.null(dt) || nrow(dt) == 0L) return(NULL)
  dt[, incident_id := id]; dt[]
}), fill = TRUE)

## Diagnostico -- janela vs. baseline global usam exatamente as mesmas
## unidades IDF (idf_units) e o mesmo heartb_dt, com o periodo global a ser
## um SUPERCONJUNTO estrito do periodo da janela (ver R/fatality_window_analysis.R)
## -- por construcao, se a janela tem linhas, o baseline global tem de ter
## pelo menos as mesmas. Se este aviso disparar (0 linhas de um lado so'),
## e' sinal de um problema a montante (ex: heartb_dt vazio para estas
## unidades no intervalo esperado) -- confirmar o formato de
## heartbeat_idf_units vs. heartb_dt_unfilt$idf (ver mensagens no inicio
## deste script) antes de assumir que e' um bug de calculo.
message(sprintf(
  "Disponibilidade IDF -- janela: %d linha(s) (unidades: %s); baseline global: %d linha(s) (unidades: %s).",
  nrow(fatality_window_availability_dt),
  paste(unique(fatality_window_availability_dt$idf), collapse = ", "),
  nrow(fatality_global_availability_dt),
  paste(unique(fatality_global_availability_dt$idf), collapse = ", ")
))

## Calendario de disponibilidade (% offline em horas de luz, por dia) das
## unidades IDF de interesse, restrito a janela de investigacao -- pedido
## do Paulo (2026-08), secção "Investigation Window" do relatorio de
## incidente. idf_sel = names(heartbeat_idf_units) (rotulo "IDF<NN>", o
## mesmo formato de fatality_window_daily_dt$idf apos o relabel de
## heartb_dt acima), nao top_n por omissao, para mostrar SEMPRE todas as
## unidades de interesse, nao so as com mais tempo offline -- ver
## R/availability_daylight.R, plot_availability_calendar()
fatality_window_daily_dt <- fatality_windows[[1]]$availability$daily
p_fatality_availability_calendar <- if (!is.null(fatality_window_daily_dt) && nrow(fatality_window_daily_dt) > 0) {
  plot_availability_calendar(
    fatality_window_daily_dt, fatality_windows[[1]]$availability$by_idf,
    idf_sel = names(heartbeat_idf_units)
  )
} else NULL

fatality_global_response_summary_dt <- data.table::rbindlist(lapply(names(fatality_windows), function(id) {
  dt <- fatality_windows[[id]]$curtailment_response_global$summary
  if (is.null(dt) || nrow(dt) == 0L) return(NULL)
  dt[, incident_id := id]; dt[]
}), fill = TRUE)

fatality_abundance_pre_post_dt <- data.table::rbindlist(lapply(names(fatality_windows), function(id) {
  dt <- fatality_windows[[id]]$abundance
  if (is.null(dt) || nrow(dt) == 0L) return(NULL)
  dt[, incident_id := id]; dt[]
}), fill = TRUE)

write_xlsx_local(
  list(
    Fatality_tracks            = fatality_tracks_dt,
    Fatality_signal_counts     = fatality_summary$counts_by_signal,
    Fatality_top_candidates    = fatality_summary$top_candidates,
    Window_availability_by_idf = fatality_window_availability_dt,
    Window_response_detail     = fatality_window_response_dt,
    Window_response_summary    = fatality_window_response_summary_dt,
    Global_availability_by_idf = fatality_global_availability_dt,
    Global_response_summary    = fatality_global_response_summary_dt,
    Abundance_pre_post         = fatality_abundance_pre_post_dt
  ),
  file.path(folder_output, "fatality_track_investigation.xlsx")
)


##
## 4. Curtailment response & latency -- OVERALL (todo o periodo, nao so a
##    janela do incidente), so para T35 -- para contexto ----
##

curtl_scada_dt <- curtl_dt[turbine %in% turbinas_scada & start >= scada_ini & start <= scada_end]

latency_dt <- time_to_first_decline(
  curtl_scada_dt, scada_dt, decline_pct_threshold = curtailment_latency_decline_pct,
  start_end_gap_sec = curtailment_start_end_gap_sec, buffer_after_end_sec = shutdown_time_buffer_sec,
  cutin_rpm = curtailment_cutin_rpm
)
summary_latency            <- summarise_latency(latency_dt)
summary_latency_by_turbine <- summarise_latency_by_turbine(latency_dt)
summary_latency_bands      <- summarise_latency_bands(latency_dt)

p_latency <- plot_latency_histogram(latency_dt)

source("R/curtailment_response_timeline.R")
latency_timeline_dt <- summarise_latency_timeline(latency_dt, unit = response_timeline_unit)
p_latency_timeline  <- plot_latency_timeline(latency_timeline_dt)

## Exemplos tipicos de no-response / resposta lenta (RPM + inicio/fim do
## curtailment) -- mesmo mecanismo do relatorio anual BSH/DGY
## (IDF_analysis.R, select_latency_examples() + plot_curtailment_events_rpm(),
## R/curtailment_forensic_trace.R), pedido do Paulo (2026-08) para este
## relatorio tambem
no_response_examples_dt      <- select_latency_examples(latency_dt, "no_response", n = curtailment_example_n)
slowest_response_examples_dt <- select_latency_examples(latency_dt, "slowest", n = curtailment_example_n)

p_no_response_examples <- plot_curtailment_events_rpm(
  no_response_examples_dt, scada_dt,
  window_before_min = curtailment_example_window_before_min,
  window_after_min = curtailment_example_window_after_min,
  title = "No-Response Events -- RPM Profile (Examples)"
)
p_slowest_response_examples <- plot_curtailment_events_rpm(
  slowest_response_examples_dt, scada_dt,
  window_before_min = curtailment_example_window_before_min,
  window_after_min = curtailment_example_window_after_min,
  title = "Slowest Responses -- RPM Profile (Examples)"
)

write_xlsx_local(
  list(Latency = latency_dt, Overall = summary_latency, By_turbine = summary_latency_by_turbine,
       Bands = summary_latency_bands, Latency_timeline = latency_timeline_dt,
       No_response_examples = no_response_examples_dt, Slowest_response_examples = slowest_response_examples_dt),
  file.path(folder_output, paste0("curtailment_response_latency_overall_", date(scada_ini), "to", date(scada_end), ".xlsx"))
)


##
## 5. Egyptian-Vulture activity -- min. individuos por bin de 2min,
##    restrito as turbinas investigadas (nao farm-wide -- este relatorio
##    investiga so' a turbina do incidente, T35, pedido do Paulo 2026-08),
##    todo o periodo (abundancia pre/pos-incidente ja' vem do ponto 2
##    acima, fatality_abundance_pre_post_dt) ----
##
## Restringe por track_dt$turbine (NearestTurbine3d, a classificacao do
## proprio IdentiFlight, ja' confirmada consistente com o shapefile wtg em
## todo o resto do pipeline -- coverage, candidatos a fatalidade, etc.),
## NAO por track_dt$idf (TowerNumber, R/read_tracks.R) -- essa coluna vem
## de uma fonte DIFERENTE de heartb_dt$idf (instance_name,
## R/read_heartbeats.R) e o seu formato nunca foi confirmado como
## coincidindo com heartbeat_idf_units ("IDF22"/"IDF24"/...); usada aqui
## originalmente, dava sempre 0 bins (caso real, 2026-08 -- via Paulo:
## "candidate tracks for collision" existiam, logo "No detections" nao
## podia estar certo). turbinas_scada e' o mesmo vetor ja' usado para
## restringir o "overall" da secção 4 a T35.
min_indiv_bins_dt  <- count_min_individuals_per_bin(track_dt[turbine %in% turbinas_scada], species = "Egyptian-Vulture", bin_min = min_individuals_bin_min, merge_dist_m = min_individuals_merge_dist_m)

min_indiv_summary_dt <- summarise_min_individuals(min_indiv_bins_dt)
min_indiv_daily_dt <- summarise_daily_max_individuals(min_indiv_bins_dt)
p_min_indiv_daily  <- plot_daily_max_individuals(min_indiv_daily_dt, species_sel = "Egyptian-Vulture", date_breaks = "1 month", geom_type = "bar")

write_xlsx_local(
  list(Bins = min_indiv_bins_dt, Summary = min_indiv_summary_dt, Daily_peak = min_indiv_daily_dt),
  file.path(folder_output, "min_individuals_egyptian_vulture.xlsx")
)


##
## 6. Candidate/duplicate tracks for the incident -- R/track_harmonization.R,
##    restrito a especie Egyptian-Vulture, janela do incidente, turbina(s)
##    investigada(s) ----
##

source("R/track_min_individuals.R") # .uf_components()
source("R/track_harmonization.R")

incident_window_start <- as.POSIXct(paste(fatality_incidents$incident_date - fatality_incidents$days_before, "00:00:00"), tz = proj_timezone)
incident_window_end   <- as.POSIXct(paste(fatality_incidents$incident_date, "23:59:59"), tz = proj_timezone)

## Restringe por turbine (NearestTurbine3d), nao por idf (TowerNumber) --
## ver nota na secção 5 acima sobre track_dt$idf nunca ter sido confirmado
## como coincidindo com heartbeat_idf_units. A logica interna de
## handoff/duplicado (find_handoff_edges()/find_duplicate_edges(),
## R/track_harmonization.R) continua a usar os valores brutos de idf para
## diferenciar unidades -- so' o filtro EXTERNO de "que tracks entram nesta
## analise" mudou.
track_dt_incident_window <- track_dt[
  spec == fatality_incidents$species &
    turbine %in% turbinas_scada &
    timestamp >= incident_window_start & timestamp <= incident_window_end
]

message(sprintf(
  "Janela do incidente (%s a %s): %d pontos de %s em %d track_ids, turbina(s) %s.",
  format(incident_window_start), format(incident_window_end), nrow(track_dt_incident_window),
  fatality_incidents$species, data.table::uniqueN(track_dt_incident_window$track_id),
  paste(turbinas_scada, collapse = ", ")
))

## Nomes de ficheiro condicionais -- so ficam definidos (nao-NULL) se o
## ficheiro correspondente for mesmo escrito no bloco abaixo, para o Rmd
## nunca apontar num "annex_note"/nota de ficheiro que nao existe (ver
## report/incident_report_template.rmd, secção Candidate Duplicate/
## Fragmented Tracks)
xlsx_candidate_tracks_name <- NULL
candidate_tracks_html_name <- NULL

if (data.table::uniqueN(track_dt_incident_window$track_id) >= 2L) {

  handoff_edges_incident <- find_handoff_edges(
    track_dt_incident_window, fatality_incidents$species,
    time_window_sec = harmonization_handoff_time_window_sec, max_dist_m = harmonization_handoff_max_dist_m
  )
  duplicate_edges_incident <- find_duplicate_edges(
    track_dt_incident_window, fatality_incidents$species,
    max_median_dist_m = harmonization_duplicate_max_median_dist_m, max_spread_m = harmonization_duplicate_max_spread_m,
    min_overlap_frac = harmonization_duplicate_min_overlap_frac, min_overlap_sec = harmonization_duplicate_min_overlap_sec
  )
  reconciliation_incident <- build_reconciliation_groups(
    track_dt_incident_window, fatality_incidents$species, handoff_edges_incident, duplicate_edges_incident
  )
  reconciliation_summary_incident_dt <- summarise_reconciliation(reconciliation_incident$groups)

  synth_incident_dt <- stitch_synthetic_tracks(
    track_dt_incident_window, fatality_incidents$species, reconciliation_incident$groups, duplicate_edges_incident
  )

  xlsx_candidate_tracks_name <- "candidate_tracks_incident_window.xlsx"
  write_xlsx_local(
    list(
      Groups              = reconciliation_incident$groups,
      Edges               = reconciliation_incident$edges,
      Reconciliation_summary = reconciliation_summary_incident_dt,
      Synthetic_tracks    = synth_incident_dt
    ),
    file.path(folder_output, xlsx_candidate_tracks_name)
  )

  ## Plot interativo (plotly, html) do maior grupo reconciliado (candidato
  ## mais provavel a ter sido fragmentado em varios track_ids) -- gravado a
  ## parte (nao entra no .docx, que nao suporta plotly interativo)
  biggest_group_incident <- reconciliation_incident$groups[, .N, by = synth_track_id][N == max(N), synth_track_id][1]
  p_candidate_tracks_incident <- plot_synthetic_track(track_dt_incident_window, synth_incident_dt, biggest_group_incident)
  candidate_tracks_html_name <- "candidate_tracks_incident_biggest_group.html"
  htmlwidgets::saveWidget(
    p_candidate_tracks_incident,
    file.path(folder_output, candidate_tracks_html_name),
    selfcontained = TRUE
  )

  ## Versao estatica (ggplot2) do mesmo grupo, para embeber diretamente no
  ## .docx -- pedido do Paulo, 2026-08 ("is there any print screen... a
  ## plot or a table with syntetic tracks merged?") -- mais a tabela de
  ## detalhe dos tracks originais que compoem o grupo (mesma que
  ## inspect_reconciliation_group() ja calcula para inspecao manual)
  p_candidate_tracks_static <- plot_synthetic_track_static(
    track_dt_incident_window, synth_incident_dt, biggest_group_incident,
    title = sprintf("Largest Reconciled Group -- %s", biggest_group_incident)
  )
  candidate_tracks_group_detail_dt <- inspect_reconciliation_group(
    track_dt_incident_window, reconciliation_incident$groups, reconciliation_incident$edges, biggest_group_incident
  )$tracks

} else {
  message("Menos de 2 track_ids de ", fatality_incidents$species, " na janela do incidente -- harmonizacao de tracks saltada.")
  reconciliation_summary_incident_dt <- data.table::data.table()
  p_candidate_tracks_static <- NULL
  candidate_tracks_group_detail_dt <- data.table::data.table()
  biggest_group_incident <- NA_character_
}


##
## 7. Word report ----
##

report_params <- list(
  title         = paste("Incident Report -", project_ref, "-", fatality_incidents$incident_id),
  project_ref   = project_ref,
  incident_id   = fatality_incidents$incident_id,
  incident_turbine = fatality_incidents$turbine,
  incident_species = fatality_incidents$species,
  incident_date    = as.character(fatality_incidents$incident_date),
  incident_days_before = fatality_incidents$days_before,
  incident_window_start = as.character(as.Date(incident_window_start)),
  incident_window_end   = as.character(as.Date(incident_window_end)),
  idf_units_of_interest = paste(names(heartbeat_idf_units), collapse = ", "), # rotulo "IDF<NN>", nao o codigo bruto do heartbeat
  investigated_turbines = paste(turbinas_scada, collapse = ", "),
  report_start  = as.character(as.Date(ini)),
  report_end    = as.character(as.Date(end)),
  analysis_date = format(Sys.time(), "%Y-%m-%d"),
  username      = username,
  code_version  = code_version,

  coverage_turbine_of_interest = coverage_turbine_of_interest_dt,
  coverage3d_by_turbine        = if (!is.null(summary_cov)) summary_cov$by_turbine else NULL,
  coverage3d_covered_png       = if (!is.null(coverage3d_covered_png)) normalizePath(coverage3d_covered_png) else NULL,
  coverage3d_not_covered_png   = if (!is.null(coverage3d_not_covered_png)) normalizePath(coverage3d_not_covered_png) else NULL,

  fatality_signal_counts           = fatality_summary$counts_by_signal,
  fatality_top_candidates          = fatality_summary$top_candidates,
  fatality_example_no_curtailment_plot     = p_fatality_example_no_curtailment,
  fatality_example_curtailment_plot        = p_fatality_example_curtailment,
  fatality_example_no_curtailment_track_id = if (nrow(fatality_example_no_curtailment_dt) > 0) fatality_example_no_curtailment_dt$track_id else NULL,
  fatality_example_curtailment_track_id    = if (nrow(fatality_example_curtailment_dt) > 0) fatality_example_curtailment_dt$track_id else NULL,
  fatality_window_availability     = fatality_window_availability_dt,
  fatality_global_availability     = fatality_global_availability_dt,
  fatality_availability_calendar_plot = p_fatality_availability_calendar,
  fatality_window_response_summary = fatality_window_response_summary_dt,
  fatality_abundance_pre_post      = fatality_abundance_pre_post_dt,

  latency_by_turbine    = summary_latency_by_turbine,
  latency_bands         = summary_latency_bands,
  latency_plot          = p_latency,
  latency_timeline_plot = p_latency_timeline,
  latency_n_no_data     = summary_latency$n_no_data,
  latency_pct_no_data   = summary_latency$pct_no_data,
  latency_n_below_cutin = summary_latency$n_below_cutin,
  latency_pct_below_cutin = summary_latency$pct_below_cutin,
  latency_no_response_examples_plot = p_no_response_examples,
  latency_slowest_examples_plot     = p_slowest_response_examples,
  latency_n_no_response_examples    = nrow(no_response_examples_dt),
  latency_n_slowest_examples        = nrow(slowest_response_examples_dt),

  min_indiv_summary    = min_indiv_summary_dt,
  min_indiv_plot_daily = p_min_indiv_daily,

  candidate_tracks_reconciliation_summary = reconciliation_summary_incident_dt,
  candidate_tracks_plot          = p_candidate_tracks_static,
  candidate_tracks_group_detail  = candidate_tracks_group_detail_dt,
  candidate_tracks_biggest_group = biggest_group_incident,

  heartbeat_interval_min    = heartbeat_interval_min,
  heartbeat_offline_gap_min = heartbeat_offline_gap_min,
  curtailment_start_end_gap_sec  = curtailment_start_end_gap_sec,
  curtailment_latency_decline_pct = curtailment_latency_decline_pct,
  curtailment_cutin_rpm           = curtailment_cutin_rpm,
  safe_shutdown_rpm               = safe_shutdown_rpm,
  shutdown_time_buffer_sec        = shutdown_time_buffer_sec,
  track_proximity_threshold_m     = track_proximity_threshold_m,
  curtailment_trigger_height_m    = if (exists("curtailment_trigger_height_m")) curtailment_trigger_height_m else NULL,
  fatality_post_incident_days     = fatality_post_incident_days,
  min_individuals_bin_min         = min_individuals_bin_min,
  min_individuals_merge_dist_m    = min_individuals_merge_dist_m,
  harmonization_handoff_time_window_sec = harmonization_handoff_time_window_sec,
  harmonization_handoff_max_dist_m      = harmonization_handoff_max_dist_m,
  harmonization_duplicate_max_median_dist_m = harmonization_duplicate_max_median_dist_m,
  harmonization_duplicate_max_spread_m      = harmonization_duplicate_max_spread_m,

  xlsx_coverage      = "turbine_idf_coverage.xlsx",
  xlsx_coverage3d    = if (!is.null(summary_cov)) "coverage_3d_summary.xlsx" else NULL,
  xlsx_fatality      = "fatality_track_investigation.xlsx",
  xlsx_latency       = paste0("curtailment_response_latency_overall_", date(scada_ini), "to", date(scada_end), ".xlsx"),
  xlsx_min_indiv     = "min_individuals_egyptian_vulture.xlsx",
  xlsx_candidate_tracks = xlsx_candidate_tracks_name,
  candidate_tracks_html = candidate_tracks_html_name
)

source("R/report.R")
build_idf_report(
  output_file    = file.path(folder_output, paste0("Incident_Report_", fatality_incidents$incident_id, "_", fatality_incidents$incident_date, ".docx")),
  report_params  = report_params,
  template       = "report/incident_report_template.rmd",
  reference_docx = file.path(folder_input, "Mod.001.05_template_documentos_gerais.docx")
)
