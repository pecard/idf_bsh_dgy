##
## Monthly report settings -- Dzhankeldy WPP (DGY)
##
## Mirrors inputs/monthlyReportSettings_BSH.R structurally (same sections,
## same run_sections_monthly keys) -- ver esse ficheiro para a nota
## completa sobre porque este ficheiro e' standalone (nao source()a
## userSettings_DGY.R). Valores farm-specific (project inputs, raw data
## patterns, species lists reused from userSettings_DGY.R) sao proprios do
## DGY; os thresholds metodologicos partilhados entre parques (ex:
## curtailment_latency_decline_pct, safe_dist_*) usam os MESMOS valores de
## monthlyReportSettings_BSH.R/userSettings_DGY.R para os relatorios serem
## comparaveis -- NAO ALTERAR esses sem alterar tambem la'.
##
## Escolhido por monthly_settings_file (ver IDF_monthly_report.R, secção
## "SETTINGS") -- definir ANTES de dar source a IDF_monthly_report.R (ver
## run_monthly_report_DGY.R).
##
## Uso:
##   report_month <- "2026-07"
##   monthly_settings_file <- "monthlyReportSettings_DGY.R"
##   source("IDF_monthly_report.R")
##


##
## Which sections run in this monthly report
##
## Mesmas seccoes de monthlyReportSettings_BSH.R -- ver esse ficheiro para
## a justificacao de cada uma (e do que fica de fora, por construcao).
##

run_sections_monthly <- list(
  system_availability          = TRUE,  # 2 (heartbeats)
  observed_species             = TRUE,  # 3 (riqueza de especies observadas)
  curtailment_by_species        = TRUE,  # 4
  short_track_curtailment       = TRUE,  # 5
  curtailment_response_delays   = TRUE,  # 6-8 (resposta, shutdown time, safe distance) -- so' cobre as poucas turbinas com unidade IDF (turbinas_scada abaixo, resolvido para "all")
  id_transitions                = TRUE,  # 9
  id_confusion                   = FALSE, # 9b -- ver id_confusion_species_of_interest abaixo
  bio_flight_metrics             = TRUE,  # 10
  min_individuals                = TRUE   # 11
)


##
## Project inputs
##
## So' os literais realmente usados pelas seccoes acima -- mesma nota de
## monthlyReportSettings_BSH.R (nao inclui idf_filename/tier*_filename).
##

project_ref <- "Dzhankeldy WPP"

wtg_filename                <- "DZH_Turbines_Sergey_20250401_UTM.shp"
turbine_idf_matrix_filename <- "ACWA_IDF_Coverage_Matrix_DGY.xlsx"

## Nome da coluna de ID no shapefile de turbinas -- ver a nota completa em
## userSettings_DGY.R e IDF_monthly_report.R, secção "0. Import data"
## (DZH_Turbines_Sergey_20250401_UTM.shp so' tem "Name", sem "InternalNa").
wtg_source_id_col <- "Name"

proj_lat      <- 40.89
proj_lon      <- 63.38
proj_timezone <- "Asia/Samarkand"

crs_projection_plannar <- 32641


##
## Raw databases (tracks, curtailments, SCADA, heartbeats)
## Ficam FORA do repositorio -- mesmos caminhos/padroes de userSettings_DGY.R
## (mesma fonte de dados), duplicados aqui de proposito (ver nota no topo
## deste ficheiro e de monthlyReportSettings_BSH.R).
##

databases_dir <- "G:/O meu disco/Programacao/r/Bsh_Dgy_WPP/data-raw"

## TODO(Paulo): sem pasta de rede alternativa confirmada para o DGY ainda --
## mesma nota de userSettings_DGY.R. Definir aqui se vier a existir.
# databases_dir_alt <- "//192.168.1.11/.../IDF_PortalData/DGY"

## databases_dir e' partilhada com o projeto BSH -- farm_pattern filtra por
## substring "DGY" -- mantido como 2a camada de filtro mesmo agora que
## "DGY" tambem esta' embutido em cada *_pattern abaixo (ver
## userSettings_DGY.R para a nota completa).
farm_pattern <- "DGY"

## Identificador curto do parque -- usado so' para nao colidir com o Bash
## nas pastas cache/ e outputs/monthly/ partilhadas (ver farm_code em
## userSettings_DGY.R, e o mesmo uso em IDF_monthly_report.R).
farm_code <- "DGY"

## "DGY" aparece algures no nome de cada ficheiro, sem posicao fixa face a
## palavra-chave do dataset -- mesma nota completa em userSettings_DGY.R.
trackreport_pattern  <- "(DGY.*_Track_|_Track_.*DGY)"
curtailments_pattern <- "(DGY.*curtail_orders|curtail_orders.*DGY|DGY.*Curtailments|Curtailments.*DGY)"
scada_pattern        <- "(SCADA_.*DGY.*csv|DGY.*SCADA_.+csv)"
heartbeats_pattern   <- "(Heartbeats_.*DGY.*csv|DGY.*Heartbeats_.+csv)"

# Sem heartbeat_idf_units (subconjunto manual de userSettings_DGY.R) -- o
# relatorio mensal usa TODAS as unidades IDF com heartbeat, sem filtro (so'
# 4 confirmadas como "Existing Coverage" -- ver userSettings_DGY.R)


##
## Report period -- 1 calendar month, computed from report_month
##
## report_month e' verificado em IDF_monthly_report.R, antes deste ficheiro
## ser carregado -- nao repetir a verificacao aqui.
##

source("R/monthly_report_utils.R")

report_month_bounds <- month_bounds(report_month, proj_timezone)
ini <- report_month_bounds$ini
end <- report_month_bounds$end

report_title_monthly <- sprintf("%s - Monthly Report (%s)", project_ref, report_month)


##
## Project's species groups -- mesmas listas de userSettings_DGY.R (mesma
## regiao/portal/taxonomia do Bash)
##

prioritysp <- c(
  'Steppe-Eagle',
  'Bearded-Vulture',
  'Egyptian-Vulture',
  'Eurasian-Or-Himalayan-Griffon',
  'Cinereous-Vulture',
  'Golden-Eagle',
  'Imperial-Eagle',
  'Saker-Falcon',
  'Peregrine-Or-Saker-Falcon',
  'White-Tailed-Eagle',
  'Protected',
  'Booted-Eagle',
  'Short-Toed-Snake-Eagle'
)

nonprioritysp <- c(
  'Accipiter',
  'Kestrel',
  'Common-Buzzard',
  'Greater-Spotted-Eagle',
  "Harrier",
  'Honey-Buzzard',
  'Long-Legged-Buzzard',
  'Merlin',
  'Osprey',
  'Peregrine-Falcon',
  'Red-Or-Black-Kite',
  'Sparrow-Hawk',
  "Eagle",
  'Eagle-Unknown',
  'Eagle-Sp'
)

othersp <- c(
  'Common-Crane',
  'White-Stork',
  'Black-Stork',
  'Raven',
  'Pigeon',
  'Grey-Heron',
  'Cormorant',
  'Great-Egret',
  'Lark',
  'Gull',
  'Other',
  'Pelican',
  'Swan',
  'Not-Eagle',
  "Turbine-Blade"
)


##
## Analysis parameters, por seccao de IDF_monthly_report.R -- mesmos valores
## de userSettings_DGY.R/monthlyReportSettings_BSH.R para os parametros
## metodologicos partilhados entre relatorios/parques -- NAO ALTERAR sem
## alterar tambem la', salvo decisao deliberada de usar criterios distintos.
##


## -- 2. System Availability (heartbeats) --

heartbeat_interval_min    <- 30
heartbeat_offline_gap_min <- 60
idf_availability_top_n    <- 4L  # so' 4 unidades no total (mesma nota de userSettings_DGY.R) -- mostrar todas


## -- 5. Short-track curtailment --

shorttrack_min_points <- 6
shorttrack_eval_range <- 300


## -- 6-8. Curtailment response / shutdown time / safe distance --
## (analise so' corre para o range temporal e turbinas com dados de SCADA)

# Janela em que ha dados de SCADA disponiveis -- TODO(Paulo): datas por
# confirmar, mesma nota/largura de userSettings_DGY.R (limiares largos por
# omissao, ajustar so' se search precisar de restringir a um periodo mais
# curto). IDF_monthly_report.R intersecta com o mes do relatorio.
scada_ini <- as.POSIXct('2024-01-01 00:00:00', tz = proj_timezone)
scada_end <- as.POSIXct('2026-12-31 23:59:59', tz = proj_timezone)

# "all" -- resolvido a cada corrida por resolve_turbinas_scada() contra as
# turbinas com dados de SCADA realmente presentes nesse mes (so' as 4 com
# unidade IDF confirmada terao dados -- ver userSettings_DGY.R)
turbinas_scada <- "all"

safe_shutdown_rpm <- 1

# herdado do valor do Bash (10) -- AINDA NAO verificado com dados reais de
# SCADA do DGY, ver a mesma nota completa em userSettings_DGY.R e
# curtailment_start_end_gap_sec em monthlyReportSettings_BSH.R
curtailment_start_end_gap_sec <- 10
curtailment_max_next_gap_sec   <- 20
curtailment_drop_pct_threshold <- 0.10

shutdown_time_thresholds <- c(2, 1, 0)
shutdown_time_low_cut    <- 40
shutdown_time_high_cut   <- 50

# mesmo valor de userSettings_DGY.R -- ver comentario la' para o racional
shutdown_time_buffer_sec <- 60

curtailment_latency_decline_pct <- 0.10

# mesmo valor de userSettings_DGY.R -- ver comentario la' para o racional
curtailment_cutin_rpm <- 3

# Exemplos ilustrativos de perfil de RPM (secção "Example Response
# Profiles") -- mesmos valores de userSettings_DGY.R
curtailment_example_n                 <- 3
curtailment_example_window_before_min <- 1
curtailment_example_window_after_min  <- 3

# "week" ou "month" -- granularidade da serie temporal de latencia (secção
# "Latency Temporal Pattern")
response_timeline_unit <- "week"

safe_dist_rpm_threshold  <- 2
safe_dist_speed_trim_q   <- 0.95
safe_dist_reference_line_m <- 600
safe_dist_already_slowing_rpm <- 6


## -- 9. Bi-directional ID transitions (P<->NP) --

id_transition_late_time_sec <- 50

track_proximity_threshold_m <- 100

# especie(s) a analisar na matriz de confusao -- run_sections_monthly$id_confusion
# esta' FALSE por omissao (ver acima). Ao contrario do Bash (2 especies dos
# incidentes de fatalidade conhecidos), o DGY ainda nao tem incidentes
# confirmados (fatality_incidents vazio em userSettings_DGY.R) -- usa-se
# aqui o mesmo alvo generico de investigacao de confusao de
# id_confusion_species_of_interest em userSettings_DGY.R (Kestrel), nao um
# par ligado a um incidente especifico. Ajustar quando houver um caso
# concreto a investigar.
id_confusion_species_of_interest <- "Kestrel"


## -- 10. Biological flight metrics (speed/height per species) --

flight_min_track_points <- 4
flight_speed_ms_min     <- 1
flight_speed_ms_max     <- 100

riskHeight_lower <- 50
riskHeight_upper <- 250


## -- 11. Minimum individuals per time bin --

min_individuals_bin_min <- 2
min_individuals_merge_dist_m <- 200
