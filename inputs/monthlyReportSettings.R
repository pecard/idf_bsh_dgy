##
## Monthly report settings
##
## Standalone settings file for IDF_monthly_report.R -- does NOT source
## userSettings_BSH.R. The 2 settings files serve different exercises on
## purpose: userSettings_BSH.R backs the overall/history-wide analysis
## (fatality investigation, coverage, turbine clustering, curtailment
## removal risk, etc.), while this file backs the recurring monthly
## operational report (a fixed, smaller set of sections, re-run every month
## over a moving 1-month window). Keeping them independent means either one
## can be tuned (thresholds, species groups, run switches) without risking
## the other -- at the cost of duplicating the few parameters both actually
## need (species groups, database file patterns, thresholds reused by the
## sections kept here). If a parameter changes in one file, check whether
## the same conceptual parameter needs the same change in the other.
##
## As seccoes "## N. ... ----" abaixo espelham a numeracao das seccoes de
## IDF_monthly_report.R, tal como userSettings_BSH.R faz para IDF_analysis.R.
##
## Uso (em IDF_monthly_report.R):
##   report_month <- "2026-07"  # definir ANTES de dar source a este ficheiro
##   source(file.path(folder_input, "monthlyReportSettings.R"))
##


##
## Which sections run in this monthly report
##
## Mirrors run_sections in userSettings_BSH.R, but for the fixed list of
## sections IDF_monthly_report.R implements. Left OUT of this report by
## construction (not represented here at all, so there is nothing to
## switch on): fatality investigation, coverage 3D/topography, curtailment
## removal risk (Kestrel -- a one-off policy analysis over the full
## history, not a periodic metric), turbine spatial/temporal clustering,
## performance-vs-phenology timeline, and turbine recent activity. FALSE
## skips the block (and logs a message saying so), it does not remove it
## from the script.
##

run_sections_monthly <- list(
  system_availability          = TRUE,  # 2 (heartbeats)
  observed_species             = TRUE,  # 3 (riqueza de especies observadas)
  curtailment_by_species        = TRUE,  # 4
  short_track_curtailment       = TRUE,  # 5
  curtailment_response_delays   = TRUE,  # 6-8 (resposta, shutdown time, safe distance) -- so corre se scada_dt tambem existir
  id_transitions                = TRUE,  # 9 (risco de transicao bi-direcional P<->NP, GLOBAL -- todas as especies nao-prioritarias em conjunto; direcao + numeros gerais, sem detalhe por especie)
  id_confusion                   = FALSE, # 9b (matriz de confusao para UMA especie -- id_confusion_species_of_interest, abaixo. DESLIGADA por omissao: e' uma analise pontual, tipo a feita para o Kestrel na discussao de remocao do curtailment trigger, nao uma metrica de rotina mensal. Ligar so' quando for preciso investigar uma especie especifica.)
  bio_flight_metrics             = TRUE,  # 10 (velocidade/altura de voo por especie)
  min_individuals                = TRUE   # 11
)


##
## Project inputs
##
## So os literais realmente usados pelas seccoes acima -- NAO inclui
## idf_filename/tier*_filename (nenhuma seccao do relatorio mensal usa o
## poligono IDF ou os esquemas de tier). wtg_filename e
## turbine_idf_matrix_filename SAO usados (secção 1, plot espacial de
## disponibilidade -- ver IDF_monthly_report.R, seccao "0. Import data").
##

project_ref <- "Bash WPP"

wtg_filename                <- "Bash_Turbines_UTM.shp"
turbine_idf_matrix_filename <- "ACWA_IDF_Coverage_Matrix.xlsx"

proj_lat      <- 40.65
proj_lon      <- 64.67
proj_timezone <- "Asia/Samarkand"

crs_projection_plannar <- 32641 # codigo CRS da projecao planar a usar (plot espacial de disponibilidade)


##
## Raw databases (tracks, curtailments, SCADA, heartbeats)
## Ficam FORA do repositorio, numa pasta acima da pasta do projeto -- mesmos
## caminhos/padroes de ficheiro que userSettings_BSH.R (mesma fonte de
## dados), duplicados aqui de proposito (ver nota no topo deste ficheiro).
##

databases_dir <- "G:/O meu disco/Programacao/r/Bsh_Dgy_WPP/data-raw"
databases_dir_alt <- "//192.168.1.11/DadosBrutos(T2)/Lisboa/08_Tecnica/2025/T05-2025_BSH_DGY/IDF_PortalData/BSH"

trackreport_pattern  <- "TrackReport_"
curtailments_pattern <- "curtail_orders|Curtailments"
scada_pattern        <- "SCADA_.+csv"
heartbeats_pattern   <- "Bash_Heartbeats.+csv"

# Sem heartbeat_idf_units (subconjunto manual de userSettings_BSH.R) -- o
# relatorio mensal usa TODAS as unidades IDF com heartbeat, sem filtro
# (cobertura farm-wide, 79 turbinas)


##
## Report period -- 1 calendar month, computed from report_month
##
## report_month e' verificado em IDF_monthly_report.R, antes deste ficheiro
## ser carregado (este ficheiro so' e' pensado para ser sourced a partir
## dali -- ver "Uso" no topo) -- nao repetir a verificacao aqui.
##

source("R/monthly_report_utils.R")

report_month_bounds <- month_bounds(report_month, proj_timezone)
ini <- report_month_bounds$ini
end <- report_month_bounds$end

report_title_monthly <- sprintf("%s - Monthly Report (%s)", project_ref, report_month)


##
## Project's species groups
## Usadas nas seccoes 4 (curtailments por especie), 9 (transicoes/confusao)
## e 10-11 (biologico) -- mesmas listas de userSettings_BSH.R
##

# 11
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

# 18
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
  'Harrier',
  "Eagle",
  'Eagle-Unknown',
  'Eagle-Sp'
)

# 17
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
  'Pigeon',
  'Other',
  'Pelican',
  'Swan',
  'Not-Eagle',
  "Lark",
  "Turbine-Blade"
)


##
## Analysis parameters, por seccao de IDF_monthly_report.R -- mesmos valores
## de userSettings_BSH.R para os parametros partilhados pelas 2 analises,
## para os resultados serem comparaveis (ex: shutdown_time_high_cut, safe
## distance) -- NAO ALTERAR sem alterar tambem em userSettings_BSH.R, salvo
## decisao deliberada de os 2 relatorios passarem a usar criterios distintos.
##


## -- 2. System Availability (heartbeats) --

heartbeat_interval_min    <- 30
heartbeat_offline_gap_min <- 60
idf_availability_top_n    <- 12L


## -- 5. Short-track curtailment --

shorttrack_min_points <- 6
shorttrack_eval_range <- 300


## -- 6-8. Curtailment response / shutdown time / safe distance --
## (analise so' corre para o range temporal e turbinas com dados de SCADA)

# Janela em que ha dados de SCADA disponiveis -- fixa, igual a
# userSettings_BSH.R; IDF_monthly_report.R intersecta com o mes do relatorio
# (scada_ini_monthly/scada_end_monthly) para nao assumir SCADA antes desta
# data nem incluir historico fora do mes.
scada_ini <- as.POSIXct('2025-10-15 00:00:00', tz = proj_timezone)
scada_end <- as.POSIXct('2026-08-15 23:59:59', tz = proj_timezone)

# "all" -- usa TODAS as turbinas com dados de SCADA na janela do relatorio
# (resolvido a cada corrida por resolve_turbinas_scada(), R/monthly_report_utils.R
# -- acompanha automaticamente se mais turbinas passarem a ter SCADA
# instalado, sem precisar de editar este literal). Alternativa: um vetor
# explicito (ex: c('BSH54', 'BSH62', 'BSH14')) para restringir a um
# subconjunto especifico -- usado tal e qual, sem verificar se tem dados.
turbinas_scada <- "all"

safe_shutdown_rpm <- 1

curtailment_start_end_gap_sec <- 2
curtailment_max_next_gap_sec   <- 20
curtailment_drop_pct_threshold <- 0.10

shutdown_time_thresholds <- c(2, 1, 0)
shutdown_time_low_cut    <- 40
shutdown_time_high_cut   <- 50

safe_dist_rpm_threshold  <- 2
safe_dist_speed_trim_q   <- 0.95
safe_dist_reference_line_m <- 600
safe_dist_already_slowing_rpm <- 6


## -- 9. Bi-directional ID transitions (P<->NP) --

# em segundos; "atraso" de um curtailment apos a reclassificacao NP->P --
# ver R/id_transitions.R e a nota equivalente em userSettings_BSH.R (secção
# 3.2) para os 2 criterios em paralelo (tempo e distancia)
id_transition_late_time_sec <- 50

# em metros; limiar de proximidade a turbina (rotor-swept zone, distancia
# horizontal/2D) reutilizado aqui como o criterio de distancia do "atraso"
# (late_dist_threshold_m) -- mesmo nome/valor/conceito de
# track_proximity_threshold_m em userSettings_BSH.R (secção 4, Fatality
# investigation, que este relatorio nao corre), critério fisico/biologico e
# nao operacional, global para todas as turbinas
track_proximity_threshold_m <- 100

# especie a analisar na matriz de confusao (que outras especies aparecem no
# mesmo track) -- so' tem efeito quando run_sections_monthly$id_confusion =
# TRUE (secção 6b, desligada por omissao -- ver acima). Ajustar este literal
# manualmente para a especie que se quer investigar antes de ligar o switch.
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
