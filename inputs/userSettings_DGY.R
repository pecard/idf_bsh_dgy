##
## User settings -- Dzhankeldy WPP (DGY)
##
## Reescrito de raiz (2026-08) -- a versao anterior deste ficheiro era um
## rascunho antigo, com varios valores ainda copiados do Bash (project_ref,
## wtg_filename, proj_lat/lon, turbinas_scada). Este ficheiro segue a MESMA
## estrutura de userSettings_BSH.R (as seccoes "## N. ... ----" espelham a
## numeracao de IDF_analysis.R -- os 2 parques partilham o mesmo script de
## analise, ver run_annual_analysis_DGY.R), com os valores farm-specific do
## DGY onde ja se sabem, e TODOs explicitos onde nao.
##
## Diferencas estruturais do DGY, face ao Bash (ver indice do relatorio
## discutido com o Paulo, 2026-08):
##   - Muitas mais turbinas (79) do que unidades IDF instaladas (so' 4
##     confirmadas em "Existing Coverage", ver heartbeat_idf_units abaixo) --
##     so' essas poucas turbinas tem curtailments/SCADA; as seccoes 3.5-3.7
##     (resposta/shutdown time/safe distance) cobrem so' esse subconjunto,
##     nao o parque todo.
##   - Os incidentes de fatalidade conhecidos aconteceram em turbinas SEM
##     unidade IDF -- por isso NAO ha dados de curtailment/resposta para
##     investigar nessas turbinas (ao contrario do Bash). fatality_incidents
##     fica vazio ate' o Paulo confirmar turbina/especie/data de cada caso;
##     mesmo quando preenchido, a secção 4 só' vai poder mostrar a parte de
##     tracks (presença da especie perto da turbina), nao a parte de
##     disponibilidade/resposta a curtailments (essa fica vazia/NA, nao e'
##     um bug -- e' a ausencia real de cobertura IDF nessa turbina).
##   - Sem setores manuais definidos ainda (risk_clusters = FALSE) -- so' a
##     via estatistica (turbine_clustering) corre por agora.
##


##
## Run switches -- que analises correm nesta ronda
##
## Mesma semantica de userSettings_BSH.R: NAO condiciona a leitura de dados
## (seccao 0 de IDF_analysis.R corre sempre) -- so as analises abaixo.
##

run_sections <- list(
  curtailment_response   = TRUE,  # 3.5-3.7 -- so' cobre as poucas turbinas com unidade IDF (turbinas_scada abaixo)
  fatality_investigation = FALSE, # 4 -- LIGAR so' depois de preencher fatality_incidents abaixo (vazio por omissao); mesmo preenchido, so' a parte de tracks tem dados (turbinas de incidente sem IDF -- ver nota no topo do ficheiro)
  coverage_3d             = FALSE,  # 5.2 -- LIGAR so' depois de colocar dem_filename (abaixo) em inputs/
  min_individuals          = TRUE,  # 6.4 -- prioridade do Paulo para o DGY (atividade sazonal por especie prioritaria)
  turbine_clustering       = TRUE,  # 10 (via ESTATISTICA, por distancia) -- via principal do DGY por agora (sem setores manuais definidos)
  risk_clusters            = FALSE  # 10 (via MANUAL/setores) -- LIGAR so' depois de manual_turbine_clusters estar definido abaixo (ainda por fazer)
)


##
## Project inputs (inside folder inputs/)
##

project_ref <- "Dzhankeldy WPP"

# Confirmados nos shapefiles ja' presentes em inputs/ (2026-08): 79
# turbinas (DZH01..DZH88, com saltos na numeracao -- DZH25/29/40/45-48/69/85
# nao existem, mesma situacao do BSH69 em userSettings_BSH.R, nao e' erro)
wtg_filename <- "DZH_Turbines_Sergey_20250401_UTM.shp"  # in .shp

# 6 registos no shapefile, mas so' 4 sao cobertura "Existing" (o resto e'
# cobertura suplementar/planeada, nao unidades instaladas -- ver
# heartbeat_idf_units abaixo)
idf_filename <- "IDF_DZH.shp"  # in .shp

## TODO(Paulo): sem ficheiros de tier scheme para o DGY ainda -- os nomes
## abaixo sao placeholders (o ficheiro nao precisa de existir: IDF_analysis.R
## agora le tier/tier3 de forma opcional, ver secção "0. Import data" --
## tier_dt/tier3_dt nao alimentam nenhuma secção ativa do relatorio de
## qualquer forma). Ajustar so' se algum dia precisares de tier scheme real
## para o DGY.
tier_start_scheme_filename  <- "Dzhankeldy_tier_scheme.xlsx"
tier3_start_scheme_filename <- "Dzhankeldy_tier_3_commissioning.xlsx"

## TODO(Paulo): sem matriz manual turbina<->IDF (equivalente a
## ACWA_IDF_Coverage_Matrix.xlsx do Bash) para o DGY ainda -- opcional
## (IDF_analysis.R ja verifica file.exists() antes de ler, so' fica sem a
## comparacao geometria-vs-manual em turbine_idf_coverage.xlsx). Com so' 4
## unidades e' facil de montar a mao se vier a fazer falta.
turbine_idf_matrix_filename <- "ACWA_IDF_Coverage_Matrix_DGY.xlsx"

# Centroide aproximado das unidades IDF/turbinas (IDF_DZH.shp, bbox
# lat 40.850-40.934, lon 63.285-63.468) -- so' usado para calculo de
# posicao do sol (disponibilidade diurna, secção 3.1), nao precisa de ser
# exato ao metro
proj_lat      <- 40.89
proj_lon      <- 63.38
proj_timezone <- "Asia/Samarkand"

# Zona UTM 41N (60-66°E) cobre tambem a longitude do DGY (~63.3-63.5°E) --
# mesmo codigo do Bash, nao precisa de EPSG diferente
crs_projection_plannar <- 32641


##
## Raw databases (tracks, curtailments, SCADA, heartbeats)
## Ficam FORA do repositorio, numa pasta acima da pasta do projeto.
## Os ficheiros sao identificados por padrao no nome, nao por nome fixo.
##

databases_dir <- "G:/O meu disco/Programacao/r/Bsh_Dgy_WPP/data-raw"

## TODO(Paulo): sem pasta de rede alternativa confirmada para o DGY (o Bash
## tem databases_dir_alt, ver userSettings_BSH.R) -- omitido por omissao
## (IDF_analysis.R so' usa databases_dir_alt se a variavel existir). Definir
## aqui se houver uma pasta de rede equivalente para o DGY.
# databases_dir_alt <- "//192.168.1.11/.../IDF_PortalData/DGY"

## databases_dir acima e' PARTILHADA com o projeto BSH -- farm_pattern
## filtra por substring "DGY" (confirmado pelo Paulo, 2026-08: aparece nos 4
## datasets principais, ao contrario do Bash onde heartbeats precisou de
## uma alternancia "BSH|Bash" -- ver farm_pattern em userSettings_BSH.R e
## list_files_multi_dir(), R/read_utils.R).
farm_pattern <- "DGY"

## Identificador curto e limpo do parque -- usado so' para nao colidir com
## outros parques na pasta cache/ e outputs/AAAAMMDD_<farm_code>/
## partilhadas (ver IDF_analysis.R, logo apos o source() deste ficheiro).
farm_code <- "DGY"

trackreport_pattern  <- "_Track_"             # ex: ..._Track_...csv
curtailments_pattern <- "curtail_orders|Curtailments" # ex: Curtailments_20260201_....xlsx
scada_pattern        <- "SCADA_.+csv"         # ex: SCADA_20260201_....csv
heartbeats_pattern   <- "Heartbeats_.+csv"    # ex: Heartbeats_20260201_....csv

## As 4 unidades IDF confirmadas como "Existing Coverage" em IDF_DZH.shp
## (as outras 2 entradas desse shapefile, ambas "DZH-23", estao em pastas
## "Supplemental Coverage" -- cobertura planeada/suplementar, nao unidades
## instaladas, e sem numero de turbina associado -- por isso excluidas
## daqui; confirmar com o Paulo se "DZH-23" corresponde a alguma unidade
## real antes de a incluir).
heartbeat_idf_units <- c("DZH01-01", "DZH03-02", "DZH64-03", "DZH62-04")


##
## Timeframe for analysis/reporting period
##

## TODO(Paulo): datas por confirmar -- limites largos por omissao (o
## pipeline so' usa dados que existirem dentro deste intervalo; nao ha
## problema em deixar largo, so' ajustar se search precisar de restringir a
## um periodo mais curto)
ini <- as.POSIXct('2024-01-01 00:00:00', tz = proj_timezone)
end <- as.POSIXct('2026-12-31 23:59:59', tz = proj_timezone)


##
## Project's species groups
## Mesmas listas do Bash (mesma regiao/portal/taxonomia) -- ajustar se a
## composicao de especies do DGY for diferente
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
## Analysis parameters, por seccao de IDF_analysis.R
## Mesmos valores do Bash onde a comparabilidade entre parques importa
## (thresholds/criterios metodologicos) -- NAO ALTERAR esses sem alterar
## tambem em userSettings_BSH.R. Valores farm-specific (turbinas, datas,
## limiares de cluster) sao proprios do DGY.
##


## -- 3.1. System Availability (heartbeats) --

heartbeat_interval_min    <- 30
heartbeat_offline_gap_min <- 60
idf_availability_top_n    <- 4L  # so' 4 unidades no total (heartbeat_idf_units acima) -- mostrar todas


## -- 3.2. Curtailments due to ID transitions --
##
## Corre sempre que track_dt/curtl_dt existirem (nao tem switch em
## run_sections, ver IDF_analysis.R) -- por isso estes 2 parametros tem de
## estar definidos mesmo nao fazendo parte do indice do relatorio DGY
## discutido com o Paulo (2026-08); os outputs vao so' para xlsx/png, nao
## para o docx (mesmo comportamento no Bash). Mesmos valores do Bash.

id_transition_late_time_sec <- 50
id_confusion_species_of_interest <- "Kestrel"

## Especie candidata a ser removida da estrategia de curtailment -- mesma
## nota acima (corre sempre, so' vai para xlsx/png, nao para o docx). Kestrel
## por omissao, mesmo criterio do Bash.
curtailment_removal_species_of_interest <- "Kestrel"
curtailment_removal_max_trigger_match_sec <- 30


## -- 3.4. Short-track curtailment --

shorttrack_min_points <- 6
shorttrack_eval_range <- 300


## -- 3.5. Curtailment response assessment (roll join com SCADA) --

## TODO(Paulo): janela de SCADA por confirmar -- largos por omissao, mesma
## nota que ini/end acima
scada_ini <- as.POSIXct('2024-01-01 00:00:00', tz = proj_timezone)
scada_end <- as.POSIXct('2026-12-31 23:59:59', tz = proj_timezone)

# As 4 turbinas com unidade IDF confirmada (heartbeat_idf_units acima, sem
# o sufixo "-NN" da unidade) -- so' estas tem curtailments/SCADA
turbinas_scada <- c('DZH01', 'DZH03', 'DZH62', 'DZH64')

safe_shutdown_rpm <- 1

# herdado do valor do Bash (10, ver o comentario completo em
# userSettings_BSH.R) -- ainda NAO verificado com dados reais de SCADA do
# DGY: a escolha de 10s para o Bash veio de um sweep (start_end_gap_sec vs.
# pct_no_data) sobre o padrao de amostragem/lacunas SCADA especifico das
# turbinas BSH54/BSH62/BSH14, que pode nao se aplicar tal e qual as 4
# turbinas do DGY (turbinas_scada acima). Repetir esse sweep
# (explore_curtailment_response_buffer.R, secções 6/6b) com dados reais do
# DGY antes de confiar neste valor para conclusões operacionais.
curtailment_start_end_gap_sec <- 10
curtailment_max_next_gap_sec   <- 20
curtailment_drop_pct_threshold <- 0.10

curtailment_window_sec         <- 90
curtailment_window_max_gap_sec <- 15


## -- 3.6. Shutdown time (tempo ate atingir limiares de RPM) --

shutdown_time_thresholds <- c(2, 1, 0)
shutdown_time_low_cut    <- 40
shutdown_time_high_cut   <- 50

# mesmo valor de userSettings_BSH.R -- ver o comentario la' para o racional
shutdown_time_buffer_sec <- 60


## -- 3.6b. Response latency (tempo ate a turbina COMECAR a reagir) --

curtailment_latency_decline_pct <- 0.10

# mesmo valor de userSettings_BSH.R -- ver o comentario la' para o racional
curtailment_cutin_rpm <- 3


## -- 3.7. Safe distance (metodologia KNE) --

safe_dist_rpm_threshold       <- 2
safe_dist_speed_trim_q        <- 0.95
safe_dist_reference_line_m    <- 600
safe_dist_already_slowing_rpm <- 6


## -- 4. Fatality investigation --
##
## Vazio por omissao -- ver nota no topo do ficheiro (incidentes conhecidos
## do DGY aconteceram em turbinas SEM unidade IDF, por isso mesmo depois de
## preenchido so' a parte de tracks/presença da especie tem dados; a parte
## de disponibilidade/resposta a curtailments fica vazia/NA para essas
## turbinas -- nao e' um bug, e' a ausencia real de cobertura IDF).
##
## TODO(Paulo): preencher com incident_id/turbine/species/incident_date/
## days_before de cada caso conhecido, no mesmo formato de
## userSettings_BSH.R (secção "4. Fatality investigation"), e mudar
## run_sections$fatality_investigation acima para TRUE.

track_proximity_threshold_m <- 100
fatality_post_incident_days <- 3

fatality_incidents <- data.table::data.table(
  incident_id   = character(),
  turbine       = character(),
  species       = character(),
  incident_date = as.Date(character()),
  days_before   = integer()
)


## -- 5. Coverage --

idf_op_detection_range <- 1000

coverage_cylinder_height       <- 1000
coverage_cylinder_wider_radius <- 1100
coverage_cylinder_inner_radius <- 600

## -- 5.2. WTG coverage 3D com topografia (DEM) --

## TODO(Paulo): colocar o GeoTIFF do DGY (ex: Copernicus GLO-30) em
## inputs/ com este nome (ou ajustar o nome aqui) e mudar
## run_sections$coverage_3d acima para TRUE
dem_filename <- "DZH_DEM_copernicus30m.tif"

wtg_3d_coverage <- c('all')

coverage_mesh_step_xy   <- 50
coverage_mesh_step_z    <- 50
coverage_prox_thresh_m  <- 50

coverage_min_sample_records <- 500000


## -- 6.1-6.2. Flight speed / flight height per species --

flight_min_track_points <- 4
flight_speed_ms_min     <- 1
flight_speed_ms_max     <- 100


## -- 6.3. Risk per species --

riskHeight_lower <- 50
riskHeight_upper <- 250


## -- 6.4. Minimum individuals per time bin (farm-wide) --
##
## Secção prioritaria para o DGY (atividade/fenologia sazonal das especies
## prioritarias, ja que nao ha dados de curtailment/resposta na maioria das
## turbinas) -- ver relatorio, secção expandida com sumario mensal/sazonal
## alem do grafico diario ja existente no Bash.

min_individuals_bin_min      <- 2
min_individuals_merge_dist_m <- 200


## -- 8. System performance vs. bird phenology (evolucao temporal) --

response_timeline_unit <- "week"

curtailment_example_n                 <- 3
curtailment_example_window_before_min <- 1
curtailment_example_window_after_min  <- 3


## -- 9. Turbine recent activity --

recent_activity_days <- 14


## -- 10. Turbine spatial/temporal clustering --

curtl_cluster_date_from <- NULL
curtl_cluster_date_to   <- NULL

## Espacamento real medido a partir de DZH_Turbines_Sergey_20250401_UTM.shp
## (2026-08): mediana do vizinho mais proximo ~517m, media ~599m (79
## turbinas). Mesma logica do Bash (cluster_max_dist_m um pouco acima do
## espacamento tipico, para ligar vizinhos diretos sem encadear demais) --
## 600m fica logo acima da mediana. USAR SEMPRE a tabela de sweep abaixo
## (turbine_cluster_threshold_sensitivity()) antes de confiar no resultado,
## mesma recomendacao de userSettings_BSH.R.
cluster_max_dist_m <- 600

cluster_threshold_sweep_m <- seq(400, 3000, by = 100)

cluster_perm_n <- 999

## TODO(Paulo): sem setores manuais definidos para o DGY ainda
## (run_sections$risk_clusters = FALSE acima, por isso esta variavel nao e'
## sequer referenciada por agora) -- definir aqui, no mesmo formato de
## manual_turbine_clusters em userSettings_BSH.R, quando houver uma proposta
## de setores para as 79 turbinas do DGY.
# manual_turbine_clusters <- list(
#   Setor_A = c("DZH01", "DZH02", ...),
#   ...
# )

## -- 10.2. Kestrel (ou outra especie) track occurrence por cluster --

cluster_species_sel <- c("Kestrel")
