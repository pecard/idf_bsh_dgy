##
## User settings -- Zarafshan WPP (ZRSHAN), incident report
## Incidente: Egyptian Vulture, turbina T94, 01/10/2026
##
## Copia de inputs/userSettings_ZRF.R (incidente anterior, T35/Maio 2026),
## adaptada para ESTE incidente -- ver run_incident_zrshan.R, secção
## "project_settings_file" (topo do ficheiro): corre com
##   project_settings_file <- "userSettings_ZRF_T94_20261001.R"
##   source("run_incident_zrshan.R")
## (ou usa o lancador dedicado run_incident_zrshan_T94_20261001.R).
## userSettings_ZRF.R (T35) fica intacto, para poderes regenerar esse
## relatorio mais tarde sem reconstruir os parametros.
##
## O que mudou face a userSettings_ZRF.R (T35): fatality_incidents,
## turbinas_scada, wtg_3d_coverage, heartbeat_idf_units (ver nota "A
## CONFIRMAR" abaixo), end/scada_end (alargados para cobrir o incidente,
## 2026-10-01 -- o ficheiro do T35 so' ia ate' 2026-08-15). Tudo o resto
## (shapefiles, timezone, especies, parametros de analise "NAO ALTERAR") e'
## identico -- mesmo parque, mesma metodologia.
##

##
## Project inputs (inside folder inputs/)
##

project_ref <- "Zarafshan WPP"

## Shapefiles fornecidos pelo Paulo -- nomes exatos das colunas de ID
## confirmados por ele (ver wtg_source_id_col/idf_source_id_col abaixo).
wtg_filename <- "Masdar_wtg_positions_utm41N.txt.shp"
idf_filename <- "identiflight.shp"

## Nome da coluna de ID no shapefile de origem -- ver normalizacao em
## run_incident_zrshan.R (mesma logica de IDF_analysis.R secção 0, com um
## ajuste extra para o formato "IDF-1" do parque, ver esse ficheiro)
wtg_source_id_col <- "ID"     # turbina de incidente: "T94"
idf_source_id_col <- "Name"   # unidades no formato "IDF-1", "IDF-2", ... "IDF-22", ...

## Ainda nao existe matriz manual turbina<->IDF para este parque -- aponta
## para um ficheiro que nao existe ainda de propósito: a logica de secção 0
## (turbine_idf_coverage.R) já trata "ficheiro nao encontrado" de forma
## graciosa, computando so' a matriz GEOMETRICA (buffer turbina<->IDF) e
## gravando-a em outputs/.../turbine_idf_coverage.xlsx.
turbine_idf_matrix_filename <- "IDF_Coverage_Matrix_ZRF.xlsx"

proj_lat      <- 41.58115
proj_lon      <- 64.39011
proj_timezone <- "Asia/Samarkand" # Uzbequistao -- mesmo fuso do BSH/DGY, sem DST

## EPSG 32641 = WGS84 / UTM zone 41N -- mesma zona do BSH (64.67E), a
## longitude do Zarafshan (64.39E) cai na mesma zona.
crs_projection_plannar <- 32641


##
## Raw databases (tracks, curtailments, SCADA, heartbeats)
## Pasta UNICA e dedicada a este parque (ao contrario de BSH/DGY, que
## partilham pasta) -- por isso, ao contrario dessas, NAO se define
## databases_dir_alt nem farm_pattern.
##

databases_dir <- "G:/O meu disco/datasets/idf_zrfshan"

## Identificador curto -- usado em cache/ e outputs/AAAAMMDD_<farm_code>/.
## MESMO farm_code do T35 -- cache partilhada entre incidentes deste
## parque (tracks/curtailments/SCADA/heartbeats sao os mesmos dados
## brutos, so' a janela/turbina de interesse e' que muda por incidente).
farm_code <- "ZRSHAN"

trackreport_pattern  <- "TrackReport_"      # ex: TrackReport_20260201_....csv
curtailments_pattern <- "Curtailments_"     # ex: Curtailments_20260201_....xlsx
scada_pattern        <- "SCADA_.+csv"       # ex: SCADA_20260201_....csv
heartbeats_pattern   <- "Heartbeats_.+csv"  # ex: Heartbeats_20260201_....csv

## So as unidades IDF de interesse para ESTE incidente (T94, nao T35) --
## usadas como fallback_idf_units em summarise_fatality_windows() (R/
## fatality_window_analysis.R), no calendario de disponibilidade e para
## restringir a reconciliacao de tracks candidatos (R/track_harmonization.R).
##
## A CONFIRMAR (Paulo/Claude, 2026-10): os ROTULOS "IDF<NN>" (60, 53, 8,
## 66) foram confirmados por calculo geometrico (buffers de 1000m,
## cobertura >= 20% do buffer da turbina T94 -- ver
## check_turbine_idf_coverage_ZRF.R, corrido com turbine_id <- "T94").
## Os codigos BRUTOS ("GW<turbina que aloja a unidade>-<numero>", formato
## confirmado para o T35 em "GW32-22" etc.) AINDA NAO estao confirmados
## para estas 4 unidades -- esse codigo so' existe nos ficheiros brutos de
## heartbeats (instance_name), nao no shapefile, por isso
## check_turbine_idf_coverage_ZRF.R (so' le os 2 shapefiles) nao o pode
## calcular. ANTES de correr o relatorio real: le os heartbeats brutos
## (ex: unique(heartb_dt_unfilt$idf) depois de um 1o load_or_read_cache(),
## ou abre um ficheiro Heartbeats_*.csv diretamente) e procura os
## instance_name que terminam em "-60", "-53", "-8", "-66" -- substitui os
## placeholders "GW??-60" etc. abaixo pelos valores reais. Os 2 checks
## AVISO ja' existentes em run_incident_zrshan.R (secção "2. Turbine/IDF
## coverage") apanham um mismatch se isto ficar por corrigir, mas nao
## travam a corrida -- confirmar ANTES, nao depois.
heartbeat_idf_units <- c("GW??-60", "GW??-53", "GW??-8", "GW??-66")
names(heartbeat_idf_units) <- c("IDF60", "IDF53", "IDF08", "IDF66")


##
## Timeframe for analysis/reporting period
##
## Alargado face ao T35 (que so' ia ate' 2026-08-15) para cobrir o
## incidente (2026-10-01) -- mesma convencao de "limite futuro largo" ja'
## usada em userSettings_BSH.R/userSettings_DGY.R (scada_end), para nao
## ter de voltar a alargar manualmente a cada novo incidente.
##

ini <- as.POSIXct('2026-01-01 00:00:00', tz = proj_timezone)
end <- as.POSIXct('2026-12-31 23:59:59', tz = proj_timezone)

scada_ini <- as.POSIXct('2026-01-01 00:00:00', tz = proj_timezone)
scada_end <- as.POSIXct('2026-12-31 23:59:59', tz = proj_timezone)

## So a turbina do incidente -- e' o unico foco deste relatorio
turbinas_scada <- c("T94")


##
## Project's species groups -- mesmo vocabulario do BSH/DGY (confirmado
## pelo Paulo, 2026-08: exportacao do mesmo portal IdentiFlight)
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
## Analysis parameters -- copiados VERBATIM de userSettings_BSH.R
## (metodologia farm-independente, "NAO ALTERAR PARA GARANTIR
## COMPARABILIDADE" entre parques/relatorios)
##

idf_op_detection_range <- 1000 # em metros; raio de deteção operacional do IDF, usado no buffer geometrico turbina<->IDF

## -- 3D Coverage (topography-corrected, DEM) --
dem_filename <- "dem_copernicus_30m_zrfshan.tif"
wtg_3d_coverage <- c("T94")

## Restantes parametros do cilindro/malha -- copiados VERBATIM de
## userSettings_BSH.R (metodologia farm-independente)
coverage_cylinder_height       <- 1000 # em metros
coverage_cylinder_wider_radius <- 1100 # em metros
coverage_cylinder_inner_radius <- 600  # em metros
coverage_mesh_step_xy          <- 50   # em metros; resolucao horizontal da malha 3D
coverage_mesh_step_z           <- 50   # em metros; resolucao vertical da malha 3D
coverage_prox_thresh_m         <- 50   # em metros; distancia 3D maxima ave-no da malha para considerar "covered"
coverage_min_sample_records    <- 500000

heartbeat_interval_min    <- 30
heartbeat_offline_gap_min <- 60

safe_shutdown_rpm <- 1
curtailment_start_end_gap_sec <- 10
curtailment_max_next_gap_sec   <- 20
curtailment_drop_pct_threshold <- 0.10
curtailment_window_sec         <- 90
curtailment_window_max_gap_sec <- 15

shutdown_time_thresholds <- c(2, 1, 0)
shutdown_time_low_cut    <- 40
shutdown_time_high_cut   <- 50
shutdown_time_buffer_sec <- 60

curtailment_latency_decline_pct <- 0.10
curtailment_cutin_rpm           <- 3
response_timeline_unit          <- "week"

curtailment_example_n                 <- 3
curtailment_example_window_before_min <- 1
curtailment_example_window_after_min  <- 3

## Janela (antes/depois da ULTIMA posicao registada do track) para os 2
## exemplos de RPM da secção "Top Candidate Tracks" do relatorio de
## incidente -- R/fatality_track_investigation.R, plot_fatality_track_rpm()
fatality_example_window_before_min <- 3
fatality_example_window_after_min  <- 3

## em metros; limiar de proximidade a turbina para identificar candidatos a
## colisao (rotor-swept zone, distancia horizontal/2D)
track_proximity_threshold_m <- 100

## em metros AGL; altura abaixo da qual o sistema despoleta curtailment
curtailment_trigger_height_m <- 300

## dias APOS o incidente a comparar com a janela pre-incidente, na
## abundancia (min individuals) da especie -- ver CLAUDE.md
fatality_post_incident_days <- 3

min_individuals_bin_min      <- 2
min_individuals_merge_dist_m <- 200

## Limiares de harmonizacao de tracks (R/track_harmonization.R) -- mesmos
## valores usados na exploracao BSH (2026-08), ainda genericos/nao
## validados para este parque especificamente.
harmonization_handoff_time_window_sec <- 30
harmonization_handoff_max_dist_m      <- 50
harmonization_duplicate_max_median_dist_m <- 300
harmonization_duplicate_max_spread_m      <- 50
harmonization_duplicate_min_overlap_frac  <- 0.8
harmonization_duplicate_min_overlap_sec   <- 10


##
## Incident details -- turbina/especie/data/janela deste relatorio
##
## janela de 15 dias ANTES do dia de registo do evento (pedido do Paulo,
## 2026-10) -- mais larga que os 8 dias por omissao usados no BSH/DGY (ver
## CLAUDE.md: "pode precisar de ser mais longa para outros casos, ja' que a
## frequencia de deteção de carcaças varia").
##

fatality_incidents <- data.table::data.table(
  incident_id   = "ZRF_Oct2026",
  turbine       = "T94",
  species       = "Egyptian-Vulture",
  incident_date = as.Date("2026-10-01"),
  days_before   = 15
)
