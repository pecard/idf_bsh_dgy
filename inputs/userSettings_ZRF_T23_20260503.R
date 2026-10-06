##
## User settings -- Zarafshan WPP (ZRSHAN), incident report
## Incidente: Egyptian Vulture, turbina T23, 03/05/2026
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
## turbinas_scada, wtg_3d_coverage, heartbeat_idf_units, end/scada_end
## (alargados para cobrir o incidente, 2026-10-01 -- o ficheiro do T35 so'
## ia ate' 2026-08-15), e a secção "Raw databases" abaixo (databases_dir,
## farm_code, farm_pattern, *_pattern -- corrigido, Paulo, 2026-10: a pasta
## dedicada "idf_zrfshan" assumida inicialmente estava errada, os brutos
## ficam na mesma pasta partilhada "data-raw" do BSH/DGY). Tudo o resto
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
wtg_source_id_col <- "ID"     # turbina de incidente: "T23"
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
## CORRIGIDO (Paulo, 2026-10): afinal os brutos NAO ficam numa pasta
## dedicada "idf_zrfshan" (assumido inicialmente, por analogia com o
## resto deste ficheiro face ao T35 -- esse pressuposto estava errado) --
## ficam na MESMA pasta partilhada "data-raw" ja' usada pelo BSH/DGY (ver
## databases_dir em userSettings_BSH.R/userSettings_DGY.R). Por isso,
## como o BSH/DGY, precisa de farm_pattern como 2a camada de filtro (ver
## list_files_multi_dir(), R/read_utils.R) -- ao contrario do que a nota
## antiga aqui dizia.
##

databases_dir <- "G:/O meu disco/Programacao/r/Bsh_Dgy_WPP/data-raw"

## farm_pattern filtra por substring "ZRF" -- 2a camada de filtro,
## redundante de proposito com "ZRF" ja' embutido em cada *_pattern
## abaixo (mesma logica de farm_pattern em userSettings_DGY.R: protege
## mesmo que um dos padroes venha a ser relaxado no futuro).
farm_pattern <- "ZRF"

## Identificador curto -- usado em cache/ e outputs/AAAAMMDD_<farm_code>/.
## MUDADO de "ZRSHAN" (userSettings_ZRF.R, incidente T35) para "ZRF" --
## codigo oficial do projeto (Paulo, 2026-10). Este incidente passa a ter
## a sua PROPRIA cache (cache/ZRF/), NAO partilhada com a cache/ZRSHAN/ do
## T35 -- essa fica intacta (construida a partir da pasta antiga, que
## pode ou nao ainda ser valida para o T35; nao mexido aqui). Sem
## diferenca pratica para esta corrida: cache/ZRF/ ainda nao existe, por
## isso le sempre os brutos da pasta certa, force_reread_cache seja TRUE
## ou FALSE.
farm_code <- "ZRF"

## Nomes confirmados pelo Paulo, 2026-10, ja' na pasta partilhada
## data-raw acima: "TrackReport_Default_ZRF_...", "Curtailments_ZRF_...",
## "SCADA_ZRF_...", "Heartbeats_ZRF_...".
trackreport_pattern  <- "TrackReport_Default_ZRF" # ex: TrackReport_Default_ZRF_20260901_....csv
curtailments_pattern <- "^Curtailments_ZRF"       # ex: Curtailments_ZRF_20260901_....xlsx -- ^ para nao apanhar um eventual "Coverage_Curtailments_ZRF.csv" (mesmo bug do BSH/DGY, 2026-09)
scada_pattern        <- "SCADA_ZRF.+csv"          # ex: SCADA_ZRF_20260901_....csv
heartbeats_pattern   <- "Heartbeats_ZRF.+csv"     # ex: Heartbeats_ZRF_20260901_....csv

## So as unidades IDF de interesse para ESTE INCIDENTE (T23) -- CORRIGIR !
## usadas como fallback_idf_units em summarise_fatality_windows() (R/
## fatality_window_analysis.R) e no calendario de disponibilidade.
##
## ROTULOS "IDF<NN>" CONFIRMADOS (Paulo/Claude, 2026-10) por calculo
## geometrico (buffers de 1000m, cobertura >= 20% do buffer da turbina
## T94 -- check_turbine_idf_coverage_ZRF.R, corrido com turbine_id <-
## "T94", apos corrigir o bug do regex guloso que truncava rotulos de 2+
## digitos -- ver run_incident_zrshan.R, secção "2. Turbine/IDF
## coverage"): IDF60 (79.5%), IDF58 (43.1%), IDF53 (31.9%), IDF66 (21.8%).
## IDF65 fica de fora (2.7%, abaixo do limiar de 20%).
##
## Codigos BRUTOS ("GW<turbina que aloja a unidade>-<numero>", formato
## confirmado para o T35 em "GW32-22" etc.) CONFIRMADOS a
## partir dos heartbeats brutos (instance_name), 2026-10.
heartbeat_idf_units <- c("GW19-13", "GW23-15", "GW27-19", "GW26-70", "GW28-20") # ALTERAR !
names(heartbeat_idf_units) <- c("IDF13", "IDF15", "IDF19", "IDF70", "IDF20")    # ALTERAR!


##
## Timeframe for analysis/reporting period
##
## -- mesma convencao de "limite futuro largo" ja'
## usada em userSettings_BSH.R/userSettings_DGY.R (scada_end), para nao
## ter de voltar a alargar manualmente a cada novo incidente.
##

ini <- as.POSIXct('2026-04-20 00:00:00', tz = proj_timezone)
end <- as.POSIXct('2026-05-03 00:00:00', tz = proj_timezone)

scada_ini <- as.POSIXct('2026-04-20 00:00:00', tz = proj_timezone)
scada_end <- as.POSIXct('2026-05-03 00:00:00', tz = proj_timezone)

## So a turbina do incidente -- e' o unico foco deste relatorio
turbinas_scada <- c("T23")


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
wtg_3d_coverage <- c("T23")

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

## em metros; raio do rotor da turbina do incidente -- usado so' para
## calcular a velocidade na ponta da pa (blade tip speed) na tabela "Top
## Candidate Tracks" do relatorio de incidente (rpm_at_identification *
## 2*pi*rotor_radius_m / 60, R/fatality_track_investigation.R), pedido do
## Paulo, 2026-10. Especifico da turbina/modelo -- confirmar se e' o mesmo
## valor usado em userSettings_ZRF_T94_20261001.R (77.5m) antes de confiar
## neste.
rotor_radius_m <- 77.5

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


##
## Incident details -- turbina/especie/data/janela deste relatorio
##
## janela de 15 dias ANTES do dia de registo do evento (pedido do Paulo,
## 2026-10) -- mais larga que os 8 dias por omissao usados no BSH/DGY (ver
## CLAUDE.md: "pode precisar de ser mais longa para outros casos, ja' que a
## frequencia de deteção de carcaças varia").
##

fatality_incidents <- data.table::data.table(
  incident_id   = "ZRF_May032026",
  turbine       = "T23",
  species       = "Egyptian-Vulture",
  incident_date = as.Date("2026-05-03"),
  days_before   = 15
)
