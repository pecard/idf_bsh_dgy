##
## User settings
##
## As seccoes "## N. ... ----" abaixo espelham a numeracao das seccoes de
## analise em IDF_analysis.R, para facilitar encontrar que parametro
## alimenta que parte do script.
##


##
## Project inputs (inside folder inputs/)
##

project_ref <- "Bash WPP"

wtg_filename                <- "Bash_Turbines_UTM.shp"  # in .shp

idf_filename                <- "Bash_IDF_coord.shp"                         # in .shp
tier_start_scheme_filename  <- "Bash_tier_scheme_20260215.xlsx"             # em .xlsx
tier3_start_scheme_filename <- "Bash_tier_3_commissioning_20260215.xlsx"    # em .xlsx

proj_lat      <- 40.65
proj_lon      <- 64.67
proj_timezone <- "Asia/Samarkand"

crs_projection_plannar <- 32641 # codigo CRS da projecao planar a usar


##
## Raw databases (tracks, curtailments, SCADA, heartbeats)
## Ficam FORA do repositorio, numa pasta acima da pasta do projeto.
## Os ficheiros sao identificados por padrao no nome, nao por nome fixo.
##

databases_dir <- "G:/O meu disco/Programacao/r/Bsh_Dgy_WPP/data-raw"
databases_dir_alt <- "//192.168.1.11/DadosBrutos(T2)/Lisboa/08_Tecnica/2025/T05-2025_BSH_DGY/IDF_PortalData/BSH"


trackreport_pattern  <- "TrackReport_" #"TrackReport.+csv"          # ex: TrackReport_20260201_....csv
curtailments_pattern <- "curtail_orders|Curtailments" # ex: Curtailments_20260201_....xlsx
scada_pattern        <- "SCADA_.+csv"               # ex: SCADA_20260201_....csv
heartbeats_pattern   <- "Bash_Heartbeats.+csv"          # ex: Heartbeats_20260201_....csv
heartbeat_idf_units <- c("BSH55-09" ,"BSH53-10", "BSH52-11", "BSH64-04", "BSH62-05", "BSH61-06")

##
## Timeframe for analysis/reporting period
##

ini <- as.POSIXct('2025-10-01 00:00:00', tz = proj_timezone)
end <- as.POSIXct('2026-06-30 23:59:59', tz = proj_timezone)


##
## Project's species groups
## Usadas nas seccoes 3.3 (curtailments por especie) e 5 (biologico)
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
  'White-Tailed-Eagle'
)

nonprioritysp <- c(
  'Kestrel',
  'Booted-Eagle',
  'Common-Buzzard',
  'Greater-Spotted-Eagle',
  'Honey-Buzzard',
  'Long-Legged-Buzzard',
  'Merlin',
  'Osprey',
  'Peregrine-Falcon',
  'Red-Or-Black-Kite',
  'Short-Toed-Snake-Eagle',
  'Sparrow-Hawk',
  'Harrier',
  'Eagle-Unknown'
)

othersp <- c(
  'Common-Crane',
  'White-Stork',
  'Black-Stork',
  'Raven',
  'Pigeon',
  'Grey-Heron',
  'Cormorant',
  'Great-Egret'
)


##
## Analysis parameters, por seccao de IDF_analysis.R -- NAO ALTERAR PARA GARANTIR COMPARABILIDADE
##


## -- 1. Performance --
## (sem parametros especificos definidos aqui, para ja)


## -- 3.1. System Availability (heartbeats) --

heartbeat_interval_min    <- 30 # intervalo esperado entre heartbeats de cada unidade IDF
heartbeat_offline_gap_min <- 60 # gap (min) a partir do qual se considera a unidade offline (perdeu >=2 heartbeats seguidos)
idf_availability_top_n    <- 12L # nº de unidades (com mais tempo offline) mostradas nos graficos de calendario/frequencia


## -- 3.4. Short-track curtailment --

shorttrack_min_points <- 6   # threshold de pontos dos tracks abaixo do qual se considera short-tracks
shorttrack_eval_range <- 300 # em metros; distancia ate qual se considera relevante avaliar short-tracks que despoletaram curtailments


## -- 3.5. Curtailment response assessment (roll join com SCADA) --

# Incluir time range e turbinas para o qual temos dados de SCADA, mesmo que no intervalo tenham "buracos" sem info
scada_ini <- as.POSIXct('2025-10-15 00:00:00', tz = proj_timezone)
scada_end <- as.POSIXct('2026-06-30 23:59:59', tz = proj_timezone)

turbinas_scada <- c('BSH54')

safe_shutdown_rpm <- 1 # threshold de rpm a considerar para uma safe passage; curtailment considerado feito com sucesso quando rpm da turbina < safe_shutdown_rpm

# tolerancia apertada (segundos) para considerar fiavel o RPM de baseline no
# start/end de um curtailment -- usada em assess_curtailment_response() (3.5)
# e reutilizada em time_to_rpm_thresholds() (3.6)
curtailment_start_end_gap_sec <- 2

curtailment_max_next_gap_sec   <- 20   # tolerancia (s) para aceitar a leitura SCADA seguinte como "resposta imediata"
curtailment_drop_pct_threshold <- 0.10 # queda minima (%) nessa leitura seguinte para considerar resposta imediata

curtailment_window_sec         <- 90 # janela larga (s) de monitorizacao usada na vista complementar (classify_curtailment_response)
curtailment_window_max_gap_sec <- 15 # tolerancia (s) de "nearest" usada nessa vista complementar


## -- 3.6. Shutdown time (tempo ate atingir limiares de RPM) --

shutdown_time_thresholds <- c(2, 1, 0) # limiares de rpm a avaliar (rpm)
shutdown_time_low_cut    <- 40 # segundos
shutdown_time_high_cut   <- 50 # segundos


## -- 3.7. Safe distance (metodologia KNE) --

safe_dist_rpm_threshold  <- 2   # rpm; "tempo ate parar em seguranca" usado na formula do KNE (distinto de safe_shutdown_rpm)
safe_dist_speed_trim_q   <- 0.95 # percentil de corte de outliers na velocidade de voo do track
safe_dist_reference_line_m <- 600 # em metros; linha de referencia nos plots -- POR CONFIRMAR/AJUSTAR

# rpm; abaixo deste valor no momento do disparo, considera-se que a turbina ja
# estava a abrandar/recuperar de outro curtailment (turbine_state =
# "already_slowing"), distinto do cenario mais gravoso de velocidade normal de
# operacao ("full_speed") -- operacional normal ~10-12 rpm
safe_dist_already_slowing_rpm <- 6


## -- 4. Coverage --

idf_op_detection_range <- 1000 # em metros; distancia de deteção operational do IDF a considerar na análise

coverage_cylinder_height       <- 1000 # em metros; 3D coverage cylinder - height
coverage_cylinder_wider_radius <- 1100 # em metros; 3D coverage cylinder - wider radius
coverage_cylinder_inner_radius <- 600  # em metros; 3D coverage cylinder - inner radius

## -- 4.2. WTG coverage 3D com topografia (DEM) --

# GeoTIFF cobrindo toda a area do parque (ex: Copernicus GLO-30), descarregado
# manualmente e colocado em databases_dir -- o script faz o crop/mask ao raio
# de cada turbina automaticamente, nao e preciso pre-recortar o ficheiro.
dem_filename <- "Bash_DEM_copernicus30m.tif"

# Subconjunto de turbinas (nomes tal como aparecem na coluna InternalNa do
# shapefile wtg) para a analise 3D completa -- e cara (DEM + malha + KD-tree
# por turbina), por isso NAO corre para todas as turbinas por omissao.
# Nomes que nao existirem no shapefile geram um aviso explicito, nao sao
# ignorados em silencio (ver run_coverage_3d_all_turbines()).
wtg_3d_coverage <- c('BSH54')

coverage_mesh_step_xy   <- 50 # em metros; resolucao horizontal da malha 3D
coverage_mesh_step_z    <- 50 # em metros; resolucao vertical da malha 3D
coverage_prox_thresh_m  <- 50 # em metros; distancia 3D maxima ave-no da malha para considerar "covered"

# Bandas de risco (altura relativa a turbina, em metros) para a malha 3D com
# topografia -- AINDA POR DECIDIR: por agora usa os breaks/labels por omissao
# da funcao (c(200), c("at risk","above")), a rever depois para alinhar (ou
# nao) com riskHeight_lower/upper acima (usado no coverage_3D.R antigo, sem
# topografia).
# coverage_3d_risk_band_breaks <- c(200)
# coverage_3d_risk_band_labels <- c("at risk", "above")


## -- 5.3. Risk per species --

riskHeight_lower <- 50  # in meters
riskHeight_upper <- 250 # in meters
