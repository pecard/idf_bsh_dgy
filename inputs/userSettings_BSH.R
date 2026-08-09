##
## User settings
##


##
## Project inputs (inside folder inputs/)
##

project_ref <- "Bash WPP"

wtg_filename                <- "Bash_Turbines_UTM.shp"  # in .shp

idf_filename                <- "Bash_IDF_coord.shp"                           # in .shp
tier_start_scheme_filename  <- "Bash_tier_scheme_20260215.xlsx"             # em .xlsx
tier3_start_scheme_filename <- "Bash_tier_3_commissioning_20260215.xlsx"    # em .xlsx

proj_lat      <- 40.65 # 40.656620,64.672409
proj_lon      <- 64.67
proj_timezone <- "Asia/Samarkand"

crs_projection_plannar <- 32641 # codigo CRS da projecao planar a usar


##
## Raw databases (tracks, curtailments, SCADA, heartbeats)
## Ficam FORA do repositorio, numa pasta acima da pasta do projeto.
## Os ficheiros sao identificados por padrao no nome, nao por nome fixo.
##

databases_dir <- "G:/O meu disco/Programacao/r/Bsh_Dgy_WPP/data-raw"

trackreport_pattern  <- "_Track_" #"TrackReport.+csv"          # ex: TrackReport_20260201_....csv
curtailments_pattern <- "curtail_orders|Curtailments" # ex: Curtailments_20260201_....xlsx
scada_pattern        <- "SCADA_.+csv"               # ex: SCADA_20260201_....csv
heartbeats_pattern   <- "Bash_Heartbeats.+csv"          # ex: Heartbeats_20260201_....csv
heartbeat_idf_units <- c("BSH55-09" ,"BSH53-10", "BSH52-11")

##
## Timeframe for analysis/reporting period
##

ini <- as.POSIXct('2025-10-15 00:00:00', tz = 'UTC')
end <- as.POSIXct('2026-06-30 23:59:59', tz = 'UTC')


##
## SCADA-related analysis - e.g. Shutdown time, Missed curtailments, Delayed curtailments
##

# Incluir time range e turbinas para o qual temos dados de SCADA, mesmo que no intervalo tenham "buracos" sem info
scada_ini <- as.POSIXct('2025-10-15 00:00:00', tz = 'UTC')
scada_end <- as.POSIXct('2026-06-30 23:59:59', tz = 'UTC')

turbinas_scada <- c('BSH54')


##
## Risk Height
##

riskHeight_lower <- 50  # in meters
riskHeight_upper <- 250 # in meters


##
## Project's species groups
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
## Analysis related settings - NAO ALTERAR PARA GARANTIR COMPARABILIDADE
##

## Short-tracks
shorttrack_min_points <- 6   # threshold de pontos dos tracks abaixo do qual se considera short-tracks
shorttrack_eval_range <- 300 # em metros; distancia ate qual se considera relevante avaliar short-tracks que despoletaram curtailments

## Coverage
idf_op_detection_range <- 1000 # em metros; distancia de deteção operational do IDF a considerar na análise

## 3D Coverage
coverage_cylinder_height       <- 1000 # em metros; 3D coverage cylinder - height
coverage_cylinder_wider_radius <- 1000 # em metros; 3D coverage cylinder - wider radius
coverage_cylinder_inner_radius <- 600  # em metros; 3D coverage cylinder - inner radius

# Falta mais parametros - estão diretamente no script:
# wtg_cylind_radius, wtg_cylind_inner_radius, mesh_cell_size, cell_prox_thresh_m

## Shutdown time, Missed curtailments & Delayed curtailment
safe_shutdown_rpm <- 1 # threshold de rpm a considerar para uma safe passage; curtailment considerado feito com sucesso quando rpm da turbina < safe_shutdown_rpm

## Heartbeats / disponibilidade (availability) das unidades IDF
heartbeat_interval_min    <- 30 # intervalo esperado entre heartbeats de cada unidade IDF
heartbeat_offline_gap_min <- 60 # gap (min) a partir do qual se considera a unidade offline (perdeu >=2 heartbeats seguidos)
