##
## Read Track Report data (IDF track/flight detections)
##
## Depende de: data.table, janitor
##

read_tracks_data <- function(databases_dir, pattern = "TrackReport.+csv") {

  files <- list.files(databases_dir, pattern = pattern, full.names = TRUE)

  vars <- c(
    'TrackId',
    'DateTimeStamp',
    'Latitude',
    'Longitude',
    'UTM_X',
    'UTM_Y',
    'SpeciesTypeName',
    'TowerNumber',
    'ApproxHeightAboveGround_m',
    'HorizontalDistance',
    'NearestTurbine3d',
    'NearestTurbineDistance3d'
  )

  dt <- rbindlist(lapply(
    files,
    fread,
    select = vars,
    header = TRUE,
    na.strings = "NULL",
    stringsAsFactors = FALSE,
    blank.lines.skip = TRUE
  ))

  setnames(dt, names(clean_names(dt)))

  # Remover tracks com coordenadas invalidas (lat/lon == 0)
  zero_ids <- dt[latitude == 0 | longitude == 0, unique(track_id)]
  dt <- dt[!track_id %in% zero_ids]

  dt[, count := .N, by = track_id] # numero de registos por track

  setnames(
    dt,
    old = c(
      "date_time_stamp",
      "latitude",
      "longitude",
      "species_type_name",
      "tower_number",
      "approx_height_above_ground_m",
      "horizontal_distance",
      "nearest_turbine_distance3d",
      "nearest_turbine3d"
    ),
    new = c(
      "timestamp",
      "lat",
      "lon",
      "spec",
      "idf",
      "height",
      "dist",
      "dist3d",
      "turbine"
    )
  )

  setorder(dt, track_id, timestamp)

  # Distancia/tempo/velocidade entre pontos consecutivos do mesmo track
  dt[, `:=`(
    dist_m = sqrt((utm_x - shift(utm_x))^2 + (utm_y - shift(utm_y))^2),
    time_s = as.numeric(difftime(timestamp, shift(timestamp), units = 'secs'))
  ), by = track_id]

  dt[, speed_ms := dist_m / time_s]

  dt[, date_min := cut(timestamp, breaks = 'min')]

  dt
}
