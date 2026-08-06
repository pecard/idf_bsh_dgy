##
## Read Track files ----
##

trackfiles <- tibble(file = list.files(databases_dir, pattern = 'TrackReport.+csv', full.names = T))
#trackfiles$date <- ymd(str_extract(trackfiles$file, "(?<=TrackReport_).+?(?=_)"))

# Files to read from specific date
#trackfiles_toread <- filter(trackfiles, date >= ini & date <= end)$file # using "ini" #filtrar a BD depois
trackfiles_toread <- filter(trackfiles)$file

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

l <- lapply(
  trackfiles_toread, # files to read 
  fread,
  select = vars,
  header = T, 
  na.strings = "NULL", 
  stringsAsFactors = F,
  blank.lines.skip = T)

track_dt_unfilt <- rbindlist( l )



#Organizar dados
setnames(track_dt_unfilt, names(clean_names(track_dt_unfilt)))

#Remove all tracks with lat or lon == 0
zeroIDs <- track_dt_unfilt[latitude == 0 | longitude == 0, unique(track_id)]

# Remover todas as entradas desses track_ids
track_dt_unfilt <- track_dt_unfilt[!track_id %in% zeroIDs]

track_dt_unfilt[ , `:=` (count = .N), by = track_id]

setnames(track_dt_unfilt, 
         old = c(
           "date_time_stamp", 
           "latitude",
           'longitude',
           'species_type_name',
           'tower_number',
           'approx_height_above_ground_m',
           'horizontal_distance',
           "nearest_turbine_distance3d",
           'nearest_turbine3d'), 
         new = c(
           "timestamp",
           "lat",
           'lon',
           'spec',
           'idf',
           'height',
           'dist',
           'dist3d',
           'turbine'))

setorder(track_dt_unfilt, track_id, timestamp) # to use shift

track_dt_unfilt[ , `:=` (
  dist_m = sqrt((utm_x - shift(utm_x))^2 + (utm_y - shift(utm_y))^2),
  time_s = as.numeric(difftime(timestamp,
                               lag(timestamp), 
                               units = 'secs'))
), by = track_id]

track_dt_unfilt[ , `:=` (speed_ms = dist_m / time_s)]

# cols = c('track_id', 'timestamp', 'lat', 'lon', 
#          'spec', 'turbine', 'idf', 'height', 'dist', 
#          'dist_m', 'time_s', 'speed_ms')
# 
# track_dt_unfilt[ , .SD, .SDcols = cols]

track_dt_unfilt[, date_min := cut(timestamp, breaks = 'min')]

#setkeyv(track_dt_unfilt, c("track_id", "spec"))

#key(track_dt_unfilt)


#remove temporary objects
remove(l, zeroIDs, vars, trackfiles, trackfiles_toread)
