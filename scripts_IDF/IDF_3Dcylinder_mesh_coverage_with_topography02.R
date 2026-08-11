# Packages ----
pacman::p_load(
  data.table,
  sf,
  terra,
  geosphere,
  RANN,
  plotly,
  here
)

# Inputs ----
wtg_id  <- "BSH54"
wtg_lat <- 40.610979
wtg_lon <- 64.646314

radius <- 1000
cyl_height <- 1000
step_xy <- 50
step_z  <- 50
prox_thresh_m <- 50

dem_file <- here::here("data-raw", "dem_copernicus30m.tif")

# track_wtg must already exist with:
# TrackId, DateTimeStamp, lat, lon, height

# CRS local centred on turbine ----
crs_local <- sprintf(
  "+proj=aeqd +lat_0=%f +lon_0=%f +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs",
  wtg_lat, wtg_lon
)

wtg_sf <- st_as_sf(
  data.frame(id = wtg_id, lon = wtg_lon, lat = wtg_lat),
  coords = c("lon", "lat"),
  crs = 4326
)

wtg_local <- st_transform(wtg_sf, crs_local)

# DEM processing ----
dem <- terra::rast(dem_file)

wtg_buffer_local <- st_buffer(wtg_local, dist = radius + 200)
wtg_buffer_demcrs <- st_transform(wtg_buffer_local, st_crs(dem)$wkt)

dem_crop <- terra::crop(dem, terra::vect(wtg_buffer_demcrs))
dem_crop <- terra::mask(dem_crop, terra::vect(wtg_buffer_demcrs))

dem_local <- terra::project(dem_crop, crs_local)

wtg_elev <- terra::extract(dem_local, terra::vect(wtg_local))[[2]]
wtg_elev <- as.numeric(wtg_elev)

# Build terrain-corrected 3D mesh ----
xs <- seq(-radius, radius, by = step_xy)
ys <- seq(-radius, radius, by = step_xy)

terrain_min_rel <- floor((min(mesh_xy$terrain_elev, na.rm = TRUE) - wtg_elev) / step_z) * step_z

zs <- seq(terrain_min_rel, cyl_height, by = step_z)
zs <- seq(0, cyl_height, by = step_z)

grid_xy <- CJ(x = xs, y = ys)
grid_xy <- grid_xy[(x^2 + y^2) <= radius^2]

mesh <- grid_xy[, .(z_rel_turbine = zs), by = .(x, y)]
mesh[, z_abs := wtg_elev + z_rel_turbine]

# Extract terrain elevation for each XY node ----
mesh_xy <- unique(mesh[, .(x, y)])

mesh_xy_sf <- st_as_sf(
  mesh_xy,
  coords = c("x", "y"),
  crs = crs_local
)

terrain_vals <- terra::extract(dem_local, terra::vect(mesh_xy_sf))
mesh_xy[, terrain_elev := as.numeric(terrain_vals[[2]])]

mesh <- merge(mesh, mesh_xy, by = c("x", "y"), all.x = TRUE)

# Classify mesh as terrain or air ----
mesh[, vertical_clearance := z_abs - terrain_elev]

mesh[, medium := fifelse(
  is.na(terrain_elev),
  NA_character_,
  fifelse(vertical_clearance <= 0, "terrain", "air")
)]

# Risk bands relative to turbine elevation ----
mesh[, risk_band := fcase(
  z_rel_turbine < 200, "at risk",
  z_rel_turbine >= 200 & z_rel_turbine <= cyl_height, "above",
  default = NA_character_
)]

mesh[, risk_band := factor(risk_band, levels = c("at risk", "above"))]

mesh_air <- mesh[medium == "air" & !is.na(risk_band)]
mesh_terrain <- mesh[medium == "terrain"]

# Prepare bird data in local CRS ----
track_wtg <- copy(track_wtg)

track_wtg_sf <- st_as_sf(
  track_wtg,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

track_wtg_m <- st_transform(track_wtg_sf, crs_local)
coords_bird <- st_coordinates(track_wtg_m)

track_wtg[, `:=`(
  x = coords_bird[, 1],
  y = coords_bird[, 2]
)]

# Extract terrain elevation under each bird point ----
bird_terrain_vals <- terra::extract(
  dem_local,
  terra::vect(track_wtg_m)
)

track_wtg[, terrain_elev := as.numeric(bird_terrain_vals[[2]])]

# Convert bird height above local ground to height relative to turbine ground ----
track_wtg[, z_abs := terrain_elev + height]
track_wtg[, z_rel_turbine := z_abs - wtg_elev]

# Keep only valid bird points inside the terrain-corrected cylinder ----
track_wtg <- track_wtg[
  !is.na(x) & !is.na(y) &
    !is.na(z_rel_turbine) &
    (x^2 + y^2) <= radius^2 &
    z_rel_turbine >= 0 &
    z_rel_turbine <= cyl_height
]

# KD-tree: overlay bird data with air-only mesh ----
M_air <- as.matrix(mesh_air[, .(x, y, z_rel_turbine)])
B <- as.matrix(track_wtg[, .(x, y, z_rel_turbine)])

nn <- RANN::nn2(
  data = M_air,
  query = B,
  k = 1
)

idx_air <- as.integer(nn$nn.idx[, 1])
dist3d  <- as.numeric(nn$nn.dists[, 1])

idx_air_use <- idx_air[dist3d <= prox_thresh_m]

counts_air <- tabulate(idx_air_use, nbins = nrow(mesh_air))

mesh_air[, hits := counts_air]
mesh_air[, covered := hits > 0L]

mesh_cov <- mesh_air[covered == TRUE]

# Metrics ----
n_total <- nrow(mesh_air)
n_cov   <- nrow(mesh_cov)

n_atrisk <- mesh_air[risk_band == "at risk", .N]
n_above  <- mesh_air[risk_band == "above", .N]

c_atrisk <- mesh_air[covered == TRUE & risk_band == "at risk", .N]
c_above  <- mesh_air[covered == TRUE & risk_band == "above", .N]

pct <- function(num, den) ifelse(den > 0, 100 * num / den, NA_real_)

p_any    <- pct(n_cov, n_total)
p_atrisk <- pct(c_atrisk, n_atrisk)
p_above  <- pct(c_above, n_above)

cat(sprintf(
  "Terrain-corrected WTG mesh coverage\nWTG: %s\nBird records: %d\nCovered air mesh: %d / %d (%.1f%%)\nAt risk: %.1f%%\nAbove: %.1f%%\n",
  wtg_id, nrow(track_wtg), n_cov, n_total, p_any, p_atrisk, p_above
))

# Terrain surface for Plotly ----
terrain_surface <- dcast(
  mesh_xy,
  y ~ x,
  value.var = "terrain_elev"
)

ys_surf <- terrain_surface$y
xs_surf <- as.numeric(names(terrain_surface)[-1])

Zterrain_abs <- as.matrix(terrain_surface[, -1])
Zterrain_rel <- Zterrain_abs - wtg_elev

# Cylinder boundary ----
# Cylinder boundary covering full DEM-corrected vertical range ----
z_min_cyl <- floor(min(Zterrain_rel, mesh_air$z_rel_turbine, na.rm = TRUE) / step_z) * step_z
z_max_cyl <- cyl_height

theta <- seq(0, 2 * pi, length.out = 180)
zv <- seq(z_min_cyl, z_max_cyl, length.out = 80)

Theta <- matrix(rep(theta, each = length(zv)), nrow = length(zv))
Zc <- matrix(rep(zv, times = length(theta)), nrow = length(zv))

Xc <- radius * cos(Theta)
Yc <- radius * sin(Theta)

# Cylinder boundary covering full DEM-corrected vertical range ----
z_pad_lower <- 50

z_min_cyl <- floor(
  min(Zterrain_rel, mesh_air$z_rel_turbine, na.rm = TRUE) / step_z
) * step_z - z_pad_lower

z_max_cyl <- cyl_height

theta <- seq(0, 2 * pi, length.out = 180)
zv <- seq(z_min_cyl, z_max_cyl, length.out = 80)

Theta <- matrix(rep(theta, each = length(zv)), nrow = length(zv))
Zc <- matrix(rep(zv, times = length(theta)), nrow = length(zv))

Xc <- radius * cos(Theta)
Yc <- radius * sin(Theta)

z_min_plot <- z_min_cyl
z_max_plot <- z_max_cyl

# Camera and bearing helpers ----
camera_from_bearing <- function(
    bearing_deg = 135,
    distance = 2.5,
    z = 0.7
) {
  theta <- bearing_deg * pi / 180
  
  list(
    eye = list(
      x = distance * sin(theta),
      y = distance * cos(theta),
      z = z
    ),
    center = list(x = 0, y = 0, z = 0),
    up = list(x = 0, y = 0, z = 1),
    projection = list(type = "orthographic")
  )
}

make_bearing_line <- function(
    bearing_from_deg = 135,
    radius = 1000,
    z = 980
) {
  b_from <- bearing_from_deg %% 360
  b_to <- (b_from + 180) %% 360
  
  bearing_to_xy <- function(bearing_deg, radius) {
    theta <- bearing_deg * pi / 180
    c(
      x = radius * sin(theta),
      y = radius * cos(theta)
    )
  }
  
  bearing_label <- function(bearing_deg) {
    dirs <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N")
    dirs[round((bearing_deg %% 360) / 45) + 1]
  }
  
  p_from <- bearing_to_xy(b_from, radius)
  p_to <- bearing_to_xy(b_to, radius)
  
  data.table(
    x = c(p_from["x"], p_to["x"]),
    y = c(p_from["y"], p_to["y"]),
    z = c(z, z),
    bearing = c(b_from, b_to),
    label = c(bearing_label(b_from), bearing_label(b_to))
  )
}

bearing_line <- make_bearing_line(
  bearing_from_deg = 135,
  radius = radius,
  z = cyl_height * 0.98
)

bearing_labels <- copy(bearing_line)
bearing_labels[, `:=`(
  x = x * 0.90,
  y = y * 0.90,
  z = z - 30
)]

# Plot data ----
mesh_cov[, z_plot := z_rel_turbine]
mesh_cov[, risk_band := factor(risk_band, levels = c("at risk", "above"))]

plot_title <- sprintf(
  "Terrain-corrected WTG mesh coverage<br>WTG: %s<br>Bird records: %d<br>Covered air mesh: %d / %d (%.1f%%)<br>at risk: %.1f%%<br>above: %.1f%%",
  wtg_id, nrow(track_wtg), n_cov, n_total, p_any, p_atrisk, p_above
)

z_min <- min(Zterrain_rel, na.rm = TRUE)
z_max <- cyl_height

# Plotly ----
p3dmeshcov <-
  plot_ly() %>%
  
  add_surface(
    x = xs_surf,
    y = ys_surf,
    z = Zterrain_rel,
    opacity = 0.85,
    showscale = FALSE,
    surfacecolor = matrix("black", nrow = nrow(Zterrain_rel), ncol = ncol(Zterrain_rel)),
    name = "Terrain"
  ) %>%
  
  add_markers(
    data = mesh_cov,
    x = ~x,
    y = ~y,
    z = ~z_plot,
    type = "scatter3d",
    mode = "markers",
    color = ~risk_band,
    colors = c(
      "at risk" = "blue",
      "above" = "green"
    ),
    marker = list(size = 2, opacity = 0.75),
    name = ~risk_band
  ) %>%
  
  add_surface(
    x = Xc,
    y = Yc,
    z = Zc,
    opacity = 0.08,
    showscale = FALSE,
    surfacecolor = matrix("lightgrey", nrow = nrow(Zc), ncol = ncol(Zc)),
    name = "1000 m cylinder boundary",
    showlegend = FALSE
  ) %>%
  
  add_trace(
    data = bearing_line,
    x = ~x,
    y = ~y,
    z = ~z,
    type = "scatter3d",
    mode = "lines",
    line = list(color = "black", width = 4),
    name = paste0("View axis ", bearing_line$label[1], "→", bearing_line$label[2])
  ) %>%
  
  add_trace(
    data = bearing_labels,
    x = ~x,
    y = ~y,
    z = ~z,
    type = "scatter3d",
    mode = "text",
    text = ~label,
    textposition = "middle center",
    textfont = list(size = 16, color = "black"),
    showlegend = FALSE,
    hoverinfo = "skip"
  ) %>%
  
  layout(
    title = list(
      text = plot_title,
      x = 0.05,
      y = 0.95,
      font = list(size = 12)
    ),
    legend = list(
      x = 0.02,
      y = 0.95,
      xanchor = "left",
      yanchor = "top",
      bgcolor = "rgba(255,255,255,0.65)",
      bordercolor = "rgba(0,0,0,0.2)",
      borderwidth = 1,
      itemsizing = "constant",
      font = list(size = 12)
    ),
    margin = list(l = 20, r = 20, t = 70, b = 20),
    scene = list(
      xaxis = list(
        title = "X to WTG (m)",
        range = c(-radius, radius),
        autorange = FALSE
      ),
      yaxis = list(
        title = "Y to WTG (m)",
        range = c(-radius, radius),
        autorange = FALSE
      ),
      zaxis = list(
        title = "Height relative to WTG ground (m)",
        range = c(z_min, z_max),
        autorange = FALSE
      ),
      aspectmode = "manual",
      aspectratio = list(
        x = 1,
        y = 1,
        z = (z_max - z_min) / (2 * radius)
      ),
      camera = camera_from_bearing(
        bearing_deg = 135,
        distance = 2.5,
        z = 0.7
      )
    )
  )

p3dmeshcov