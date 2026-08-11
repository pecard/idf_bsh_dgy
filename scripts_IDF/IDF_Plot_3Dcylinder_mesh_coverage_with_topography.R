mesh_air_plot <- copy(mesh_air)
mesh_air_plot[, z_plot := z_rel_turbine]

mesh_air_plot[, .N, by = covered]

mesh_air_plot[covered == TRUE, .N]

mesh_air_plot[covered == TRUE, .N, by = risk_band]

summary(mesh_air_plot[covered == TRUE]$z_plot)
summary(mesh_air_plot[covered == TRUE]$hits)

mesh_air_plot[, risk_band := as.character(risk_band)]
mesh_air_plot[is.na(risk_band), risk_band := "unclassified"]

mesh_air_cov <- mesh_air_plot[covered == TRUE]

wtg_symbol <- data.table(
  name = "WTG",
  x = 0,
  y = 0,
  z = 0
)

#idf_10 40.6118291,64.6438637
#idf_09 40.60518781,64.64990611

idf_sf <- st_as_sf(
  data.frame(name = "IDF10", lon = 64.6438637, lat = 40.6118291),
  coords = c("lon", "lat"),
  crs = 4326
)

idf_local <- st_transform(idf_sf, crs_local)
idf_xy <- st_coordinates(idf_local)

idf_x <- idf_xy[1, 1]
idf_y <- idf_xy[1, 2]
idf_z <- 0

# WTG tower ----
wtg_line <- data.table(
  x = c(0, 0),
  y = c(0, 0),
  z = c(0, 90)
)

# WTG nacelle point ----
wtg_nacelle <- data.table(
  x = 0,
  y = 0,
  z = 90
)

p_debugv <-
  plot_ly() %>%
  
  add_surface(
    x = xs_surf,
    y = ys_surf,
    z = Zterrain_rel,
    opacity = 0.7,
    showscale = FALSE,
    surfacecolor = matrix("black", nrow = nrow(Zterrain_rel), ncol = ncol(Zterrain_rel)),
    name = "Terrain"
  ) %>%
  
  # add_markers(
  #   data = mesh_air_cov,
  #   x = ~x,
  #   y = ~y,
  #   z = ~z_plot,
  #   type = "scatter3d",
  #   mode = "markers",
  #   color = ~risk_band,
  #   colors = c(
  #     "at risk" = "blue",
  #     "above" = "green",
  #     "unclassified" = "orange"
  #   ),
  #   marker = list(
  #     size = 1.5,
  #     opacity = .8
  #   ),
  #   name = "Air mesh — covered"
  # ) %>%
  
  add_markers(
    data = mesh_air_plot[covered == FALSE],
    x = ~x,
    y = ~y,
    z = ~z_plot,
    type = "scatter3d",
    mode = "markers",
    marker = list(
      size = 1.5,
      opacity = 0.51,
      color = "orange"
    ),
    name = "Air mesh — not covered"
  ) %>%
  
  # add_markers(
  #   data = mesh_air_plot[covered == TRUE],
  #   x = ~x,
  #   y = ~y,
  #   z = ~z_plot,
  #   type = "scatter3d",
  #   mode = "markers",
  #   color = ~risk_band,
  #   colors = c(
  #     "at risk" = "blue",
  #     "above" = "green"
  #   ),
  #   marker = list(
  #     size = 2.4,
  #     opacity = 0.85
  #   ),
  #   name = "Air mesh — covered"
  # ) %>%
  
  # add_markers(
  #   data = mesh_air_plot[covered == TRUE],
  #   x = ~x,
  #   y = ~y,
  #   z = ~z_plot,
  #   type = "scatter3d",
  #   mode = "markers",
  #   marker = list(
  #     size = 2,
  #     opacity = 0.8,
  #     color = "orange"
  #   ),
  #   name = "Air mesh — covered"
  # ) %>%
  
  # WTG tower
  add_trace(
    data = wtg_line,
    x = ~x,
    y = ~y,
    z = ~z,
    type = "scatter3d",
    mode = "lines",
    line = list(
      color = "red",
      width = 10
    ),
    name = "WTG tower"
  ) %>%
  
  # WTG nacelle point
  add_markers(
    data = wtg_nacelle,
    x = ~x,
    y = ~y,
    z = ~z,
    type = "scatter3d",
    mode = "markers",
    marker = list(
      size = 5,
      color = "darkred"
    ),
    name = "WTG nacelle"
  ) %>%
  
  # Cilynder 
  add_surface(
    x = Xc,
    y = Yc,
    z = Zc,
    opacity = 0.06,
    showscale = FALSE,
    surfacecolor = matrix("lightgrey", nrow = nrow(Zc), ncol = ncol(Zc)),
    name = "Cylinder boundary",
    showlegend = FALSE
  ) %>%
  
  layout(
    title = "Debug view — full air mesh and covered points",
    legend = list(
      x = 0.02,
      y = 0.95,
      xanchor = "left",
      yanchor = "top",
      bgcolor = "rgba(255,255,255,0.65)",
      bordercolor = "rgba(0,0,0,0.2)",
      borderwidth = 1
    ),
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
        range = c(z_min_plot, z_max_plot),
        autorange = FALSE
      ),
      
      aspectmode = "manual",
      
      aspectratio = list(
        x = 1,
        y = 1,
        z = (z_max_plot - z_min_plot) / (2 * radius)
      ),
      
      camera = camera_from_bearing(
        bearing_deg = 135,
        distance = 2.5,
        z = 0.7
      )
    )
  )

p_debug
