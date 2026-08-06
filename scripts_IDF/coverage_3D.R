##
## IDF 3D coverage analysis
##

    ##Needed packages
    packages <- c('data.table','sf','RANN','geosphere','plotly','htmlwidgets','webshot2') 
    
    ##Check and install packages that are missing + call library()
    for (p in packages) {
      if (!require(p, character.only = TRUE)) install.packages(p)
      library(p, character.only = TRUE) }

    #create subfolder
    folder_output_coverage <- file.path(folder_output,paste0("idf_3Dcoverage_c",mesh_cell_size,"r",wtg_cylind_radius))
    dir.create(folder_output_coverage, recursive = TRUE, showWarnings = FALSE)
    
    ##Needed parameters
    
    #cylinder mesh
    #mesh_cell_size <- 25 #definido no script-mae
    #wtg_cylind_height <- 1000 #definido no script-mae
    #wtg_cylind_radius <- 1000 #definido no script-mae
    #cell_prox_thresh_m <- 50 #definido no script-mae

    # ---------- Helper: build cylinder mesh ----------
    build_mesh <- function(step = mesh_cell_size, radius = wtg_cylind_radius, height = wtg_cylind_height) {
      xs <- seq(-radius, radius, by = step)
      ys <- seq(-radius, radius, by = step)
      zs <- seq(0, height,  by = step)
      
      grid <- CJ(x = xs, y = ys)
      grid <- grid[(x^2 + y^2) <= radius^2]
      mesh <- grid[, .(z = zs), by = .(x, y)]
      setcolorder(mesh, c("x","y","z"))
      
      M <- as.matrix(mesh[, .(x, y, z)])
      list(mesh = mesh, M = M)
    }
    
    test_mesh <- build_mesh(step = mesh_cell_size, radius = wtg_cylind_radius, height = wtg_cylind_height)
    
    # ---------- Main: compute coverage + exports ----------
    idf_coverage_table <- function(
        wtg,                # sf with 'ID' and point geometry (EPSG:4326)
        track_dt,           # data.table with: lon, lat, height
        step   = mesh_cell_size,
        radius = wtg_cylind_radius,
        height = wtg_cylind_height,
        z_breaks = c(0, 50, 200, wtg_cylind_height),  # mesh height bands
        prox_thresh_m = cell_prox_thresh_m,            # e.g., 50 for 3D proximity; NULL to disable
        out_dir_csv  = "mesh_csv",
        export_plots = TRUE,
        out_dir_html = "html",
        out_dir_png  ="png",
        plot_width   = wtg_cylind_radius+100,
        plot_height  = 800,
        camera_eye   = list(x = 2.0, y = 1.8, z = 1.2),   # fixed view
        title_font_size = 14,
        verbose = TRUE
    ) {
      stopifnot(inherits(wtg, "sf"))
      if (sf::st_crs(wtg)$epsg != 4326) wtg <- sf::st_transform(wtg, 4326)
      
      # Ensure output dirs
      dir.create(file.path(folder_output_coverage,out_dir_csv),  recursive = TRUE, showWarnings = FALSE)
      if (export_plots) {
        dir.create(file.path(folder_output_coverage,out_dir_html), recursive = TRUE, showWarnings = FALSE)
        dir.create(file.path(folder_output_coverage,out_dir_png),  recursive = TRUE, showWarnings = FALSE)
      }
      
      # Build mesh once
      mesh_build <- build_mesh(step = step, radius = radius, height = height)
      mesh0 <- mesh_build$mesh
      M     <- mesh_build$M
      nmesh <- nrow(mesh0)
      
      # Fixed risk band by mesh z
      mesh0[, risk_band :=
              fcase(
                z >= z_breaks[1] & z <  z_breaks[2], "below",
                z >= z_breaks[2] & z <  z_breaks[3], "at risk",
                z >= z_breaks[3] & z <= z_breaks[4], "above",
                default = NA_character_
              )]
      mesh0[, risk_band := factor(risk_band, levels = c("below","at risk","above"))]
      
      # Precompute cylinder/ground surfaces for plotting
      theta <- seq(0, 2*pi, length.out = 180)
      zv    <- seq(0, height, length.out = 50)
      Theta <- matrix(rep(theta, each = length(zv)), nrow = length(zv))
      Zc    <- matrix(rep(zv,    times = length(theta)), nrow = length(zv))
      Xc    <- radius * cos(Theta)
      Yc    <- radius * sin(Theta)
      
      xs <- seq(-radius, radius, by = step)
      ys <- seq(-radius, radius, by = step)
      Xg <- matrix(rep(xs, each = length(ys)), nrow = length(ys))
      Yg <- matrix(rep(ys, times = length(xs)), nrow = length(ys))
      R2 <- Xg^2 + Yg^2
      Zg <- matrix(0, nrow = nrow(Xg), ncol = ncol(Xg))
      Zg[R2 > radius^2] <- NA_real_
      
      # Progress bar
      nturb <- nrow(wtg)
      if (verbose) pb <- txtProgressBar(min = 0, max = nturb, style = 3)
      
      out_list <- vector("list", nturb)
      now <- function() as.numeric(Sys.time())
      
      for (i in seq_len(nturb)) {
        t0_total <- now()
        
        wtg_id <- as.character(wtg$ID[i])
        ll     <- sf::st_coordinates(wtg[i, , drop = FALSE])
        lon0   <- ll[1, 1]; lat0 <- ll[1, 2]
        
        # 1) Fast bbox prefilter in lon/lat
        buffer_m <- 200 # Adjust here if needed
        lat_deg <- (radius + buffer_m) / 111320
        lon_deg <- (radius + buffer_m) / (111320 * cos(lat0 * pi / 180))
        t0_pref <- now()
        B_bb <- track_dt[
          lat >= (lat0 - lat_deg) & lat <= (lat0 + lat_deg) &
            lon >= (lon0 - lon_deg) & lon <= (lon0 + lon_deg)
        ]
        if (nrow(B_bb)) {
          B_bb[, dist_wtg := geosphere::distGeo(cbind(lon, lat), cbind(lon0, lat0))]
          B1 <- B_bb[dist_wtg <= radius]
        } else {
          B1 <- B_bb
        }
        t_pref <- now() - t0_pref
        nrec   <- nrow(B1)
        
        covered_points <- 0L
        p_any <- p_below <- p_atrisk <- p_above <- 0
        t_proj <- t_nn <- t_count <- 0
        
        if (nrec > 0) {
          # 2) Project to local AEQD
          t0_proj <- now()
          crs_local <- sprintf(
            "+proj=aeqd +lat_0=%f +lon_0=%f +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs",
            lat0, lon0
          )
          B1_sf <- sf::st_as_sf(B1, coords = c("lon","lat"), crs = 4326, remove = FALSE)
          B1_m  <- suppressMessages(sf::st_transform(B1_sf, crs_local))
          xy    <- sf::st_coordinates(B1_m)
          B1[, `:=`(x = xy[,1], y = xy[,2], z = height)]
          B1 <- B1[(x^2 + y^2) <= radius^2 & z >= 0 & z <= height]
          t_proj <- now() - t0_proj
          
          if (nrow(B1) > 0) {
            # 3) KD-tree NN
            t0_nn <- now()
            B <- as.matrix(B1[, .(x, y, z)])
            nn   <- RANN::nn2(data = M, query = B, k = 1)
            idx  <- as.integer(nn$nn.idx[, 1])
            dist <- as.numeric(nn$nn.dists[, 1])
            if (!is.null(prox_thresh_m)) {
              keep <- dist <= prox_thresh_m
              idx  <- idx[keep]
            }
            t_nn <- now() - t0_nn
            
            # # 4) Count hits & coverage
            t0_count <- now()
            counts <- tabulate(idx, nbins = nmesh)
            mesh   <- copy(mesh0)
            mesh[, hits := counts]
            mesh[, covered := hits > 0L]
            
            # Band flags
            mesh[, below_cov   := covered & (risk_band == "below")]
            mesh[, at_risk_cov := covered & (risk_band == "at risk")]
            mesh[, above_cov   := covered & (risk_band == "above")]
            
            # ---- Band sizes (denominators) ----
            n_below  <- sum(mesh$risk_band == "below",   na.rm = TRUE)
            n_atrisk <- sum(mesh$risk_band == "at risk", na.rm = TRUE)
            n_above  <- sum(mesh$risk_band == "above",   na.rm = TRUE)
            n_total  <- nrow(mesh)
            
            # ---- Covered counts (numerators) ----
            c_below  <- sum(mesh$below_cov,   na.rm = TRUE)
            c_atrisk <- sum(mesh$at_risk_cov, na.rm = TRUE)
            c_above  <- sum(mesh$above_cov,   na.rm = TRUE)
            c_any    <- sum(mesh$covered,     na.rm = TRUE)
            
            # ---- Safe percentage helper ----
            pct <- function(num, den) if (den > 0) 100 * num / den else NA_real_
            
            # ---- Coverage % per band (relative to band size) ----
            p_below  <- pct(c_below,  n_below)
            p_atrisk <- pct(c_atrisk, n_atrisk)
            p_above  <- pct(c_above,  n_above)
            p_any    <- pct(c_any,    n_total)
            
            covered_points <- c_any
            t_count <- now() - t0_count
            
            # 5) create and Save mesh_cov CSV
            mesh_cov <- mesh[covered == TRUE, .(x, y, z, hits, risk_band)]
            
            data.table::fwrite(mesh_cov, file = file.path(folder_output_coverage,out_dir_csv, sprintf("%s.csv", wtg_id)))
            
            # 6) Build Plotly (fixed camera) & export (optional)
            if (export_plots) {
              
              # Build dynamic title
              plot_title <- sprintf(
                "WTG: %s | Bird Records: %d<br>Covered mesh points: %d (%.1f%%)
                 below (<50 m): %.1f%% 
                 at risk (50–200 m): %.1f%%
                 above (200–1000 m): %.1f%%\n ",
                wtg_id, nrec, covered_points, round(p_any,1),
                round(p_below,1), round(p_atrisk,1), round(p_above,1) 
              )
              
              
              # plot_title <- sprintf(
              #   "WTG: %s | Records: %d<br>Covered mesh: %d (%.1f%%)\n"
              #   %+% "below (<50 m): %.1f%% | at risk (50–200 m): %.1f%% | above (200–1000 m): %.1f%%",
              #   wtg_id, nrec, covered_points, round(p_any,1),
              #   round(p_below,1), round(p_atrisk,1), round(p_above,1)
              # )
              
              p <- 
                plot_ly() %>%
                add_markers(
                  data = mesh_cov,
                  x = ~x, y = ~y, z = ~z,
                  type = "scatter3d",
                  mode = "markers",
                  color = ~risk_band ,
                  colors = c("below" = "blue",
                             "at risk" = "red",
                             "above" = "green"),
                  marker = list(size = 2, opacity = 0.7)
                ) %>%
                # 2 Ground surface (accepts opacity, showscale, etc.)
                add_surface(
                  x = Xg, y = Yg, z = Zg,
                  opacity = 0.1,
                  showscale = FALSE,
                  surfacecolor = matrix("lightblue", nrow = nrow(Zg), ncol = ncol(Zg)),
                  name = "Ground (z=0)"
                ) %>%
                # 3️ Cylinder boundary surface
                add_surface(
                  x = Xc, y = Yc, z = Zc,
                  opacity = 0.08,
                  showscale = FALSE,
                  surfacecolor = matrix("lightgrey", nrow = nrow(Zc), ncol = ncol(Zc)),
                  #visible = "legendonly",
                  name = "Cylinder boundary"
                ) %>%
                layout(
                  title = list(
                    text = plot_title,
                    x = 0.05,
                    y = 0.95,
                    font = list(size = 11, color = "black")  # adjust title font size here
                  ),
                  scene = list(
                    xaxis = list(title = "X (m)"),
                    yaxis = list(title = "Y (m)"),
                    zaxis = list(title = "Z (m)"),
                    aspectmode = "manual",
                    aspectratio = list(x = 1, y = 1, z = 1),
                    # Fixed initial camera angle
                    camera = list(
                      eye = list(x = 1.8, y = 1.5, z = 1),  # position of the “eye”
                      center = list(x = 0, y = 0, z = 0),     # point camera looks at
                      up = list(x = 0, y = 0, z = 1)          # “up” direction
                    )
                  )
                )
              # add_markers(
              #   data = mesh_cov,
              #   x = ~x, y = ~y, z = ~z,
              #   type = "scatter3d", mode = "markers",
              #   color = ~risk_band,
              #   colors = c("below" = "blue", "at risk" = "red", "above" = "green"),
              #   marker = list(size = 2, opacity = 0.7),
              #   name = "Covered mesh"
              # ) %>%
              # add_surface(
              #   x = Xg, y = Yg, z = Zg,
              #   opacity = 0.12, showscale = FALSE,
              #   surfacecolor = matrix("lightblue", nrow = nrow(Zg), ncol = ncol(Zg)),
              #   name = "Ground (z=0)"
              # ) %>%
              # add_surface(
              #   x = Xc, y = Yc, z = Zc,
              #   opacity = 0.12, showscale = FALSE,
              #   surfacecolor = matrix("lightgrey", nrow = nrow(Zc), ncol = ncol(Zc)),
              #   visible = "legendonly",
              #   name = "Cylinder boundary"
              # ) %>%
              # layout(
              #   title = list(text = plot_title, font = list(size = title_font_size)),
              #   scene = list(
              #     xaxis = list(title = "X (m)"),
              #     yaxis = list(title = "Y (m)"),
              #     zaxis = list(title = "Z (m)"),
              #     aspectmode = "manual",
              #     aspectratio = list(x = 1, y = 1, z = 1),
              #     camera = list(eye = camera_eye, center = list(x=0,y=0,z=0), up = list(x=0,y=0,z=1))
              #   )
              # )
              
              # Save HTML
              # html_path <- file.path(out_dir_html, sprintf("%s.html", wtg_id))
              # saveWidget(p, file = html_path, selfcontained = TRUE)
              # 
              # # Save PNG (snapshot of HTML)
              # png_path <- file.path(out_dir_png, sprintf("%s.png", wtg_id))
              # webshot2::webshot(url = html_path, file = png_path, vwidth = plot_width, vheight = plot_height)
              # ---- HTML export: non-selfcontained for speed ----
              html_path <- file.path(folder_output_coverage,out_dir_html, sprintf("%s.html", wtg_id))
              saveWidget(
                widget = p,
                file   = html_path,
                selfcontained = FALSE,             # << faster, avoids giant base64 bundle
                libdir = file.path(folder_output_coverage,out_dir_html, sprintf("%s_libs", wtg_id))
              )
              
              # ---- PNG export via webshot2 with retries ----
              png_path <- file.path(folder_output_coverage,out_dir_png, sprintf("%s.png", wtg_id))
              
              try_webshot <- function(html, png, width, height, delay = 2, timeout = 300000, tries = 3) {
                for (k in seq_len(tries)) {
                  ok <- try({
                    webshot2::webshot(
                      url    = html,
                      file   = png,
                      vwidth = width, vheight = height,
                      delay  = delay,       # seconds to wait after load
                      timeout = timeout     # ms to wait for load event
                    )
                    TRUE
                  }, silent = TRUE)
                  if (isTRUE(ok)) return(TRUE)
                  Sys.sleep(1 + k)  # backoff
                }
                FALSE
              }
              
              ok <- try_webshot(
                html  = html_path,
                png   = png_path,
                width = plot_width, height = plot_height,
                delay = 2, timeout = 300000, tries = 3
              )
              
              if (!ok) {
                message(sprintf("[WARN] PNG export failed for %s via webshot2; continuing.", wtg_id))
              }
            } ### if export plots
            
            t_count <- now() - t0_count
          }
        }
        
        t_total <- now() - t0_total
        
        out_list[[i]] <- data.table(
          WTG_ID            = wtg_id,
          Records           = nrec,
          Covered_Mesh      = covered_points,
          Coverage_Mesh_Pct = round(p_any, 1),
          Below_Pct         = round(p_below, 1),
          AtRisk_Pct        = round(p_atrisk, 1),
          Above_Pct         = round(p_above, 1),
          BandSize_Below    = n_below,
          BandSize_AtRisk   = n_atrisk,
          BandSize_Above    = n_above,
          Covered_Below     = c_below,
          Covered_AtRisk    = c_atrisk,
          Covered_Above     = c_above,
          Covered_SumBands  = c_below + c_atrisk + c_above,   # should ≈ c_any
          Step_m            = step,
          Radius_m          = radius,
          Prox_3D_m         = if (is.null(prox_thresh_m)) NA_real_ else prox_thresh_m,
          t_prefilter_s     = round(t_pref, 3),
          t_project_s       = round(t_proj, 3),
          t_nn_s            = round(t_nn, 3),
          t_count_s         = round(t_count, 3),
          t_total_s         = round(t_total, 3)
        )
        
        #)
        
        if (verbose) setTxtProgressBar(pb, i)
      }
      
      if (verbose) close(pb)
      result <- rbindlist(out_list, use.names = TRUE, fill = TRUE)
      setorder(result, WTG_ID)
      result
    }
    
    result_tbl <- idf_coverage_table(
      wtg          = wtg,          # sf: ID + geometry (EPSG:4326)
      track_dt     = track_dt,     # data.table: lon, lat, height
      step         = mesh_cell_size,
      radius       = wtg_cylind_radius,
      height       = wtg_cylind_height,
      z_breaks     = c(0, 50, 200, wtg_cylind_height),
      prox_thresh_m = cell_prox_thresh_m,          # NULL to disable 3D proximity
      export_plots  = TRUE,
      out_dir_csv   = "mesh_csv",
      out_dir_html  = "html",
      out_dir_png   = "png",
      plot_width    = 850,
      plot_height   = 850,
      camera_eye    = list(x = 2.5, y = 2.5, z = 1.4),
      title_font_size = 11
    )
    
    fwrite(result_tbl, file.path(folder_output_coverage,paste0("idf_coverage_per_WTG_c",mesh_cell_size,"m-r",wtg_cylind_radius,"m.csv")))
    
    
    nrow(result_tbl) #total number of analysed turbines
    
    
  ### Plots ----
    
  
    ###Coverage freq. plot ----
    coverage <- result_tbl$Coverage_Mesh_Pct
    df <- data.frame(coverage)
    p <- ggplot(df, aes(x = coverage)) +
      geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
      scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
      labs(
        x = "Volumetric coverage (%)",
        y = "Number of WTGs",
        title = "Frequency distribution of volumetric coverage per WTG"
      ) +
      theme_minimal()
    ggsave(file.path(folder_output_coverage, paste0("idf_coverage_freq_c",mesh_cell_size,"m-r",wtg_cylind_radius,"m.png")), 
           plot = p, width = 8, height = 6, dpi = 300, bg = "white")
  
    
   
    
    
    ###Risk-zone coverage freq. plot ----
    coverage_risk <- result_tbl$AtRisk_Pct
    
    risk_df <- data.frame(AtRisk_Pct = coverage_risk)
    
    p <- ggplot(risk_df, aes(x = AtRisk_Pct)) +
      geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
      scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
      labs(
        x = "Risk-zone volumetric coverage (%)",
        y = "Number of WTGs",
        title = "Frequency distribution of risk-zone volumetric coverage per WTG"
      ) +
      theme_minimal()
    ggsave(file.path(folder_output_coverage, paste0("idf_coverage-risk_freq_c",mesh_cell_size,"m-r",wtg_cylind_radius,"m.png")), 
           plot = p, width = 8, height = 6, dpi = 300, bg = "white")
    
    
    
    
    ###Plot conjunto - coverage + risk-coverage ----
    plot_df <- rbind(
      data.frame(metric = "Volumetric coverage", value = coverage),
      data.frame(metric = "Risk-zone coverage", value = coverage_risk)
    )
    
    p <- ggplot(plot_df, aes(x = value, fill = metric)) +
      geom_histogram(
        binwidth = 1,
        position = "identity",
        alpha = 0.5,
        color = "black"
      ) +
      scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
      labs(
        x = "Coverage (%)",
        y = "Number of WTGs",
        fill = "Metric",
        title = "Frequency distribution of volumetric and risk-zone coverage"
      ) +
      theme_minimal()
    ggsave(file.path(folder_output_coverage, paste0("idf_coverage&cov-risk_freq_c",mesh_cell_size,"m-r",wtg_cylind_radius,"m.png")), 
           plot = p, width = 8, height = 6, dpi = 300, bg = "white")
    
    
    #Table with coverage categories and turbines
    #Total coverage
    coverage_levels <- c(
      "<25% (Very low)",
      "25-49% (Low)",
      "50-74% (Moderate)",
      "75-94% (High)",
      "≥95% (Very high)"
    )
    
    coverage_table <- result_tbl %>%
      mutate(
        Coverage_Class = case_when(
          Coverage_Mesh_Pct < 25 ~ "<25% (Very low)",
          Coverage_Mesh_Pct >= 25 & Coverage_Mesh_Pct < 50 ~ "25-49% (Low)",
          Coverage_Mesh_Pct >= 50 & Coverage_Mesh_Pct < 75 ~ "50-74% (Moderate)",
          Coverage_Mesh_Pct >= 75 & Coverage_Mesh_Pct < 95 ~ "75-94% (High)",
          Coverage_Mesh_Pct >= 95 ~ "≥95% (Very high)"
        ),
        Coverage_Class = factor(Coverage_Class, levels = coverage_levels)
      ) %>%
      group_by(Coverage_Class) %>%
      summarise(
        `Number of WTGs` = n(),
        WTGs = paste(sort(WTG_ID), collapse = ", "),
        .groups = "drop"
      ) %>%
      tidyr::complete(
        Coverage_Class = coverage_levels,
        fill = list(`Number of WTGs` = 0, WTGs = "")
      ) %>%
      mutate(
        `% of Total` = round(`Number of WTGs` / sum(`Number of WTGs`) * 100, 1)
      ) %>%
      arrange(factor(Coverage_Class, levels = coverage_levels))
    
    write_xlsx(coverage_table, file.path(folder_output_coverage,paste0("idf_coverage_summary_c",mesh_cell_size,"m-r",wtg_cylind_radius,"m.xlsx")))

    
    #Risk-zone coverage
    coverage_levels <- c(
      "<25% (Very low)",
      "25-49% (Low)",
      "50-74% (Moderate)",
      "75-94% (High)",
      "≥95% (Very high)"
    )
    
    coverage_risk_table <- result_tbl %>%
      mutate(
        Coverage_Class = case_when(
          AtRisk_Pct < 25 ~ "<25% (Very low)",
          AtRisk_Pct >= 25 & AtRisk_Pct < 50 ~ "25-49% (Low)",
          AtRisk_Pct >= 50 & AtRisk_Pct < 75 ~ "50-74% (Moderate)",
          AtRisk_Pct >= 75 & AtRisk_Pct < 95 ~ "75-94% (High)",
          AtRisk_Pct >= 95 ~ "≥95% (Very high)"
        ),
        Coverage_Class = factor(Coverage_Class, levels = coverage_levels)
      ) %>%
      group_by(Coverage_Class) %>%
      summarise(
        `Number of WTGs` = n(),
        WTGs = paste(sort(WTG_ID), collapse = ", "),
        .groups = "drop"
      ) %>%
      tidyr::complete(
        Coverage_Class = coverage_levels,
        fill = list(`Number of WTGs` = 0, WTGs = "")
      ) %>%
      mutate(
        `% of Total` = round(`Number of WTGs` / sum(`Number of WTGs`) * 100, 1),
        Coverage_Class = factor(Coverage_Class, levels = coverage_levels)
      ) %>%
      arrange(Coverage_Class)
    
    write_xlsx(coverage_risk_table, file.path(folder_output_coverage,paste0("idf_coverage-risk_summary_c",mesh_cell_size,"m-r",wtg_cylind_radius,"m.xlsx")))
    

    
    ###Spatial plot/map ----
    
    # Spatial Visualization - colored (heat map style)
    p <-
      wtg %>%
      left_join(
        result_tbl %>%
          mutate(Coverage_Mesh_Pct = as.numeric(gsub(",", ".", Coverage_Mesh_Pct))) %>%
          select(WTG_ID, Coverage_Mesh_Pct),
        by = c("ID" = "WTG_ID")
      ) %>%
      mutate(
        cov_plot = ifelse(is.na(Coverage_Mesh_Pct) | Coverage_Mesh_Pct == 0, NA, Coverage_Mesh_Pct)
      ) %>%
      ggplot() +
      geom_sf(aes(size = Coverage_Mesh_Pct, colour = cov_plot)) +
      geom_sf_text(
        aes(label = ID),
        colour = "darkred",
        size = 2,
        nudge_y = 200
      ) +
      scale_colour_gradientn(
        colours = c("red", "orange", "yellow"),   # reversed order
        limits = c(0, 100),
        na.value = "grey70",
        name = "Coverage (%)"
      ) +
      scale_size(
        range = c(1, 8),
        limits = c(0, 100),
        name = "Coverage (%)"
      ) +
      theme_minimal()
    
    ggsave(filename = file.path(folder_output_coverage, paste0('idf_coverage_spatial(colored)_',date(ini),'to',date(end),'.png')),
           plot = p, bg = "white", width = 10, height = 8, dpi = 300)
    
  
    
    
    
    #map 2
    
    coverage_levels <- c(
      "<25% (Very low)",
      "25-49% (Low)",
      "50-74% (Moderate)",
      "75-94% (High)",
      "≥95% (Very high)"
    )
    
    coverage_colors <- c(
      "<25% (Very low)"   = "#d73027",
      "25-49% (Low)"      = "#fc8d59",
      "50-74% (Moderate)" = "#fee08b",
      "75-94% (High)"     = "#66bd63",
      "≥95% (Very high)"  = "#4575b4"
    )
    
    wtg_cov_plot <- wtg %>%
      left_join(
        result_tbl %>%
          mutate(
            Coverage_Mesh_Pct = as.numeric(gsub(",", ".", Coverage_Mesh_Pct))
          ) %>%
          select(WTG_ID, Coverage_Mesh_Pct),
        by = c("ID" = "WTG_ID")
      ) %>%
      mutate(
        Coverage_Class = case_when(
          is.na(Coverage_Mesh_Pct) ~ NA_character_,
          Coverage_Mesh_Pct < 25 ~ "<25% (Very low)",
          Coverage_Mesh_Pct >= 25 & Coverage_Mesh_Pct < 50 ~ "25-49% (Low)",
          Coverage_Mesh_Pct >= 50 & Coverage_Mesh_Pct < 75 ~ "50-74% (Moderate)",
          Coverage_Mesh_Pct >= 75 & Coverage_Mesh_Pct < 95 ~ "75-94% (High)",
          Coverage_Mesh_Pct >= 95 ~ "≥95% (Very high)"
        ),
        Coverage_Class = factor(Coverage_Class, levels = coverage_levels)
      )
    
    p <- ggplot(wtg_cov_plot) +
      geom_sf(
        aes(size = Coverage_Mesh_Pct, colour = Coverage_Class),
        show.legend = c(size = TRUE, colour = TRUE)
      ) +
      geom_sf_text(
        aes(label = ID),
        colour = "darkred",
        size = 2,
        nudge_y = 200
      ) +
      scale_colour_manual(
        values = coverage_colors,
        breaks = coverage_levels,   # show all classes in this order
        limits = coverage_levels,
        drop = FALSE,
        na.value = "grey70",
        name = "Coverage class"
      ) +
      scale_size_continuous(
        range = c(1, 8),
        limits = c(0, 100),
        breaks = c(0, 25, 50, 75, 100),
        name = "Coverage (%)"
      ) +
      guides(
        colour = guide_legend(
          override.aes = list(size = 6),   # larger legend icons
          order = 1
        ),
        size = guide_legend(order = 2)
      ) +
      theme_minimal() +
      theme(
        legend.position = "right",
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 10),
        legend.key.size = unit(0.7, "cm")  # optional: bigger legend key box
      ) +
      labs(x = "x", y = "y")
    
    ggsave(filename = file.path(folder_output_coverage, paste0('idf_coverage_spatial(colored2)_',date(ini),'to',date(end),'.png')),
      plot = p, bg = "white", width = 10, height = 8, dpi = 300)
    
 
    
    