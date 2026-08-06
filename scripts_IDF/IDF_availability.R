##
## Availability ----
##
    

    ##Needed packages
    packages <- c('tidyverse','janitor','data.table','lubridate','suncalc','ggplot2') 
    
    ##Check and install packages that are missing + call library()
    for (p in packages) {
      if (!require(p, character.only = TRUE)) install.packages(p)
      library(p, character.only = TRUE) }

    
    # --- Inputs needed ---
    # heartb_dt: idf (chr), timestamp (POSIXct), min_hb (ignored here)
    # cal: date (Date), sunrise (POSIXct tz="Asia/Tashkent"), sunset (POSIXct tz="Asia/Tashkent")
    
    ## 
    ## Parâmetros e dados
    ## 

    # ini --> defined in userSettings.txt
    # end --> defined in userSettings.txt
    tz_site  <- proj_timezone # --> defined in userSettings.txt
    start_date <- as.Date(ini) #get date only
    end_date   <- as.Date(end) #get date only    
    lat <- proj_lat
    lon <- proj_lon
    
    # Sort and make previous timestamp per unit
    setkeyv(heartb_dt, c("idf","timestamp"))
    
    heartb_dt[, ts_prev := shift(timestamp, type = "lag"), by = idf]
    
    # For the first reading of each unit, assume a 30-min previous tick so gap=30 (online)
    heartb_dt[is.na(ts_prev), ts_prev := timestamp - minutes(30)]
    
    # Gap in minutes
    heartb_dt[, gap_mins := as.numeric(difftime(timestamp, ts_prev, units = "mins"))]
    
    uniqhb_dt <- unique(heartb_dt, by = key(heartb_dt))
    
    # --- First operational heartbeat per IDF (datetime + local date)
    idf_first <- uniqhb_dt[, .(
      first_ts   = min(timestamp, na.rm = TRUE)
    ), by = idf]
    
    idf_first[, first_date := as.IDate(first_ts, tz = tz_site)]
    
    # --- Build OFFLINE intervals only for gaps >= 60 ---
    off <-
      heartb_dt[gap_mins >= 60,
                .(idf,
                  off_start = ts_prev + minutes(30),  # first 30 min of the gap are considered online
                  off_end   = timestamp)
      ]
    
    
    ### 
    ### Calendário com horas de luz (sunrise/sunset) ----
    ### 
    cal <- data.table(date = seq(start_date, end_date, by = "day", tz = tz_site))
    
    #cal[, date := force_tz(date, tz_site)] # Time zone
    
    sun <-
      as.data.table(
        getSunlightTimes(
          date = cal$date, 
          lat = lat, 
          lon = lon,
          keep = c("sunrise", "sunset")
          ,tz = tz_site # ensure Time Zone
        )
      )
    
    # check tz
    tz(sun$sunrise)
    tz(sun$sunset)
    
    # Tabela calendário final
    cal <- 
      sun[, .(date, sunrise, sunset)][
        , daylight_hours := as.numeric(difftime(sunset, sunrise, units = "hours"))][]
    
    sum(cal$daylight_hours)
    
    # If there are no offline intervals, create empty outputs and bail out cleanly
    if (nrow(off) == 0L) {
      res <- cal[, .(date,
                     idf = NA_character_,
                     offline_mins = 0.0,
                     daylight_mins = as.numeric(difftime(sunset, sunrise, units = "mins")),
                     offline_pct = 0.0)][0] # empty
      sum_by_idf <- data.table(idf=character(), offline_mins_total=double(),
                               offline_hours_total=double(), daylight_mins_total=double(),
                               offline_pct_weighted=double(), days_covered=integer())
    } else {
      
      # For each offline interval, generate all dates it spans
      off[, `:=`(
        date_start = as.IDate(off_start, tz = tz_site),
        date_end   = as.IDate(off_end,   tz = tz_site)
      )]
      
      # Expand to one row per (interval × date)
      off_exp <- off[, .(date = seq(date_start, date_end, by = "day")),
                     by = .(idf, off_start, off_end)]
      
      # Join daily daylight window
      off_exp <- cal[off_exp, on = .(date), nomatch = 0]
      
      # Day boundaries
      off_exp[, day_start := as.POSIXct(date, tz = tz_site)]
      off_exp[, day_end   := day_start + days(1)]
      
      # Compute overlap between [off_start, off_end] and daylight [sunrise, sunset] within the day.
      # Work in numeric seconds to avoid tz attribute shenanigans.
      off_exp[, start_num := pmax(as.numeric(off_start),
                                  as.numeric(day_start),
                                  as.numeric(sunrise))]
      off_exp[, end_num   := pmin(as.numeric(off_end),
                                  as.numeric(day_end),
                                  as.numeric(sunset))]
      
      off_exp[, offline_mins := pmax(0, (end_num - start_num) / 60)]
      
      # Aggregate per unit × day
      daily_off <- off_exp[, .(offline_mins = sum(offline_mins)), by = .(idf, date)]
      
      # Join daylight and compute % offline
      res <- cal[daily_off, on = .(date)]
      res[, daylight_mins := as.numeric(difftime(sunset, sunrise, units = "mins"))]
      res[, offline_pct   := fifelse(daylight_mins > 0,
                                     offline_mins / daylight_mins,
                                     NA_real_)]
      res[, ym := format(date, "%Y-%m")]
      
      
      
      ### Totals per IDF
      sum_by_idf <- 
        res[, .(
          offline_mins_total   = sum(offline_mins,  na.rm = TRUE),
          offline_hours_total  = sum(offline_mins,  na.rm = TRUE) / 60,
          daylight_mins_total  = sum(daylight_mins, na.rm = TRUE),
          # offline_pct_weighted = fifelse(sum(daylight_mins, na.rm = TRUE) > 0,
          #                                100 * sum(offline_mins, na.rm = TRUE) /
          #                                  sum(daylight_mins, na.rm = TRUE),
          #                                NA_real_),
          days_covered         = uniqueN(date)
        ), by = idf][order(-offline_mins_total)]
      sum_by_idf[, online_per := 100-offline_hours_total/2742.287*100]
      sum_by_idf[, online_h := 2742.287-offline_hours_total]
      
    }
    
    view(arrange(res, date))
    
    
    res_month <- 
      res[, .(
        offline_mins  = sum(offline_mins,  na.rm = TRUE),
        daylight_mins = sum(daylight_mins, na.rm = TRUE),
        # weighted monthly % = (total offline / total daylight)
        offline_pct   = fifelse(sum(daylight_mins, na.rm = TRUE) > 0,
                                sum(offline_mins,  na.rm = TRUE) / sum(daylight_mins, na.rm = TRUE),
                                NA_real_)*100,
        days_covered  = uniqueN(date),
        days_with_daylight = sum(daylight_mins > 0, na.rm = TRUE)
      ), by = .(idf, ym)][order(idf, ym)]
    
    
    sum_by_idf_month <- 
      res[, .(
        offline_mins_total   = sum(offline_mins,  na.rm = TRUE),
        offline_hours_total  = sum(offline_mins,  na.rm = TRUE) / 60,
        daylight_mins_total  = sum(daylight_mins, na.rm = TRUE),
        # weighted monthly % in percent (to match sum_by_idf)
        offline_pct_weighted = fifelse(sum(daylight_mins, na.rm = TRUE) > 0,
                                       100 * sum(offline_mins,  na.rm = TRUE) /
                                         sum(daylight_mins, na.rm = TRUE),
                                       NA_real_),
        days_covered         = uniqueN(date),
        days_with_daylight   = sum(daylight_mins > 0, na.rm = TRUE)
      ), by = .(idf, ym)][order(ym, -offline_mins_total)]
    
    fleet_month <- 
      res[, .(
        offline_mins_total   = sum(offline_mins,  na.rm = TRUE),
        daylight_mins_total  = sum(daylight_mins, na.rm = TRUE),
        offline_pct_weighted = fifelse(sum(daylight_mins, na.rm = TRUE) > 0,
                                       100 * sum(offline_mins, na.rm = TRUE) /
                                         sum(daylight_mins, na.rm = TRUE),
                                       NA_real_)
      ), by = ym][order(ym)]
    
    # 'res' -> per IDF × day: offline_mins (daylight-only), daylight_mins, offline_pct
    # 'sum_by_idf' -> per IDF totals over the study window
    
    
    # If you prefer viridis, uncomment:
    # library(viridis)
    
    # INPUT expected:
    # res: data.table with columns idf, date (Date), offline_mins, daylight_mins, offline_pct (0..1 or NA)
    # cal: data.table with columns date, sunrise, sunset (POSIXct, tz="Asia/Tashkent")  # used only to ensure full date range
    # heartb_dt: (used only to list all IDFs) idf, timestamp
    
    ### Build a complete IDF × date grid so all days appear (0% for missing)
    setDT(res); setDT(cal)
    all_idf <- sort(unique(heartb_dt$idf))
    grid <- CJ(idf = all_idf, date = cal$date)
    
    # Join res onto the full grid, bring daylight_mins in, and fill missing as 0% (when daylight > 0)
    res_full <- res[grid, on = .(idf, date)]
    res_full <- cal[res_full, on = .(date)]  # adds sunrise/sunset if needed
    res_full[, daylight_mins := as.numeric(difftime(sunset, sunrise, units = "mins"))]
    
    # Offline mins / pct: set NA to 0 where there is daylight (i.e., no offline observed)
    res_full[is.na(offline_mins) & daylight_mins > 0, offline_mins := 0]
    res_full[, offline_pct := fifelse(daylight_mins > 0, offline_mins / daylight_mins, NA_real_)]
    
    # --- Calendar coordinates ---

    res_full[, `:=`(
      ym   = sprintf("%d-%02d", lubridate::year(date), lubridate::month(date)),
      wday = lubridate::wday(date, week_start = 1),
      weekday_lab = factor(
        c("M","T","W","T.","F","S","S.")[lubridate::wday(date, week_start = 1)],
        levels = c("M","T","W","T.","F","S","S.")
      ),
      week_of_month = {
        ms  <- lubridate::floor_date(date, "month")
        wms <- lubridate::wday(ms, week_start = 1)
        as.integer(1 + ((lubridate::mday(date) + wms - 2) %/% 7))
      }
    )]
    
    # --- Choose which IDFs to show (A) provide a vector, or (B) auto-pick top N) ---
    # A) provide a vector manually:
    idf_sel <- NULL  # e.g., c("GW55-35", "GW56-37")
    
    # B) or pick top N by total offline across the period:
    if (is.null(idf_sel)) {
      top_n <- 12L
      idf_sel <- res_full[, .(offline_mins_total = sum(offline_mins, na.rm = TRUE)), by = idf
      ][order(-offline_mins_total)][seq_len(min(top_n, .N)), idf]
    }
    
    res_plot <- res_full[idf %in% idf_sel]
    
    
    ### Availability calendar plot per IDF ----
    # Split data for two-layer fill: grey for 0%, gradient for > 0
    dat_zero <- res_plot[!is.na(offline_pct) & offline_pct <= 0]
    dat_pos  <- res_plot[!is.na(offline_pct) & offline_pct > 0]
    
    p <- ggplot() +
      # 0% cells in grey (no legend)
      geom_tile(data = dat_zero,
                aes(x = weekday_lab, y = week_of_month),
                fill = "grey90", color = "grey85", linewidth = 0.2) +
      # >0% cells on gradient
      geom_tile(data = dat_pos,
                aes(x = weekday_lab, y = week_of_month, fill = offline_pct),
                color = "grey85", linewidth = 0.2) +
      facet_grid(idf ~ ym, scales = "free_y") +
      # If you prefer viridis (comment the line below and uncomment the next one)
      scale_fill_gradient(name = "Offline (%)",
                          limits = c(0, 1),
                          labels = scales::percent,
                          breaks = scales::pretty_breaks(5),
                          low = "#e0f3db", high = "#0868ac") +
      # scale_fill_viridis(name = "Offline (%)", limits = c(0,1), labels = scales::percent, option = "C")
      scale_y_reverse(breaks = seq(1, 6, 1)) +
      labs(
        x = NULL, y = "Week of month",
        title = "Calendar-style heatmap of offline % during daylight",
        subtitle = sprintf("Top %d IDFs by offline time • %s to %s",
                           length(idf_sel),
                           format(min(res_full$date), "%d %b %Y"),
                           format(max(res_full$date), "%d %b %Y")),
        caption = "Grey = 0% offline; coloured cells show % of daylight that was offline"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "grey95", color = NA),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(hjust = 0.5),
        axis.ticks = element_blank()
      )
    
    
    png(file.path(folder_output, 
                  paste0('idf_availabity_calendar_',date(ini),'to',date(end),'.png')),
        width = 3000, height = 2400, res = 300)
    p
    dev.off()
    
    ###Availability time/ unavailable(offline) time per IDF----
    offline_summary <- res_plot[
      , .(
        total_offline_mins = sum(offline_mins, na.rm = TRUE),
        total_daylight_mins = sum(daylight_mins, na.rm = TRUE)
      ),
      by = idf
    ][
      , monitoring_period_pct := round(100 * total_offline_mins / total_daylight_mins, 1)
    ][
      , availability_pct := round(100 - monitoring_period_pct, 1)
    ][
      order(-monitoring_period_pct)
    ]
    
    write_xlsx(offline_summary, file.path(folder_output,"idf_availability_summary-Top12unavail.xlsx"))
    
    ### Tabela todos os IDFs ----
    offline_summary_all <- res_full[
      , .(
        total_offline_mins = sum(offline_mins, na.rm = TRUE),
        total_daylight_mins = sum(daylight_mins, na.rm = TRUE)
      ),
      by = idf
    ][
      , monitoring_period_pct := round(100 * total_offline_mins / total_daylight_mins, 1)
    ][
      , availability_pct := round(100 - monitoring_period_pct, 1)
    ][
      order(-monitoring_period_pct)
    ]
    
    write_xlsx(offline_summary_all, file.path(folder_output,"idf_availability_summary-allIDFs.xlsx"))
    
    ### Plot freq ----
    offline_summary_all <- offline_summary_all %>%
      mutate(
        availability_cat = case_when(
          availability_pct > 96 ~ ">96%",
          availability_pct > 90 & availability_pct <= 96 ~ "90–96%",
          availability_pct > 80 & availability_pct <= 90 ~ "80–90%",
          availability_pct > 60 & availability_pct <= 80 ~ "60–80%",
          TRUE ~ "<60%"
        )
      )
    
    offline_summary_all <- offline_summary_all %>%
      mutate(
        availability_cat = factor(
          availability_cat,
          levels = c("<60%", "60–80%", "80–90%", "90–96%", ">96%")
        )
      )
    
    p <- ggplot(offline_summary_all, aes(x = availability_cat)) +
      geom_bar(fill = "steelblue") +
      labs(
        x = "System availability category",
        y = "Number of IDF units",
        title = "Frequency of IDF units by availability category"
      ) +
      theme_minimal()
  
    png(file.path(folder_output, 
                  paste0('idf_availabity_freq_',date(ini),'to',date(end),'.png')),
        width = 1800, height = 900, res = 300)
    p
    dev.off()
    
    freq_table <- offline_summary_all %>%
      count(availability_cat, name = "n_IDF_units") %>%
      mutate(
        pct = round(100 * n_IDF_units / sum(n_IDF_units), 1)
      )
    write_xlsx(freq_table, file.path(folder_output,"idf_availability_freq.xlsx"))
    
    
    