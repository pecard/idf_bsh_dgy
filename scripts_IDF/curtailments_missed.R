##
## Missed curtailments
##

    #combine all
    scada_crt_roll_all_combined <- rbindlist(scada_crt_roll_all, use.names = TRUE, fill = TRUE, idcol = "list_id")
    setnames(scada_crt_roll_all_combined, "value", "rpm_value")
    
    hora_ini <- hour(scada_ini)
    hora_end <- hour(scada_end)
    
    ##
    ## Rotor profile plot for 1 turbine as example ----
    ##
    scada_dtWTG <- scada_crt_roll_all[[1]] #turbina exemplo 
    scada_dtWTG[, `:=`(
      day = as.Date(datetime),
      h = hour(datetime),
      hms = format(datetime, "%H:%M:%S"),
      dec = hour(datetime) +
        minute(datetime)/60 +
        second(datetime)/3600
    )]
    setnames(scada_dtWTG, "value", "rpm_value")
    
    # RPM profile only
    rpm_plot <- scada_dtWTG %>%
      filter(h %in% hora_ini:hora_end)
    
    # Curtailment markers only
    curt_plot <- scada_dtWTG %>%
      filter(status %in% c("start", "end"),
             h %in% hora_ini:hora_end)
    
    #plot 

    p <- ggplot() +
      geom_path(
        data = rpm_plot,
        aes(x = dec, y = rpm_value, group = day),
        linewidth = 0.2,
        colour = "black"
      ) +
      geom_vline(
        data = curt_plot,
        aes(xintercept = dec, colour = status),
        linewidth = 0.4
      ) +
      facet_grid(day ~ .) +
      scale_x_continuous(
        breaks = hora_ini:hora_end,
        limits = c(hora_ini, hora_end + 1)
      ) +
      scale_colour_manual(
        values = c("start" = "blue", "end" = "orange")
      ) +
      theme(
        strip.text.y = element_text(size = 6, angle = 0),
        axis.text.y = element_text(size = 6)
      ) +
      labs(x = "hour", y = "rpm", colour = "status",  title = paste0("Rotor RPM profile and curtailment events - ",turbine_list))
    

    ggsave(file.path(folder_output, paste0('curtail&rotor_profile_day_',date(ini),'to',date(end),"-",turbinas_teste[1],'.png')), 
           plot = p, width = 6, height = 4, dpi = 300)
   
    
    
    
    
    ##
    ## Calcular total de missed curtailment com base na definicao --> missed = if min(rpm) >1 ----
    ##
    

    # 1. Curtailment orders: one row per curtailment id
    curtailment_orders <- scada_crt_roll_all_combined[
      status %in% c("start", "end") & !is.na(id),
      .(
        turbinelabel = first(na.omit(turbinelabel)),
        species = first(na.omit(species)),
        track_id = first(na.omit(track_id)),
        datetime_ini = datetime[status == "start"][1],
        datetime_end = datetime[status == "end"][1],
        rpm_value_ini = rpm_value[status == "start"][1],
        rpm_value_end = rpm_value[status == "end"][1]
      ),
      by = .(list_id, id)
    ][
      ,
      `:=`(
        curtl_dur_sec = as.numeric(difftime(datetime_end, datetime_ini, units = "secs")),
        curtl_dur_min = as.numeric(difftime(datetime_end, datetime_ini, units = "mins"))
      )
    ]
    
    # RPM-only table
    rpm_dt <- scada_crt_roll_all_combined[
      !is.na(rpm_value),
      .(list_id, turbinelabel, datetime, rpm_value)
    ]
    
    # Rename curtailment window columns to avoid confusion in the join
    orders_dt <- copy(curtailment_orders)
    setnames(orders_dt, c("datetime_ini", "datetime_end"), c("curt_start", "curt_end"))
    
    # Join all RPM rows that fall inside each curtailment window
    rpm_join <- rpm_dt[
      orders_dt,
      on = .(
        list_id,
        turbinelabel,
        datetime >= curt_start,
        datetime <= curt_end
      ),
      allow.cartesian = TRUE,
      nomatch = NA
    ]
    
    # Compute minimum RPM during each curtailment
    min_rpm_dt <- rpm_join[
      ,
      .(
        min_rpm = if (all(is.na(rpm_value))) NA_real_ else min(rpm_value, na.rm = TRUE),
        n_rpm_obs = sum(!is.na(rpm_value))
      ),
      by = .(list_id, id)
    ]
    
    # Merge back to curtailment_orders
    curtailment_orders <- merge(
      curtailment_orders,
      min_rpm_dt,
      by = c("list_id", "id"),
      all.x = TRUE
    )
    
    # Missed curtailment classification:
    # TRUE  = turbine never dropped below 1 rpm
    # FALSE = turbine reached < 1 rpm
    # NA    = no RPM data in interval
    curtailment_orders[
      ,
      missed_curtailment := fifelse(
        is.na(rpm_value_end),
        NA,
        min_rpm >= 1
      )
    ]
   
    
    summary_table <- curtailment_orders[
      ,
      .(
        total_orders = .N,
        orders_with_rpm_data = sum(!is.na(missed_curtailment)),
        missed_orders = sum(missed_curtailment, na.rm = TRUE),
        missed_pct = 100 * sum(missed_curtailment, na.rm = TRUE) / sum(!is.na(missed_curtailment)),
        num_missed_60s = sum(missed_curtailment & curtl_dur_sec < 60, na.rm = TRUE),
        missed_60s_pct = ifelse(
          sum(missed_curtailment, na.rm = TRUE) == 0,
          NA_real_,
          100 * sum(missed_curtailment & curtl_dur_sec < 60, na.rm = TRUE) /
            sum(missed_curtailment, na.rm = TRUE)
        )
      )
    ]
    write.xlsx(summary_table, file.path(folder_output,paste0("curtail_Missed_summary_",date(scada_ini),'to',date(scada_end),".xlsx")), overwrite = TRUE)
    
   
    
  #View missed curtailments
    missed_curtailments <- curtailment_orders[
      missed_curtailment == TRUE
    ]
    write.xlsx(missed_curtailments, file.path(folder_output,paste0("curtail_Missed_events_",date(scada_ini),'to',date(scada_end),".xlsx")), overwrite = TRUE)
    
    
    
    
  ##
  ## View rpm profile for specific temporal window----
  ##
    
    View(curtl_dt[track_id == "20A21E0A-1293-4FCE-9072-C1EADE6F6392"])
    
    #Get the longest missed curtailment event
    #Define window
    missed_event_max_dur <- missed_curtailments[which.max(curtl_dur_sec)]
    turbine_sel <- missed_curtailments[which.max(curtl_dur_sec), turbinelabel] #get turbine label of a missed curtailment with max duration
    dia_sel <- as.Date(missed_curtailments[which.max(curtl_dur_sec), datetime_ini])
    datetime_ini_sel <- as.POSIXct(missed_curtailments[which.max(curtl_dur_sec), datetime_ini]-60) #intervalo - 5seg
    datetime_end_sel <- as.POSIXct(missed_curtailments[which.max(curtl_dur_sec), datetime_end]+60) #intervalo + 5seg
    missed_species <- missed_curtailments[which.max(curtl_dur_sec), species]
    
    # get selected turbine
    scada_dtWTG <- as.data.table(scada_crt_roll_all[[turbine_sel]])

    # create helper columns
    scada_dtWTG[, `:=`(
      day = as.Date(datetime),
      h = hour(datetime),
      hms = format(datetime, "%H:%M:%S"),
      dec = hour(datetime) + minute(datetime)/60 + second(datetime)/3600
    )]
    
    # rename only if needed
    if ("value" %in% names(scada_dtWTG) && !"rpm_value" %in% names(scada_dtWTG)) {
      setnames(scada_dtWTG, "value", "rpm_value")
    }
    
    # filter rpm profile for selected date/time window
    rpm_plot <- scada_dtWTG %>%
      filter(
        day == dia_sel,
        datetime >= datetime_ini_sel,
        datetime <= datetime_end_sel
      )
    
    # curtailment markers in same window
    curt_plot <- scada_dtWTG %>%
      filter(
        day == dia_sel,
        status %in% c("start", "end"),
        datetime >= datetime_ini_sel,
        datetime <= datetime_end_sel
      )
    
    # decimal hour limits for plotting
    x_ini <- hour(datetime_ini_sel) + minute(datetime_ini_sel)/60 + second(datetime_ini_sel)/3600
    x_end <- hour(datetime_end_sel) + minute(datetime_end_sel)/60 + second(datetime_end_sel)/3600
    
    p <- ggplot() +
      geom_path(
        data = rpm_plot,
        aes(x = dec, y = rpm_value, group = day),
        linewidth = 0.2,
        colour = "black"
      ) +
      geom_vline(
        data = curt_plot,
        aes(xintercept = dec, colour = status),
        linewidth = 0.4
      ) +
      facet_grid(day ~ .) +
      scale_x_continuous(
        limits = c(x_ini, x_end)
      ) +
      scale_colour_manual(
        values = c("start" = "blue", "end" = "orange")
      ) +
      theme(
        strip.text.y = element_text(size = 6, angle = 0),
        axis.text.y = element_text(size = 6)
      ) +
      labs(
        x = "hour",
        y = "rpm",
        colour = "status",
        title = paste0("Rotor RPM profile during a curtailment classified as Missed (", turbine_sel,"; ",missed_species,")")
      ) +
      geom_hline(
        yintercept = 1,
        colour = "red",
        linewidth = 0.6,
        linetype = "dashed"
      )
    
    p
    
   
    remove(p, curt_plot,scada_dtWTG,turbine_sel,dia_sel,hora_ini,hora_end,rpm_plot)
    remove(min_rpm_dt,curtailment_orders)
    