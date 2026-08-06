##
## Response time
##
    
    #combine all
    scada_crt_roll_all_combined <- rbindlist(scada_crt_roll_all, use.names = TRUE, fill = TRUE, idcol = "list_id")
    setnames(scada_crt_roll_all_combined, "value", "rpm_value")
    
    # RPM rows only
    rpm_dt <- scada_crt_roll_all_combined[
      !is.na(rpm_value),
      .(list_id, turbinelabel, datetime, rpm_value)
    ]
    
    # Curtailment orders
    orders <- scada_crt_roll_all_combined[
      status %in% c("start", "end") & !is.na(id),
      .(
        datetime_ini = datetime[status == "start"][1],
        datetime_end = datetime[status == "end"][1]
      ),
      by = .(list_id, turbinelabel, id)
    ][!is.na(datetime_ini)]
    
    # Add 60-second pre-curtailment window start
    orders[, ref_start := datetime_ini - 60]
    
    # Join RPM rows in the 60 s before curtailment start
    ref_join <- rpm_dt[
      orders,
      on = .(
        list_id,
        turbinelabel,
        datetime >= ref_start,
        datetime < datetime_ini
      ),
      allow.cartesian = TRUE,
      nomatch = NA
    ]
    
    # -----------------------------
    # 1) Reference RPM = mean RPM in the 60 s before curtailment start
    # -----------------------------
    ref_dt <- ref_join[
      ,
      .(
        ref_rpm = if (all(is.na(rpm_value))) NA_real_ else mean(rpm_value, na.rm = TRUE),
        n_ref_obs = sum(!is.na(rpm_value))
      ),
      by = .(list_id, turbinelabel, id)
    ]
    
    
    # -----------------------------
    # 2) Attach reference RPM and calculate 10% drop threshold
    # -----------------------------
    orders_rt <- merge(
      orders,
      ref_dt,
      by = c("list_id", "turbinelabel", "id"),
      all.x = TRUE
    )
    
    orders_rt[, rpm_10drop := ref_rpm * 0.9]
    
    # -----------------------------
    # 3) Join RPM rows from curtailment start onward
    #    (here restricted to curtailment window; remove <= datetime_end
    #     if you want to allow response after curtailment end)
    # -----------------------------
    post_dt <- rpm_dt[
      orders_rt,
      on = .(
        list_id,
        turbinelabel,
        datetime >= datetime_ini,
        datetime <= datetime_end
      ),
      allow.cartesian = TRUE,
      nomatch = NA
    ]
    
    # -----------------------------
    # 4) First time rotor speed drops to or below the 10% threshold
    # -----------------------------
    resp_dt <- post_dt[
      !is.na(rpm_value) & !is.na(rpm_10drop) & rpm_value <= rpm_10drop,
      .(
        datetime_10drop = min(datetime)
      ),
      by = .(list_id, turbinelabel, id)
    ]
    
    # -----------------------------
    # 5) Merge back and compute response time
    # -----------------------------
    response_table <- merge(
      orders_rt,
      resp_dt,
      by = c("list_id", "turbinelabel", "id"),
      all.x = TRUE
    )
    
    response_table[
      ,
      response_time_sec := as.numeric(datetime_10drop - datetime_ini)
    ]
    
    response_table[
      ,
      response_time_min := response_time_sec / 60
    ]
    
    # Optional flag: did the turbine reach the 10% drop during curtailment?
    response_table[
      ,
      reached_10drop := !is.na(datetime_10drop)
    ]
    
  ## summary table----
    summary_response <- response_table[
      ,
      .(
        total_curtailments = .N,
        valid_curtailments = sum(!is.na(response_time_sec)),
        mean_RT_sec = mean(response_time_sec, na.rm = TRUE),
        sd_RT_sec = sd(response_time_sec, na.rm = TRUE),
        min_RT_sec = min(response_time_sec, na.rm = TRUE),
        max_RT_sec = max(response_time_sec, na.rm = TRUE)
      )
    ]
    write.xlsx(summary_response, file.path(folder_output,paste0("response_time_summary_",date(scada_ini),'to',date(scada_end),".xlsx")), overwrite = TRUE)
    
    
    events_no_response <- response_table[
      reached_10drop == FALSE
    ]
    write.xlsx(events_no_response, file.path(folder_output,paste0("response_time_fail_events_",date(scada_ini),'to',date(scada_end),".xlsx")), overwrite = TRUE)
    
    
    
    remove(events_no_response,summary_response,response_table,resp_dt,post_dt,ref_join,orders,rpm_dt)
    
    