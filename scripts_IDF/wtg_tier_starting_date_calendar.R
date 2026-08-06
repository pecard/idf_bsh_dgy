##
## Gant-like calendar for starting date in each tier per turbine
##
       

    #report_start <- as.Date("2025-02-25") ##definido no userSettings.txt
    #report_end   <- as.Date("2026-02-15") ##definido no userSettings.txt

    #make gantt
    gantt <- rbindlist(list(
      tier_dt[, .(turbine, Tier = "Tier 1",  start = safe_date(`Tier 1 ini`),  end = safe_date(`tier 1 end`))][!is.na(start) & !is.na(end) & end >= start],
      tier_dt[, .(turbine, Tier = "Tier 2A", start = safe_date(`Tier 2A ini`), end = safe_date(`Tier 2A end`))][!is.na(start) & !is.na(end) & end >= start],
      tier_dt[, .(turbine, Tier = "Tier 2B", start = safe_date(`Tier 2B ini`), end = safe_date(`Tier 2B end`))][!is.na(start) & !is.na(end) & end >= start],
      tier_dt[, .(turbine, Tier = "Tier 3",  start = safe_date(`Tier 3 ini`),  end = safe_date(report_end))][!is.na(start) & !is.na(end) & end >= start]
    ), use.names = TRUE, fill = TRUE)
    
    ## Keep only valid intervals
    gantt <- gantt[!is.na(start) & !is.na(end) & start <= end]
    
    ## Make sure start/end are Date
    gantt[, start := as.Date(start)]
    gantt[, end   := as.Date(end)]
    
    ## Order turbines by first start date
    #wtg_order <- gantt[, .(first_start = min(start, na.rm = TRUE)), by = turbine][order(first_start), turbine]
    #gantt[, turbine := factor(turbine, levels = rev(wtg_order))]
    
    ## Create custom tick positions
    start_date <- min(gantt$start, na.rm = TRUE)
    end_date   <- max(gantt$end, na.rm = TRUE)
    
    months_seq <- seq(floor_date(start_date, "month"),
                      ceiling_date(end_date, "month"),
                      by = "1 month")
    
    axis_breaks <- sort(unique(c(
      months_seq,            # day 1
      months_seq + days(14)  # day 15
    )))
    
    axis_breaks <- axis_breaks[axis_breaks >= start_date & axis_breaks <= end_date]
    
    ## Plot
    p <- ggplot(as.data.frame(gantt), aes(y = turbine, x = start, xend = end, color = Tier)) +
      geom_segment(aes(yend = turbine), linewidth = 6, lineend = "butt") +
      geom_vline(xintercept = report_start, linetype = "dashed", color = "black") +
      geom_vline(xintercept = report_end,   linetype = "dashed", color = "black") +
      scale_x_date(
        breaks = axis_breaks,
        date_labels = "%d %b"
      ) +
      labs(
        title = "WTG Tier Timeline",
        x = "Date",
        y = "WTG"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank()
      )
    
    ggsave(file.path(folder_output, "WTG_tier_timeline.png"), p, width = 12, height = 14, bg="white")
    
    