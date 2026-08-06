##
## DQ check ----
##

  ### SCADA data ----

    setDT(scada_dt_unfilt)  # Ensure data.table
    
    # 1) Create Date column (from date_min or datetime)
scada_dt_unfilt[, date := as.Date(as.character(date_min))]
    # Alternatively (often more robust): scada_dt_unfilt[, date := as.Date(datetime)]
    
    # 2) Build complete sequence of dates (min -> max) and full turbine x date grid
    all_dates <- seq(min(scada_dt_unfilt$date, na.rm = TRUE),
                     max(scada_dt_unfilt$date, na.rm = TRUE),
                     by = "day")
    
    all_turbines <- unique(scada_dt_unfilt$turbinelabel)
    
    full_grid <- CJ(turbinelabel = all_turbines, date = all_dates)
    
    # 3) Count rows per turbine per day
    daily_counts <- scada_dt_unfilt[, .(n = .N), by = .(turbinelabel, date)]
    
    # 4) Merge with full grid to include missing days, fill with 0
    daily_complete <- merge(full_grid, daily_counts,
                            by = c("turbinelabel", "date"),
                            all.x = TRUE)
    
    daily_complete[is.na(n), n := 0]
    
    # 5) Pivot wide: turbines = rows, dates = columns (now includes ALL dates)
    wide <- dcast(
      daily_complete,
      turbinelabel ~ date,
      value.var = "n"
    )
    
    # 6) Write to Excel with conditional formatting (0 -> red)
    dir.create(folder_output_DC, recursive = TRUE, showWarnings = FALSE)
    
    wb <- createWorkbook()
    addWorksheet(wb, "daily_counts")
    writeData(wb, "daily_counts", wide)
    
    # Formatting
    freezePane(wb, "daily_counts", firstRow = TRUE, firstCol = TRUE)
    setColWidths(wb, "daily_counts", cols = 1:ncol(wide), widths = "auto")
    
    # Highlight zeros in red (data area only, excluding turbine column)
    data_rows <- 2:(nrow(wide) + 1)
    data_cols <- 2:ncol(wide)
    
    conditionalFormatting(
      wb, "daily_counts",
      cols = data_cols,
      rows = data_rows,
      rule = "==0",
      style = createStyle(bgFill = "#FF4D4D")
    )
    
    saveWorkbook(wb, file.path(folder_output_DC, "data-SCADA_range.xlsx"), overwrite = TRUE)

    

  ### Curtail data ----
    
    # curtl_dt_unfilt is a tibble; convert to data.table
    curtl_dt_unfilt <- as.data.table(curtl_dt_unfilt)
    
    # 1) Daily date from START (as requested)
    curtl_dt_unfilt[, date := as.Date(start)]
    
    # 2) Build complete sequence of dates (min -> max) and full turbine x date grid
    all_dates <- seq(min(curtl_dt_unfilt$date, na.rm = TRUE),
                     max(curtl_dt_unfilt$date, na.rm = TRUE),
                     by = "day")
    
    all_turbines <- unique(curtl_dt_unfilt$turbine)
    
    full_grid <- CJ(turbine = all_turbines, date = all_dates)
    
    # 3) Count rows per turbine per day
    daily_counts <- curtl_dt_unfilt[, .(n = .N), by = .(turbine, date)]
    
    # 4) Merge with full grid to include missing days, fill with 0
    daily_complete <- merge(full_grid, daily_counts,
                            by = c("turbine", "date"),
                            all.x = TRUE)
    
    daily_complete[is.na(n), n := 0]
    
    # 5) Pivot wide (turbines = rows, dates = columns) - now includes ALL dates
    wide <- dcast(
      daily_complete,
      turbine ~ date,
      value.var = "n"
    )
    
    # 6) Export to Excel + conditional formatting (0 -> red)
    dir.create(folder_output_DC, recursive = TRUE, showWarnings = FALSE)
    
    wb <- createWorkbook()
    addWorksheet(wb, "daily_counts")
    
    writeData(wb, "daily_counts", wide)
    
    # Nice formatting
    freezePane(wb, "daily_counts", firstRow = TRUE, firstCol = TRUE)
    setColWidths(wb, "daily_counts", cols = 1:ncol(wide), widths = "auto")
    
    # Conditional formatting: highlight zeros in red (data area only)
    data_rows <- 2:(nrow(wide) + 1)   # +1 because row 1 is header in Excel
    data_cols <- 2:ncol(wide)        # exclude first column (turbine)
    
    conditionalFormatting(
      wb, "daily_counts",
      cols = data_cols,
      rows = data_rows,
      rule = "==0",
      style = createStyle(bgFill = "#FF4D4D")  # red fill
    )
    
    saveWorkbook(wb, file.path(folder_output_DC, "data-Curtail_range.xlsx"), overwrite = TRUE)
    
    

  ### Tracks data ----

    # Ensure data.table
    setDT(track_dt_unfilt)
    
    # 1) Create daily date from timestamp
    track_dt_unfilt[, date := as.Date(timestamp)]
    
    # 2) Build complete sequence of dates (min -> max) and full turbine x date grid
    all_dates <- seq(min(track_dt_unfilt$date, na.rm = TRUE),
                     max(track_dt_unfilt$date, na.rm = TRUE),
                     by = "day")
    
    all_turbines <- unique(track_dt_unfilt$turbine)
    
    full_grid <- CJ(turbine = all_turbines, date = all_dates)
    
    # 3) Count rows per turbine per day
    daily_counts <- track_dt_unfilt[, .(n = .N), by = .(turbine, date)]
    
    # 4) Merge with full grid to include missing days, fill with 0
    daily_complete <- merge(full_grid, daily_counts,
                            by = c("turbine", "date"),
                            all.x = TRUE)
    
    daily_complete[is.na(n), n := 0]
    
    # 5) Pivot wide (turbines = rows, dates = columns) - now includes ALL dates
    wide <- dcast(
      daily_complete,
      turbine ~ date,
      value.var = "n"
    )
    
    # 6) Export to Excel + conditional formatting (0 -> red)
    dir.create(folder_output_DC, recursive = TRUE, showWarnings = FALSE)
    
    wb <- createWorkbook()
    addWorksheet(wb, "daily_counts")
    
    writeData(wb, "daily_counts", wide)
    
    # Formatting
    freezePane(wb, "daily_counts", firstRow = TRUE, firstCol = TRUE)
    setColWidths(wb, "daily_counts", cols = 1:ncol(wide), widths = "auto")
    
    # Highlight 0 values in red (exclude turbine column)
    data_rows <- 2:(nrow(wide) + 1)
    data_cols <- 2:ncol(wide)
    
    conditionalFormatting(
      wb, "daily_counts",
      cols = data_cols,
      rows = data_rows,
      rule = "==0",
      style = createStyle(bgFill = "#FF4D4D")
    )
    
    saveWorkbook(wb, file.path(folder_output_DC, "data-Tracks_range.xlsx"), overwrite = TRUE)
