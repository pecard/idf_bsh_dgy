##
##  Tier3 start date calendar ----
##
  
  ##Needed packages
  packages <- c('dplyr','lubridate','ggplot2','tidyr','forcats') 
  
  ##Check and install packages that are missing + call library()
  for (p in packages) {
    if (!require(p, character.only = TRUE)) install.packages(p)
    library(p, character.only = TRUE) }
  
  
  
  
  # 1) Agregar arranques por dia ----
  daily_counts <- 
    tier3 %>%
    mutate(start_date = as.Date(timestamp)) %>%
    count(date = start_date, name = "n_starts")
  
  
  # Zero isolated 
  # 2) Criar grade diária contínua do período (para mostrar dias sem arranques) ----
  full_calendar <- 
    tibble(date = seq(as.Date(ini), as.Date(end), by = "day")) %>%
    left_join(daily_counts, by = "date") %>%
    mutate(n_starts = replace_na(n_starts, 0))
  
  # (exemplo para garantir weekday_lab EN 1.ª letra e week_of_month inteiro)
  full_calendar <- 
    full_calendar %>%
    mutate(
      month = lubridate::month(date, label = TRUE, abbr = TRUE),
      month_start = lubridate::floor_date(date, "month"),
      wday_month_start = lubridate::wday(month_start, week_start = 1),
      week_of_month = as.integer(1 + ((mday(date) + wday_month_start - 2) %/% 7)),
      # 1.ª letra do dia (EN)
      weekday_lab = lubridate::wday(date, label = TRUE, abbr = TRUE, week_start = 1, locale = "en_US"),
      ym = fct_inorder(paste0(year(date), "-", sprintf("%02d", month(date))))
    )
  
  # separar dados (zero vs >=1)
  cal_zero <- filter(full_calendar, n_starts == 0)
  cal_pos  <- filter(full_calendar, n_starts > 0)
  
  p <- ggplot() +
    # 1) dias com zero (cinzento, sem legenda)
    geom_tile(
      data = cal_zero,
      aes(x = weekday_lab, y = week_of_month),
      fill = "grey60", color = "grey85", linewidth = 0.2
    ) +
    # 2) dias com >=1 (gradiente + legenda)
    geom_tile(
      data = cal_pos,
      aes(x = weekday_lab, y = week_of_month, fill = n_starts),
      color = "grey85", linewidth = 0.2
    ) +
    facet_wrap(~ ym, ncol = 4, scales = "free_y") +
    scale_fill_gradient(
      name = "WTG in\nTier 3 per day",
      low  = "#e0f3db",
      high = "#0868ac"
    ) +
    scale_y_reverse(breaks = seq(1, 6, 1)) +
    labs(
      x = NULL, y = "Week month",
      title = "Daily number of WTG starting at Tier 3 strategy",
      subtitle = paste(format(min(full_calendar$date), "%d %b %Y"), "to",
                       format(max(full_calendar$date), "%d %b %Y")),
      caption = "Grey = no starts; Colour scale = number of WTG starting Tier 3"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid = element_blank(),
      strip.background = element_rect(fill = "grey95", color = NA),
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)
    )
  
 
  ggsave(file.path(folder_output, "WTG_tier3_start_date_cal.png"), p, width = 12, height = 14, bg="white")
  
  
  
  