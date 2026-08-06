##
## plot with distribution of heights and speeds per Priority Bird species
##

    make_abbr <- function(x) {
      sapply(strsplit(x, "[- ]"), function(words) {
        paste0(toupper(substr(words, 1, 1)), collapse = "")
      })
    }
    
    track_plot <- track_dt %>%
      filter(spec %in% prioritysp) %>%
      filter(!is.na(speed_ms), height > 0, speed_ms < 100) %>%
      mutate(spec_abbr = make_abbr(spec)) %>%
      select(spec_abbr, track_id, speed_ms, count, height) %>%
      pivot_longer(
        cols = c(speed_ms, height),
        names_to = "variable",
        values_to = "value"
      )
    
    risk_zone <- data.frame(
      variable = "height",
      xmin = 50,
      xmax = 250,
      ymin = -Inf,
      ymax = Inf
    )
    
    p<- ggplot(track_plot, aes(x = value)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "black") +
      facet_grid(spec_abbr ~ variable, scales = "free_x") +
      theme_minimal() +
      labs(
        x = NULL,
        y = "Frequency",
        title = "Distribution of track metrics for priority species"
      ) +
      geom_rect(
        data = risk_zone,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        inherit.aes = FALSE,
        fill = "red",
        alpha = 0.15
      ) 
    
    ggsave(file.path(folder_output, "FlightSpeed&Height_prioritysp.png"), plot = p, width = 8, height = 10, dpi = 300, bg = "white")
