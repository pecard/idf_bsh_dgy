##
## ID transitions per single track
##

    #Track: species ID
    p <-
      track_dt %>%
      group_by(track_id) %>%
      summarise(n = length(unique(spec))) %>%
      arrange(desc(n)) %>%
      ggplot() +
      geom_bar(aes(n)) +
      labs(x=' Num species/single track')
    ggsave(file.path(folder_output, "ID_transition_plot.png"), plot = p, width = 8, height = 6, dpi = 300, bg = "white")
    
    transitions_dt <- p$data #needed for later
    
    transitions <- p$data %>%
      count(n) %>%
      mutate(perc = nn/sum(nn)*100) 
    
    id_transition_rate <- transitions %>%
      summarise(
        total_tracks = sum(nn),
        tracks_with_transition = sum(nn[n >= 2]),
        ID_transition_rate = tracks_with_transition / total_tracks,
        ID_transition_rate_pct = 100 * ID_transition_rate
      )
    write_xlsx(id_transition_rate, file.path(folder_output,"ID_transition_metrics.xlsx"))
    
    
    #Indice de Entropia H (Shannon)
    # H = 0 --> nao ha transicoes =estável
    # H > 0 --> há mais que 1 transicao; quanto maior o indice mais variacao
    
    track_entropy <- track_dt %>%
      filter(!is.na(spec)) %>%
      count(track_id, spec, name = "n_spec") %>%
      group_by(track_id) %>%
      mutate(
        p_i = n_spec / sum(n_spec),
        shannon_entropy = -sum(p_i * log(p_i))
      ) %>%
      summarise(
        n_ids = n(),
        shannon_entropy = first(shannon_entropy),
        .groups = "drop"
      )
    
    #summary of metrics of entropy
    H_summary <- track_entropy %>%
      summarise(
        n_tracks = n(),
        mean_entropy = mean(shannon_entropy, na.rm = TRUE),
        median_entropy = median(shannon_entropy, na.rm = TRUE)
      )
    write_xlsx(H_summary, file.path(folder_output,"ID_entropy_metrics.xlsx"))
    
    
    ggplot(track_entropy, aes(x = shannon_entropy)) +
      geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black") +
      labs(
        x = "Shannon entropy index",
        y = "Number of tracks",
        title = "Distribution of track ID entropy"
      ) +
      theme_minimal()
    
    
    
   remove(p,H_summary,track_entropy,id_transition_rate,transitions) #do not remove transitions_dt --> needed for later 
    
