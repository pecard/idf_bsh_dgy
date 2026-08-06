###
### Wind Farm IDF coverage
###

    

    #Efective WF coverage metrics
    summary <- data.frame(
    metric = c(
      "Number of IDF units",
      "Number of WTGs",
      "Total WTGs covered",
      "Proportion WTGs covered (%)"
    ),
      value = c(
        total_idf_units,
        nrow(wtg),
        total_turbines_covered,
        round((total_turbines_covered / nrow(wtg)) * 100, 2)
      )
    )
    write.xlsx(summary, file.path(folder_output,paste0("IDF_coverage_WF_summary.xlsx")), overwrite = TRUE)
    


    
    
    #Spatial Visualization
    
    #Creat 1000m buffer for each IDF
    idf_buf <- st_buffer(idf, dist = idf_op_detection_range)
    
    
    p <- ggplot() +
      # IDF buffer
      geom_sf(
        data = idf_buf,
        aes(fill = "IDF detection range"),
        colour = NA,
        alpha = 0.25
      ) +
      # IDF points
      geom_sf(
        data = idf,
        aes(colour = "IDF"),
        size = 2
      ) +
      # WTG points
      geom_sf(
        data = wtg,
        aes(colour = "WTG"),
        size = 1
      ) +
      # WTG labels (no legend entry)
      geom_sf_text(
        data = wtg,
        aes(label = ID),
        colour = "black",
        size = 2,
        nudge_y = 200,
        show.legend = FALSE
      ) +
      scale_colour_manual(
        name = "Points",
        values = c(
          "WTG" = "darkred",
          "IDF" = "black"
        )
      ) +
      scale_fill_manual(
        name = "",
        values = c(
          "IDF detection range" = "lightblue"
        )
      ) +
      theme_minimal()
    p
    
    
    ggsave(file.path(folder_output, paste0('IDF_coverage_WF.png')), 
           plot=p, width = 8, height = 6, dpi = 300, bg='white')
    
    remove(summary, p, idf_buf)
    