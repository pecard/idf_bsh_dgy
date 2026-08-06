##
## Classification performance - confusion matrix analysis
##


    
    ##Needed packages
    packages <- c('purrr','rstudioapi', #purrr needed for citation; rstudioapi needed for dynamic atribution of working directory with getActiveDocumentContext()
                  'ggplot2','readxl','caret','dplyr','pROC','igraph','ggraph','tidyr') 
    
    ##Check and install packages that are missing + call library()
    for (p in packages) {
      if (!require(p, character.only = TRUE)) install.packages(p)
      library(p, character.only = TRUE) }
    

    folder_output_ID <-paste0(folder_output,"\\",format(Sys.time(), "%Y%m%d"),"\\","ID_perform\\")
    dir.create(folder_output_ID, showWarnings = FALSE, recursive = TRUE)
    
    
    
##
## Data
##
    
    #Labels excluidos da analise
    subsample_total <- read_excel(file.path(folder_input,"subsample_total.xlsx")) #para ver que labels vao ser excluidas devido a baixa representatividade
    excluded_sp <- subsample_total %>%
      filter(!is.na(Notas) & grepl("Não validado", Notas)) %>%
      distinct(`IDF label`) %>%
      pull()
    
    
    #Read subsample_valid excel files
    files <- list.files(
      path = file.path(databases_dir,folder_subsample),
      pattern = "valid\\.xlsx$",
      full.names = TRUE,
      recursive = TRUE
    )
    
    #read excel file
    data <- files %>%
      lapply(read_excel) %>%
      bind_rows()
    
    #remove excluded labels due to low sample
    data <- data %>%
      filter(!tolower(SpeciesTypeName) %in% tolower(excluded_sp))
    
    
##
## 1. Validação classif. Priority/Non-Priority ----
##
    
    #Ajuste classes predicted e actual para bird/not-bird
    data$prio_actual <- data$sp_valid
    data$prio_actual <- ifelse(
      data$prio_actual %in% prioritysp,
      "Priority",
      "Non-Priority"
    )
    
    data$prio_pred <- data$SpeciesTypeName
    data$prio_pred <- ifelse(
      data$prio_pred %in% prioritysp,
      "Priority",
      "Non-Priority"
    )
    
    
    #Create data.frame    
    cm <- as.data.frame(table(Predicted = data$prio_pred, Actual = data$prio_actual))
    
    #plot
    ggplot(cm, aes(x=Actual, y=Predicted, fill=Freq)) +
      geom_tile() +
      geom_text(aes(label=Freq)) +
      scale_fill_gradient(low="white", high="steelblue")
    
    cm_tbl <- table(Predicted = data$prio_pred, Actual = data$prio_actual)
    
    #Create confusion matrix - use validated as reference
    cm_res <- confusionMatrix(
      data = factor(data$prio_pred, levels = c("Priority", "Non-Priority")),
      reference = factor(data$prio_actual, levels = c("Priority", "Non-Priority")),
      positive = "Priority"
    )    
    capture.output(cm_res, file = file.path(folder_output_ID,"P-NP_confusionMatrix.txt"))
    
    
    
    
    #Confirmar confusion matrix está certo
    TP <- cm_tbl["Priority","Priority"]
    TN <- cm_tbl["Non-Priority","Non-Priority"]
    FP <- cm_tbl["Priority","Non-Priority"]
    FN <- cm_tbl["Non-Priority","Priority"]
    
    precision <- TP / (TP + FP)
    recall    <- TP / (TP + FN)
    specificity <- TN / (TN + FP)
    accuracy <- (TP + TN) / sum(cm_tbl)
    F1 <- 2 * (precision * recall) / (precision + recall)
    
    results <- data.frame(
      TP = TP,
      TN = TN,
      FP = FP,
      FN = FN,
      Precision = precision,
      Recall = recall,
      Specificity = specificity,
      Accuracy = accuracy,
      F1 = F1
    )
    
    writexl::write_xlsx(results, file.path(folder_output_ID,"P-NP_confusion_metrics.xlsx"))
    
    
    
##
## 2. Validação classif. especies ----
##
    
    #Nova colunas
    data$sp_actual <- data$sp_valid
    data$sp_pred <- data$SpeciesTypeName
    
    #total por label
    pred_counts <- data %>%
      count(sp_pred, name = "n")
    actual_counts <- data %>%
      count(sp_actual, name = "n")
    data %>%
      count(sp_pred, sp_actual)
    
    #Create data.frame    
    cm <- as.data.frame(table(Predicted = data$sp_pred, Actual = data$sp_actual))
    all_levels <- union(unique(data$sp_pred), unique(data$sp_actual))
    
    data$sp_pred   <- factor(data$sp_pred, levels = all_levels)
    data$sp_actual <- factor(data$sp_actual, levels = all_levels)
    
    #plot
    cm$Correct <- as.character(cm$Predicted) == as.character(cm$Actual)
    p <- ggplot(cm, aes(x = Actual, y = Predicted, fill = Freq)) +
      geom_tile(color = "grey80") +
      geom_tile(data = subset(cm, Correct), fill = NA, color = "red", size = 1.2) +
      geom_text(aes(label = Freq)) +
      scale_fill_gradient(low = "white", high = "steelblue") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    
    ggsave(file.path(folder_output_ID,"SP_correct_matrix.png"), plot=p, width = 8, height = 6, dpi = 300, bg='white')
    
    
    #Create confusion matrix - use actual as reference
    cm_res <- confusionMatrix(
      data = as.factor(data$sp_pred),
      reference = as.factor(data$sp_actual)
    )
    capture.output(cm_res, file = file.path(folder_output_ID,"SP_confusionMatrix.txt"))
    
    
    #Export to excel
    cm_res <- confusionMatrix(
      data = as.factor(data$sp_pred),
      reference = as.factor(data$sp_actual)
    )
    
    cm_table <- as.matrix(cm_res$table)
    classes <- colnames(cm_table)
    
    # Compute TP, TN, FP, FN per class
    stats_list <- lapply(classes, function(cls) {
      TP <- cm_table[cls, cls]
      FP <- sum(cm_table[cls, ]) - TP
      FN <- sum(cm_table[, cls]) - TP
      TN <- sum(cm_table) - TP - FP - FN
      
      data.frame(
        Class = cls,
        TP = TP,
        TN = TN,
        FP = FP,
        FN = FN
      )
    })
    
    conf_stats <- do.call(rbind, stats_list)
    
    # Add metrics
    conf_stats <- conf_stats %>%
      mutate(
        Precision   = ifelse((TP + FP) == 0, NA, TP / (TP + FP)),
        Recall      = ifelse((TP + FN) == 0, NA, TP / (TP + FN)),
        Specificity = ifelse((TN + FP) == 0, NA, TN / (TN + FP)),
        Accuracy    = (TP + TN) / (TP + TN + FP + FN),
        F1 = case_when(
          Class %in% excluded_sp ~ NA_real_,  # colocar como NA se nao tiver sido analisado
          is.na(Precision) | is.na(Recall) | (Precision + Recall) == 0 ~ 0,
          TRUE ~ 2 * Precision * Recall / (Precision + Recall)
        )
      )
    
    # Add performance level before rounding
    conf_stats <- conf_stats %>%
      mutate(
        Performance = case_when(
          Class %in% excluded_sp ~ "NA",  
          F1 == 0 ~ "Very poor",                   
          F1 > 0   & F1 <= 0.6 ~ "Poor",
          F1 > 0.6 & F1 <= 0.8 ~ "Moderate",
          F1 > 0.8 ~ "Good"
        )
      )
    
    # Round numeric columns
    conf_stats <- conf_stats %>%
      mutate(
        across(where(is.numeric), ~ round(., 2))
      )
    
    # Export to Excel
    writexl::write_xlsx(
      list(Class_Statistics = conf_stats),
      path = file.path(folder_output_ID,"SP_confusion_metrics.xlsx")
    )
    
    
    
    #Indicar quais a top3 especies que confude para cada label
    cm_errors <- cm %>%
      filter(as.character(Predicted) != as.character(Actual))
    top_confusions <- cm_errors %>%
      group_by(Predicted) %>%
      mutate(TotalPred = sum(Freq),
             Percent = Freq / TotalPred) %>%
      arrange(Predicted, desc(Freq)) %>%
      slice_head(n = 3) %>%
      ungroup()
    
    writexl::write_xlsx(top_confusions, file.path(folder_output_ID, "SP_top3_confusions.xlsx"))
    
    top_confusions$Label <- paste0(top_confusions$Actual, " (", top_confusions$Freq, ")")
    top_confusions$Label <- paste0(
      top_confusions$Actual,
      " (", round(100 * top_confusions$Percent, 1), "%)"
    )
    top_confusions_priority <- top_confusions %>%
      filter(Predicted %in% prioritysp)
    
    top_confusions_priority$Label <- paste0(
      top_confusions_priority$Actual,
      " (", round(100 * top_confusions_priority$Percent, 1), "%)"
    )
    top_confusions_priority$Label <- paste0(
      top_confusions_priority$Actual,
      " (", round(100 * top_confusions_priority$Percent, 1), "%)"
    )
    
    p <- ggplot(top_confusions_priority,
                aes(x = reorder(Label, Freq), y = Freq)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      facet_wrap(~ Predicted, scales = "free_y") +
      labs(
        title = "Top 3 Confusions per Priority Bird species",
        x = "Actual (Confused with)",
        y = "Count"
      ) +
      theme_minimal()
    
    ggsave(file.path(folder_output_ID,"SP_top_confusions_Priority.png"), plot=p, width = 10, height = 8, dpi = 300, bg='white')
    
    
    
    
    
    
    
    #plot falsos positivos e falsos negativos
    error_df <- do.call(rbind, lapply(classes, function(cls) {
      
      TP <- cm_tbl[cls, cls]
      FP <- sum(cm_tbl[cls, ]) - TP
      FN <- sum(cm_tbl[, cls]) - TP
      
      data.frame(
        Species = cls,
        FP = FP,
        FN = FN
      )
    }))
    error_long <- error_df %>%
      pivot_longer(cols = c(FP, FN),
                   names_to = "ErrorType",
                   values_to = "Count")
    ggplot(error_long, aes(x = reorder(Species, Count), y = Count, fill = ErrorType)) +
      geom_col(position = "dodge") +
      coord_flip() +
      scale_fill_manual(values = c(
        "FP" = "red",
        "FN" = "blue"
      )) +
      labs(
        title = "False Positives and False Negatives per Species",
        x = "Species",
        y = "Count"
      )
    
    ##Confusion network plot
    edges <- cm %>%
      filter(as.character(Predicted) != as.character(Actual),
             Freq > 0)
    g <- graph_from_data_frame(
      d = edges,
      directed = TRUE,
      vertices = unique(c(edges$Predicted, edges$Actual))
    )
    p <-ggraph(g, layout = "fr") +
      geom_edge_link(aes(width = Freq), alpha = 0.6) +
      geom_node_point(size = 5, color = "steelblue") +
      geom_node_text(aes(label = name), repel = TRUE, size = 3) +
      theme_void()
    
    ggsave(file.path(folder_output_ID,"SP_confusion_network.png"), plot=p, width = 10, height = 8, dpi = 300, bg='white')
    
    
    
    #By species performance
    cm_tbl <- table(Predicted = data$sp_pred, Actual = data$sp_actual)
    classes <- rownames(cm_tbl)
    metrics_list <- lapply(classes, function(cls) {
      
      TP <- cm_tbl[cls, cls]
      FP <- sum(cm_tbl[cls, ]) - TP
      FN <- sum(cm_tbl[, cls]) - TP
      TN <- sum(cm_tbl) - TP - FP - FN
      
      precision <- ifelse((TP + FP) == 0, NA, TP / (TP + FP))
      recall    <- ifelse((TP + FN) == 0, NA, TP / (TP + FN))
      F1 <- ifelse(is.na(precision) | is.na(recall) | (precision + recall) == 0,
                   0,
                   2 * precision * recall / (precision + recall))
      
      data.frame(
        Species = cls,
        TP = TP,
        FP = FP,
        FN = FN,
        TN = TN,
        Precision = precision,
        Recall = recall,
        F1 = F1
      )
    })
    
    metrics_df <- do.call(rbind, metrics_list)
    
    
##    
## 3. How well did IDF predict each species? -----
##  
    
    
    #thresholds
    metrics_df$Performance <- cut(
      metrics_df$F1,
      breaks = c(-Inf, 0, 0.6, 0.8, Inf),
      labels = c("Very Poor**", "Poor", "Moderate", "Good"),
      right = TRUE
    )
    
    #marcar as priority species
    metrics_df$Species_label <- ifelse(
      metrics_df$Species %in% prioritysp,
      paste0(metrics_df$Species, " *"),
      metrics_df$Species
    )
    metrics_df <- metrics_df %>%
      filter(!Species %in% excluded_sp)
    
    #ordenar por species
    metrics_df$Species_label <- factor(
      metrics_df$Species_label,
      levels = rev(sort(metrics_df$Species_label))
    )
    
    #force levels
    metrics_df$Performance <- factor(
      metrics_df$Performance,
      levels = c("Very Poor**", "Poor", "Moderate", "Good")
    )
    
    p <- ggplot(metrics_df, aes(x = Species_label, y = F1, fill = Performance)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c(
        "Good" = "darkgreen",
        "Moderate" = "orange",
        "Poor" = "red",
        "Very Poor**" = "grey80"
      ), drop = FALSE) +
      labs(title = "Classifier Performance per Species",
           y = "F1 Score", x = "Species") +
      geom_text(aes(label = ifelse(F1 == 0, "**", "")),
                hjust = 0.1, color = "black", size = 3) +
      labs(subtitle = "** = No or very low correct prediction (F1 = 0)") +
      ylim(0, 1)   # force axis to max = 1
    
    ggsave(file.path(folder_output_ID,"SP_performance-F1score.png"), plot=p, width = 8, height = 6, dpi = 300, bg='white')
    
    
    
    ##Priority sp only
    metrics_df_priority <- metrics_df %>%
      filter(Species %in% prioritysp,
             !Species %in% excluded_sp)
    metrics_df_priority$Performance <- factor(
      metrics_df_priority$Performance,
      levels = c("Very Poor**", "Poor", "Moderate", "Good")
    )
    
    p <- ggplot(metrics_df_priority, aes(x = Species, y = F1, fill = Performance)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c(
        "Good" = "darkgreen",
        "Moderate" = "orange",
        "Poor" = "red",
        "Very Poor**" = "grey80"
      )) +
      labs(
        title = "Classifier Performance (Priority Species)",
        subtitle = "** = No or very low correct prediction (F1 = 0)",
        y = "F1 Score",
        x = "Species"
      ) +
      geom_text(
        aes(label = ifelse(F1 == 0, "**", "")),
        hjust = 0.1, color = "black", size = 3
      ) +  
      ylim(0, 1)   # force axis to max = 1
    
    ggsave(file.path(folder_output_ID,"SP_performance-F1score_Priority.png"), plot=p, width = 8, height = 6, dpi = 300, bg='white')
    
    
    