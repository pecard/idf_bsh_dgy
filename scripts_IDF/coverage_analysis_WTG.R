###
### A. Cobertura de volume detection area - 1000m raio ----
###

    
      #### HARD CODE ####
      
      ###... A.1. 3D coverage ----
      #cylinder mesh
      mesh_cell_size <- 100
      cell_prox_thresh_m <- 55 #considerar nos pontos todas as rotas que passam a 55m
      wtg_cylind_height <- coverage_cylinder_height
      wtg_cylind_radius <- coverage_cylinder_wider_radius
      
      ##Analise 3D coverage
      source(file.path(folder_script_IDF, 'coverage_3D.R'))



###
### B. Cobertura de volume inner cylinder - 600m raio ----
###
    
      ###... B.1. 3D coverage ----
      #cylinder mesh
      mesh_cell_size <- 100
      cell_prox_thresh_m <- 55
      wtg_cylind_height <- coverage_cylinder_height
      wtg_cylind_radius <- coverage_cylinder_inner_radius
      
      ##Analise 3D coverage
      source(file.path(folder_script_IDF, 'coverage_3D.R'))



###
### C. Cobertura de volume com maior resolucao inner cylinder - 600m --> para fazer a analise blind areas
###
      
    
      ###... C.1. 3D coverage ----
      #cylinder mesh
      mesh_cell_size <- 10 #malha mais apertada para ter maior resolucao visual
      cell_prox_thresh_m <- 10 
      wtg_cylind_height <- coverage_cylinder_height
      wtg_cylind_radius <- coverage_cylinder_inner_radius
      
      ##Analise 3D coverage
      source(file.path(folder_script_IDF, 'coverage_3D.R'))
      
      
      
      ###... C.2. Blind spots ----
      #analise feita visualmente selecionando as turbinas com coverage mais baixos 
      #--> ver pasta de output idf_3D_coverage/html 
      
      