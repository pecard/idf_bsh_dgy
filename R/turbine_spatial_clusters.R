##
## Distancias entre turbinas e clusters espaciais (com restricao de
## distancia "dura") -- infraestrutura partilhada pelos 2 componentes da
## seccao 10:
##   10.1 -- padroes espaciais/temporais de curtailments por cluster de
##           turbinas (R/curtailment_cluster_patterns.R)
##   10.2 -- ocorrencia de tracks de uma especie (kestrel por omissao) por
##           cluster de turbinas (R/track_species_clusters.R)
##
## Metodo de cluster: distancia 2D planar (wtg ja em crs_projection_plannar,
## metros) + single-linkage hierarchical clustering, cortado a uma altura =
## limiar de distancia (cluster_max_dist_m em userSettings_BSH.R).
## Single-linkage e' a forma direta de implementar uma "restricao dura de
## distancia": 2 turbinas ficam no MESMO cluster se houver uma cadeia de
## turbinas vizinhas, cada par consecutivo a <= cluster_max_dist_m -- NAO
## exige que TODOS os pares dentro do cluster cumpram o limiar (isso seria
## complete-linkage, um conceito de "diametro maximo do cluster" diferente
## e mais restritivo). Equivale a componentes conexas de um grafo de
## proximidade (aresta entre 2 turbinas se distancia <= limiar).
##
## Ha tambem uma via de clusters MANUAIS (setores definidos a olho a partir
## do layout da quinta, manual_turbine_clusters em userSettings_BSH.R) --
## fica no mesmo formato de saida (turbine, cluster_id) que
## cluster_turbines_by_distance(), para as funcoes de sumario em
## R/curtailment_cluster_patterns.R e R/track_species_clusters.R serem
## reutilizaveis para os 2 tipos de cluster sem alteracao.
##
## turbine_cluster_threshold_sensitivity() segue a mesma logica de
## threshold sweep ja usada em R/id_transitions.R
## (id_transition_late_time/dist_sensitivity()) -- em vez de escolher
## cluster_max_dist_m as cegas, corre-se o sweep e ve-se onde o nº de
## clusters/silhouette estabiliza.
##
## Depende de: data.table, sf, cluster (silhouette), ggplot2
##
## Uso:
##   source("R/turbine_spatial_clusters.R")
##
##   dist_mat   <- turbine_distance_matrix(wtg)
##   cluster_dt <- cluster_turbines_by_distance(dist_mat, max_dist_m = cluster_max_dist_m)
##   sens_dt    <- turbine_cluster_threshold_sensitivity(dist_mat, thresholds_m = cluster_threshold_sweep_m)
##   manual_dt  <- manual_turbine_clusters_dt(manual_turbine_clusters)
##
##   p_map <- plot_turbine_clusters_map(wtg, cluster_dt, highlight_turbines = fatality_incidents$turbine)
##

## 1. Matriz de distancias 2D entre turbinas (metros) ----

turbine_distance_matrix <- function(wtg_sf, wtg_id_col = "InternalNa") {

  # st_coordinates() devolve X,Y,Z se a geometria de wtg_sf for 3D (o
  # shapefile de turbinas tem elevacao, usada na cobertura 3D --
  # R/coverage_3d_topography.R ja indexa por nome "X"/"Y" pela mesma razao)
  # -- selecionar so' X,Y explicitamente para garantir distancia horizontal
  # 2D, nao 3D (que incluiria a diferenca de elevacao entre turbinas)
  coords   <- sf::st_coordinates(wtg_sf)[, c("X", "Y")]
  turbines <- wtg_sf[[wtg_id_col]]

  d <- as.matrix(stats::dist(coords, method = "euclidean"))
  dimnames(d) <- list(turbines, turbines)
  d
}


## 2. Clusters espaciais por restricao de distancia (single-linkage) ----

cluster_turbines_by_distance <- function(dist_mat, max_dist_m) {

  hc <- stats::hclust(stats::as.dist(dist_mat), method = "single")
  cl <- stats::cutree(hc, h = max_dist_m)

  data.table::data.table(
    turbine    = names(cl),
    cluster_id = sprintf("C%02d", as.integer(cl))
  )
}


## 3. Sensibilidade ao limiar de distancia -- nº de clusters + silhouette
##    media, para escolher cluster_max_dist_m de forma defensavel
##    (silhouette so' e' definida quando ha' mais de 1 cluster e nem todos
##    os pontos sao singleton -- caso contrario fica NA)
turbine_cluster_threshold_sensitivity <- function(dist_mat, thresholds_m) {

  n_turbines <- nrow(dist_mat)

  res <- lapply(thresholds_m, function(th) {
    hc <- stats::hclust(stats::as.dist(dist_mat), method = "single")
    cl <- stats::cutree(hc, h = th)
    n_clusters   <- length(unique(cl))
    n_singletons <- sum(table(cl) == 1L)

    mean_sil <- if (n_clusters > 1L && n_clusters < n_turbines) {
      # dist= explicito (nao a matriz "crua") para nao depender de como
      # cluster::silhouette() decide coagir um argumento posicional
      sil <- cluster::silhouette(cl, dist = stats::as.dist(dist_mat))
      round(mean(sil[, "sil_width"]), 3)
    } else {
      NA_real_
    }

    data.table::data.table(
      threshold_m            = th,
      n_clusters              = n_clusters,
      n_singleton_clusters    = n_singletons,
      mean_silhouette_width   = mean_sil
    )
  })

  data.table::rbindlist(res)
}


## 4. Clusters manuais (setores definidos a olho) -- mesmo formato de saida
##    que cluster_turbines_by_distance(), para reutilizar as mesmas funcoes
##    de sumario com qualquer um dos 2 tipos de cluster
manual_turbine_clusters_dt <- function(manual_clusters_list) {

  data.table::rbindlist(lapply(names(manual_clusters_list), function(nm) {
    data.table::data.table(turbine = manual_clusters_list[[nm]], cluster_id = nm)
  }))
}


## 5. Mapa de referencia -- turbinas no espaco, coloridas pelo cluster a que
##    pertencem (estatistico OU manual, o mesmo cluster_dt de sempre) --
##    para se ver a olho se os clusters fazem sentido geograficamente antes
##    de confiar nas tabelas. coord_equal() e' importante aqui (senao a
##    escala X/Y distorce-se e o mapa fica enganador). Turbinas de interesse
##    (ex: fatality_incidents$turbine) ficam marcadas com triangulo maior.
plot_turbine_clusters_map <- function(wtg_sf, cluster_dt, wtg_id_col = "InternalNa",
                                      highlight_turbines = NULL, title = "Clusters de turbinas") {

  coords <- sf::st_coordinates(wtg_sf)[, c("X", "Y"), drop = FALSE]
  dt <- data.table::data.table(turbine = wtg_sf[[wtg_id_col]], x = coords[, "X"], y = coords[, "Y"])
  dt[cluster_dt, on = "turbine", cluster_id := i.cluster_id]
  dt[, cluster_id := data.table::fifelse(is.na(cluster_id), "(sem cluster)", cluster_id)]
  dt[, is_highlight := turbine %in% highlight_turbines]

  ggplot2::ggplot(dt, ggplot2::aes(x = x, y = y, color = cluster_id)) +
    ggplot2::geom_point(ggplot2::aes(size = is_highlight, shape = is_highlight)) +
    ggplot2::geom_text(
      ggplot2::aes(label = turbine), size = 2.5, vjust = -0.8, show.legend = FALSE
    ) +
    ggplot2::scale_size_manual(values = c(`TRUE` = 4, `FALSE` = 2.2), guide = "none") +
    ggplot2::scale_shape_manual(values = c(`TRUE` = 17, `FALSE` = 16), guide = "none") +
    ggplot2::scale_color_viridis_d() +
    ggplot2::coord_equal() +
    ggplot2::labs(x = "UTM X (m)", y = "UTM Y (m)", color = "Cluster", title = title) +
    ggplot2::theme_minimal()
}
