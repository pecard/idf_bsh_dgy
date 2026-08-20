##
## Sumario "zona critica" -- junta, numa unica tabela por turbina de
## interesse (ex: fatality_incidents$turbine), o contributo marginal de
## curtailments (R/curtailment_cluster_patterns.R,
## summarise_turbine_marginal_contribution()) com o ranking do cluster de
## atividade da especie (R/track_species_clusters.R,
## summarise_turbine_species_cluster_rank()).
##
## A pergunta que motivou isto (Paulo): esta turbina esta numa zona critica
## -- com muitos movimentos da especie analisada E muitos curtailments? Sao
## 2 eixos independentes (uma turbina pode ter muitos curtailments SEM
## estar numa zona de muito movimento da especie escolhida, se a maior
## parte dos curtailments forem de outras especies, e vice-versa) -- por
## isso ficam as 2 colunas de ranking lado a lado, para uma leitura direta,
## sem ter de cruzar 2 tabelas separadas.
##
## Depende de: data.table
##
## Uso:
##   source("R/turbine_critical_zone_summary.R")
##
##   critical_zone_dt <- summarise_turbine_critical_zone(marginal_stat_dt, species_cluster_rank_stat_dt)
##

summarise_turbine_critical_zone <- function(marginal_dt, species_cluster_rank_dt) {

  out <- merge(marginal_dt, species_cluster_rank_dt, by = c("turbine", "cluster_id"), all = TRUE)
  data.table::setcolorder(out, c(
    "turbine", "cluster_id", "n_turbines_in_cluster",
    "n_total", "pct_of_cluster", "cluster_rank", "n_clusters",
    "track_n_tracks", "track_pct_of_total", "track_cluster_rank", "track_n_clusters"
  ))
  out[]
}
