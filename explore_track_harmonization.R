##
## Script de exploracao -- harmonizacao/reconciliacao de tracks fragmentados
## (candidatos a MESMO individuo com track_id diferentes). NAO faz parte do
## pipeline de producao (IDF_analysis.R/IDF_monthly_report.R nunca o chamam).
##
## Motivacao (Paulo, 2026-08): estudo a parte dos relatorios BSH/DGY atuais,
## dos 3 padroes de fragmentacao identificados visualmente em dados BSH de
## 2026-08-07 -- handoff (fim de um track perto no tempo+espaco do inicio de
## outro), duplicado (2 unidades IDF registam o mesmo individuo em
## simultaneo, desviadas por erro de calibracao) e a combinacao dos 2. Ver
## R/track_harmonization.R para o metodo e a justificacao de cada limiar.
##
## Reutiliza a MESMA cache (fst) ja gravada por uma corrida anterior de
## run_annual_analysis_BSH.R -- NAO rele os ficheiros brutos, so' o dataset
## track_dt_unfilt. Se ainda nao correste o pipeline pelo menos 1 vez para
## BSH, corre isso primeiro (so precisas da cache, nao do relatorio docx
## completo).
##
## Uso:
##   1) Ajustar date_from/date_to abaixo se quiseres outro dia/janela.
##   2) Dar Source A ESTE FICHEIRO.
##   3) Ver reconciliation_summary_dt (1 linha por especie com >=1 aresta
##      candidata nesse dia) e edges_by_species (as arestas propriamente
##      ditas, com edge_type).
##   4) Para um grupo especifico que pareca suspeito, usar os exemplos no
##      fim do ficheiro -- inspect_reconciliation_group() para os dados
##      brutos por track (cruzar com o portal IdentiFlight antes de confiar
##      na fusao) e stitch_synthetic_tracks() para a reconstrucao
##      geometrica (plottar por cima do portal/outro GIS).
##

project_settings_file <- "userSettings_BSH.R"  ## ajustar para "userSettings_DGY.R" se for o caso

source(file.path("inputs", project_settings_file))
source("R/data_cache.R")
source("R/track_min_individuals.R")  # .uf_components()
source("R/track_harmonization.R")

## cache/<farm_code>/ (layout atual) -- se ainda nao existir, tenta cache/
## direto (layout anterior a' mudanca de 2026-08 que separou a cache por
## farm_code)
folder_cache <- file.path("cache", farm_code)
if (!file.exists(file.path(folder_cache, "track_dt_unfilt.fst")) && file.exists(file.path("cache", "track_dt_unfilt.fst"))) {
  message("Aviso: a usar cache/ (layout antigo, sem separacao por farm_code) -- nao encontrada em ", folder_cache, ". Corre run_annual_analysis_BSH.R outra vez para regravar no layout novo.")
  folder_cache <- "cache"
}

track_dt_unfilt <- load_or_read_cache(
  file.path(folder_cache, "track_dt_unfilt.fst"),
  function() stop("track_dt_unfilt.fst nao encontrado -- corre run_annual_analysis_BSH.R pelo menos 1 vez primeiro."),
  force_reread = FALSE, tz = proj_timezone
)


##
## Janela de estudo -- dia identificado pelo Paulo com casos bons para testar
## o metodo (2026-08). Ajustar aqui para explorar outro dia/periodo.
##

date_from <- as.POSIXct("2026-08-07 00:00:00", tz = proj_timezone)
date_to   <- as.POSIXct("2026-08-08 00:00:00", tz = proj_timezone)

track_dt_day <- track_dt_unfilt[timestamp >= date_from & timestamp < date_to]


##
## Limiares -- pontos de partida acordados com o Paulo (2026-08), a rever
## depois de olhar para os grupos encontrados neste dia (ver
## R/track_harmonization.R para a justificacao de cada um)
##
## duplicate_max_median_dist_m/duplicate_max_spread_m substituem o antigo
## duplicate_max_dist_m (2026-08, dados reais BSH unidades 31/32,
## Steppe-Eagle) -- o desvio de calibracao entre 2 unidades pode ir aos
## 130-180m e ainda ser o MESMO individuo; o que importa e' a ESTABILIDADE
## do desvio (max_spread_m), nao a sua magnitude (max_median_dist_m e' so'
## um teto de sanidade generoso). Ver R/track_harmonization.R.
##

handoff_time_window_sec      <- 30
handoff_max_dist_m           <- 50
duplicate_max_median_dist_m  <- 300
duplicate_max_spread_m       <- 50
duplicate_min_overlap_frac   <- 0.8
duplicate_min_overlap_sec    <- 10


##
## Corre a reconciliacao por especie -- so' guarda especies com pelo menos 1
## aresta candidata (a maioria das especies num dia normal nao tem nenhuma
## fragmentacao detetada com estes limiares)
##

species_sel <- sort(unique(track_dt_day$spec))
species_sel <- species_sel[!is.na(species_sel)]

reconciliation_by_species <- lapply(species_sel, function(sp) {
  
  handoff_edges_dt <- find_handoff_edges(
    track_dt_day, sp,
    time_window_sec = handoff_time_window_sec, max_dist_m = handoff_max_dist_m
  )
  duplicate_edges_dt <- find_duplicate_edges(
    track_dt_day, sp,
    max_median_dist_m = duplicate_max_median_dist_m,
    max_spread_m      = duplicate_max_spread_m,
    min_overlap_frac  = duplicate_min_overlap_frac,
    min_overlap_sec   = duplicate_min_overlap_sec
  )
  
  rec <- build_reconciliation_groups(track_dt_day, sp, handoff_edges_dt, duplicate_edges_dt)
  
  list(species = sp, groups = rec$groups, edges = rec$edges, duplicate_edges = duplicate_edges_dt)
})
names(reconciliation_by_species) <- species_sel

reconciliation_by_species <- Filter(function(x) nrow(x$edges) > 0L, reconciliation_by_species)

if (length(reconciliation_by_species) == 0L) {
  
  message("Nenhuma aresta candidata (handoff/duplicate) encontrada em ", date_from, " - ", date_to, " com os limiares atuais.")
  
} else {
  
  reconciliation_summary_dt <- data.table::rbindlist(lapply(reconciliation_by_species, function(x) {
    s <- summarise_reconciliation(x$groups)
    s[, `:=`(
      spec              = x$species,
      n_handoff_edges   = sum(x$edges$edge_type == "handoff"),
      n_duplicate_edges = sum(x$edges$edge_type == "duplicate")
    )]
    data.table::setcolorder(s, "spec")
    s
  }))
  data.table::setorder(reconciliation_summary_dt, -n_merged_synth_tracks)
  
  edges_by_species <- data.table::rbindlist(lapply(reconciliation_by_species, function(x) {
    e <- data.table::copy(x$edges)
    e[, spec := x$species]
    e
  }))
  data.table::setcolorder(edges_by_species, "spec")
  
  print(reconciliation_summary_dt)
  print(edges_by_species)
}


##
## Diagnostico -- especies que o Paulo identificou visualmente (2026-08-07)
## mas que NAO apareceram acima (Griffon-Vulture/Cinereous-Vulture/
## Steppe-Eagle nao passaram find_handoff_edges()/find_duplicate_edges() com
## os limiares atuais). Antes de mexer nos limiares as cegas outra vez: (1)
## confirmar que a especie sequer tem tracks neste dia, com este nome exato;
## (2) se tiver, ver os numeros REAIS a' volta dos pares que ja se sabe
## deverem ligar-se, sem filtro nenhum de limiar -- ver R/track_harmonization.R
## secção 6 (diagnose_handoff_candidates/diagnose_overlap_candidates).
##

print(track_dt_day[, .N, by = spec][order(-N)])

species_to_debug <- c("Bearded-Vulture", "Cinereous-Vulture", "Steppe-Eagle")

for (sp in species_to_debug) {
  
  cat(sprintf("\n===== %s -- handoff candidates (todos os pares, sem limiar) =====\n", sp))
  print(diagnose_handoff_candidates(track_dt_day, sp))
  
  cat(sprintf("\n===== %s -- overlap candidates (todos os pares com sobreposicao temporal) =====\n", sp))
  print(diagnose_overlap_candidates(track_dt_day, sp))
}


#
# Plot interativo (plotly) -- ver R/track_harmonization.R secção 7,
# plot_candidate_tracks(). Funciona com QUALQUER tabela de arestas com
# colunas track_id_a/track_id_b -- as filtradas (find_handoff_edges/
# find_duplicate_edges) ou as brutas (diagnose_handoff_candidates/
# diagnose_overlap_candidates). Depois de confirmares o dia/especie certos
# para Griffon-Vulture/Cinereous-Vulture/Steppe-Eagle, o padrao e':
#
cand <- diagnose_handoff_candidates(track_dt_day, "Steppe-Eagle")[1:5]  # os 5 pares mais proximos no tempo
plot_candidate_tracks(track_dt_day, unique(c(cand$track_id_a, cand$track_id_b)), edges_dt = cand)

#
# Exemplo ja' disponivel com os dados de hoje -- o maior grupo encontrado
# (Egyptian-Vulture, 7 tracks originais fundidos, ver reconciliation_summary_dt):
#
groups_ev  <- reconciliation_by_species[["Egyptian-Vulture"]]$groups
edges_ev   <- reconciliation_by_species[["Egyptian-Vulture"]]$edges
merged_ids <- groups_ev[, .N, by = synth_track_id][N == max(N), synth_track_id]
ids_ev     <- groups_ev[synth_track_id == merged_ids, track_id]
plot_candidate_tracks(track_dt_day, ids_ev, edges_dt = edges_ev[track_id_a %in% ids_ev & track_id_b %in% ids_ev])

#
# Plot do resultado FINAL (fragmentos originais em cinzento + trajetoria
# sintetica reconciliada por cima, ver R/track_harmonization.R secção 8) --
# para o maior grupo Steppe-Eagle ja validado nesta conversa:
#
groups_se    <- reconciliation_by_species[["Steppe-Eagle"]]$groups
duplicate_se <- reconciliation_by_species[["Steppe-Eagle"]]$duplicate_edges
synth_se     <- stitch_synthetic_tracks(track_dt_day, "Steppe-Eagle", groups_se, duplicate_se)
big_group_se <- groups_se[, .N, by = synth_track_id][N == max(N), synth_track_id]
plot_synthetic_track(track_dt_day, synth_se, big_group_se)

source("R/track_harmonization.R")   # picks up the fix

groups_se    <- reconciliation_by_species[["Steppe-Eagle"]]$groups
duplicate_se <- reconciliation_by_species[["Steppe-Eagle"]]$duplicate_edges
synth_se     <- stitch_synthetic_tracks(track_dt_day, "Steppe-Eagle", groups_se, duplicate_se)
plot_synthetic_track(track_dt_day, synth_se, "SYN_Steppe-Eagle_004")

##
## Exemplos -- inspecionar/reconstruir UM grupo especifico, antes de confiar
## na fusao automatica:
##
##   sp1        <- reconciliation_summary_dt$spec[1]
##   groups1    <- reconciliation_by_species[[sp1]]$groups
##   edges1     <- reconciliation_by_species[[sp1]]$edges
##   dup_edges1 <- reconciliation_by_species[[sp1]]$duplicate_edges
##
##   merged_ids <- groups1[, .N, by = synth_track_id][N > 1, synth_track_id]
##   inspect_reconciliation_group(track_dt_day, groups1, edges1, merged_ids[1])
##
## synth_dt1 <- stitch_synthetic_tracks(track_dt_day, sp1, groups1, dup_edges1)
## synth_dt1[synth_track_id == merged_ids[1]]  # pontos ja reconciliados, prontos a plottar
##
