##
## Teste com dados simulados para R/track_species_clusters.R
##
## Mesmo layout de turbinas do teste de R/turbine_spatial_clusters.R (linha
## reta, y=0): T1=0, T2=300, T3=600, T4=5000, T5=5300, T6=20000.
##
## 4 tracks, so' 3 sao Kestrel (species_sel por omissao) -- o 4º (KT3, Steppe-
## Eagle) serve para confirmar que fica EXCLUIDO por especie:
##   KT1 (Kestrel) -- pontos (5,0) e (290,0). Distancia ao turbina mais
##     proximo em cada ponto: 5 (a T1) e 10 (a T2) -- minimo global = 5 (T1),
##     por isso o track fica atribuido a T1, closest_dist_m=5.
##   KT2 (Kestrel) -- pontos (4980,0) e (5325,0). Distancias: 20 (a T4) e 25
##     (a T5) -- minimo = 20 (T4), track atribuido a T4, closest_dist_m=20.
##   KT3 (Steppe-Eagle) -- ponto (20015,0), perto de T6 -- EXCLUIDO por
##     especie quando species_sel="Kestrel" (omissao).
##   KT4 (Kestrel) -- ponto unico (615,0), distancia 15 a T3 -- track
##     atribuido a T3, closest_dist_m=15.
##
## Semanas: KT1 e KT2 comecam no mesmo dia (2026-04-01); KT4 comeca 19 dias
## depois (2026-04-20) -- claramente noutra semana, sem ambiguidade de
## week_start.
##
## Nao usa testthat -- dados sinteticos, resultado calculado a mao.
##
## Correr: source("tests/test_track_species_clusters.R")
##
## Depende de: data.table, sf, RANN
##

source("R/turbine_spatial_clusters.R")
source("R/track_species_clusters.R")

wtg_test <- sf::st_as_sf(
  data.frame(
    InternalNa = c("T1", "T2", "T3", "T4", "T5", "T6"),
    x = c(0, 300, 600, 5000, 5300, 20000),
    y = c(0, 0, 0, 0, 0, 0)
  ),
  coords = c("x", "y"), crs = 32641
)

t_kt1 <- as.POSIXct("2026-04-01 08:00:00", tz = "UTC")
t_kt2 <- as.POSIXct("2026-04-01 09:00:00", tz = "UTC")
t_kt3 <- as.POSIXct("2026-04-01 10:00:00", tz = "UTC")
t_kt4 <- as.POSIXct("2026-04-20 08:00:00", tz = "UTC")

track_dt_test <- data.table(
  track_id = c(rep("KT1", 2), rep("KT2", 2), "KT3", "KT4"),
  spec     = c(rep("Kestrel", 2), rep("Kestrel", 2), "Steppe-Eagle", "Kestrel"),
  timestamp = c(
    t_kt1, t_kt1 + 60,
    t_kt2, t_kt2 + 60,
    t_kt3,
    t_kt4
  ),
  utm_x = c(5, 290, 4980, 5325, 20015, 615),
  utm_y = c(0, 0, 0, 0, 0, 0)
)

tracks_assigned_dt <- assign_tracks_to_nearest_turbine(track_dt_test, wtg_test, species_sel = "Kestrel")

cat("\n===== assign_tracks_to_nearest_turbine() =====\n")
print(tracks_assigned_dt[order(track_id)])
cat(paste(
  "\nEsperado: 3 linhas (KT3 excluido, e' Steppe-Eagle). KT1 -> turbine=T1,",
  "closest_dist_m=5. KT2 -> turbine=T4, closest_dist_m=20. KT4 -> turbine=T3,",
  "closest_dist_m=15.\n"
))

## ---- summarise_track_occurrence_weekly() ----

weekly_dt <- summarise_track_occurrence_weekly(tracks_assigned_dt)
cat("\n===== summarise_track_occurrence_weekly() =====\n")
print(weekly_dt)
cat(paste(
  "\nEsperado: 2 linhas -- a semana de 2026-04-01 com n_tracks=2 (KT1+KT2),",
  "e a semana de 2026-04-20 com n_tracks=1 (KT4). O valor exato da data",
  "(inicio da semana) depende da convencao week_start do lubridate -- nao",
  "criticar isso, so' confirmar 2 linhas com n_tracks=2 e 1.\n"
))

## ---- summarise_track_occurrence_by_turbine() ----

by_turbine_dt <- summarise_track_occurrence_by_turbine(tracks_assigned_dt)
cat("\n===== summarise_track_occurrence_by_turbine() =====\n")
print(by_turbine_dt)
cat(paste(
  "\nEsperado: 3 linhas (T1, T3, T4), cada uma com n_tracks=1,",
  "pct_of_total=33.3%.\n"
))

## ---- summarise_track_occurrence_by_cluster() ----

manual_test <- list(Grupo_X = c("T1", "T2", "T3"), Grupo_Y = c("T4", "T5"), Grupo_Z = c("T6"))
cluster_dt_test <- manual_turbine_clusters_dt(manual_test)

by_cluster_out <- summarise_track_occurrence_by_cluster(tracks_assigned_dt, cluster_dt_test)

cat("\n===== summarise_track_occurrence_by_cluster()$by_cluster =====\n")
print(by_cluster_out$by_cluster)
cat(paste(
  "\nEsperado: Grupo_X n_tracks=2 (KT1 em T1 + KT4 em T3), 66.7% do total;",
  "Grupo_Y n_tracks=1 (KT2 em T4), 33.3%. Grupo_Z nao aparece (T6 sem",
  "tracks atribuidos).\n"
))

cat("\n===== summarise_track_occurrence_by_cluster()$weekly =====\n")
print(by_cluster_out$weekly)
cat(paste(
  "\nEsperado: 3 linhas -- Grupo_X/semana-04-01 n=1 (KT1), Grupo_X/semana-04-20",
  "n=1 (KT4), Grupo_Y/semana-04-01 n=1 (KT2).\n"
))
