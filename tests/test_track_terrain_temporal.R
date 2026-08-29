##
## Teste com dados simulados para R/track_terrain_temporal.R
##
## 3 turbinas sinteticas bem separadas -- "TWA1"/"TWA2" (flat) e "TWB1"
## (ridge) -- para testar a agregacao ENTRE TURBINAS da mesma classe
## (media/mediana/desvio-padrao), nao so' 1 razao soma/contagem. flat tem 2
## turbinas com atividades DIFERENTES (para o desvio-padrao ser > 0 e
## calculavel a mao); ridge tem so' 1 turbina (para testar o caso-limite
## sd = NA, n < 2). Tracks espalhados por 2 bins de 7 dias.
##
## Correr: source("tests/test_track_terrain_temporal.R")
##
## Depende de: data.table, sf
##

source("R/track_terrain_temporal.R")

wtg_test <- sf::st_as_sf(
  data.frame(InternalNa = c("TWA1", "TWA2", "TWB1"), x = c(1000, 2000, 9000), y = c(1000, 1000, 9000)),
  coords = c("x", "y"), crs = 32641
)

turbine_terrain_dt_test <- data.table::data.table(
  wtg_id = c("TWA1", "TWA2", "TWB1"),
  terrain_class = factor(c("flat", "flat", "ridge"), levels = c("flat", "complex", "ridge"))
)

base_date <- as.Date("2026-06-01") # sera' o min_date -> week_start da semana 0

make_track_at <- function(track_id, day_offset, x, y) {
  data.table::data.table(
    track_id = track_id,
    timestamp = as.POSIXct(base_date + day_offset, tz = "UTC") + 3600,
    utm_x = x, utm_y = y
  )
}

## TWA1 (flat): 3 tracks na semana 0 (dias 0,1,2), 1 na semana 1 (dia 7)
## TWA2 (flat): 1 track na semana 0 (dia 0), 0 na semana 1
##   -> flat semana0: turbinas [3,1] -> mean=2, median=2, sd=1.41
##   -> flat semana1: turbinas [1,0] -> mean=0.5, median=0.5, sd=0.71
tracks_a1 <- data.table::rbindlist(list(
  make_track_at("TA1_W0_1", 0, 1000, 1000),
  make_track_at("TA1_W0_2", 1, 1005, 995),
  make_track_at("TA1_W0_3", 2, 995, 1005),
  make_track_at("TA1_W1_1", 7, 1000, 1000)
))
tracks_a2 <- data.table::rbindlist(list(
  make_track_at("TA2_W0_1", 0, 2000, 1000)
))

## TWB1 (ridge, unica turbina da classe): 2 tracks na semana 0, 0 na semana 1
##   -> ridge semana0: turbinas [2] -> mean=2, median=2, sd=NA (n=1)
##   -> ridge semana1: turbinas [0] -> mean=0, median=0, sd=NA (n=1)
tracks_b1 <- data.table::rbindlist(list(
  make_track_at("TB1_W0_1", 0, 9000, 9000),
  make_track_at("TB1_W0_2", 3, 9005, 8995)
))

track_dt_test <- data.table::rbindlist(list(tracks_a1, tracks_a2, tracks_b1))


## 1. assign_track_terrain_class() -- turbina mais proxima + classe --------

cat("\n===== assign_track_terrain_class(track_dt_test, wtg_test, turbine_terrain_dt_test) =====\n")
track_terrain_test <- assign_track_terrain_class(track_dt_test, wtg_test, turbine_terrain_dt_test)
print(track_terrain_test)

expected_terrain <- data.table::data.table(
  track_id = c("TA1_W0_1", "TA1_W0_2", "TA1_W0_3", "TA1_W1_1", "TA2_W0_1", "TB1_W0_1", "TB1_W0_2"),
  expected_turbine = c("TWA1", "TWA1", "TWA1", "TWA1", "TWA2", "TWB1", "TWB1"),
  expected_class   = c("flat", "flat", "flat", "flat", "flat", "ridge", "ridge")
)
check1 <- merge(track_terrain_test, expected_terrain, by = "track_id")
check1[, ok := nearest_turbine == expected_turbine & as.character(terrain_class) == expected_class]
cat(sprintf("Resultado: %d/%d tracks com turbina/classe corretas.\n", sum(check1$ok), nrow(check1)))


## 2. summarise_tracks_by_week_terrain() -- media/mediana/sd ENTRE TURBINAS -

cat("\n===== summarise_tracks_by_week_terrain(track_terrain_test, turbine_terrain_dt_test) =====\n")
weekly_test <- summarise_tracks_by_week_terrain(track_terrain_test, turbine_terrain_dt_test)
print(weekly_test)

week0 <- base_date
week1 <- base_date + 7

flat_w0  <- weekly_test[week_start == week0 & terrain_class == "flat"]
flat_w1  <- weekly_test[week_start == week1 & terrain_class == "flat"]
ridge_w0 <- weekly_test[week_start == week0 & terrain_class == "ridge"]
ridge_w1 <- weekly_test[week_start == week1 & terrain_class == "ridge"]

cat(sprintf(
  "flat semana0: n_turbines=%d (esp. 2), n_tracks=%d (esp. 4), mean=%.2f (esp. 2.00), median=%.2f (esp. 2.00), sd=%.2f (esp. 1.41)\n",
  flat_w0$n_turbines, flat_w0$n_tracks, flat_w0$mean_tracks_per_turbine, flat_w0$median_tracks_per_turbine, flat_w0$sd_tracks_per_turbine
))
cat(sprintf(
  "flat semana1: n_turbines=%d (esp. 2), n_tracks=%d (esp. 1), mean=%.2f (esp. 0.50), median=%.2f (esp. 0.50), sd=%.2f (esp. 0.71)\n",
  flat_w1$n_turbines, flat_w1$n_tracks, flat_w1$mean_tracks_per_turbine, flat_w1$median_tracks_per_turbine, flat_w1$sd_tracks_per_turbine
))
cat(sprintf(
  "ridge semana0: n_turbines=%d (esp. 1), n_tracks=%d (esp. 2), mean=%.2f (esp. 2.00), sd=%s (esp. NA, so' 1 turbina)\n",
  ridge_w0$n_turbines, ridge_w0$n_tracks, ridge_w0$mean_tracks_per_turbine, as.character(ridge_w0$sd_tracks_per_turbine)
))
cat(sprintf(
  "ridge semana1: n_turbines=%d (esp. 1), n_tracks=%d (esp. 0, 0-fill), mean=%.2f (esp. 0.00), sd=%s (esp. NA)\n",
  ridge_w1$n_turbines, ridge_w1$n_tracks, ridge_w1$mean_tracks_per_turbine, as.character(ridge_w1$sd_tracks_per_turbine)
))

cat(sprintf(
  "pct_turbines_active: flat semana0=%.1f (esp. 100, 2/2 ativas), flat semana1=%.1f (esp. 50, 1/2), ridge semana0=%.1f (esp. 100, 1/1), ridge semana1=%.1f (esp. 0, 0/1)\n",
  flat_w0$pct_turbines_active, flat_w1$pct_turbines_active, ridge_w0$pct_turbines_active, ridge_w1$pct_turbines_active
))

cat(sprintf(
  "\nClasses presentes no resultado: %s (esperado: so' 'flat'/'ridge' -- 'complex' nao aparece, 0 turbinas dessa classe em turbine_terrain_dt_test)\n",
  paste(sort(unique(as.character(weekly_test$terrain_class))), collapse = ", ")
))
cat(sprintf("Linhas totais: %d (esperado: 2 semanas x 2 classes com turbinas = 4)\n", nrow(weekly_test)))


## 3. plot_tracks_by_week_terrain() -- smoke test ---------------------------

cat("\n===== plot_tracks_by_week_terrain() -- smoke test =====\n")
p_mean   <- plot_tracks_by_week_terrain(weekly_test) # omissao: mean_tracks_per_turbine + banda SD
p_total  <- plot_tracks_by_week_terrain(weekly_test, metric = "n_tracks")
p_median <- plot_tracks_by_week_terrain(weekly_test, metric = "median_tracks_per_turbine")
p_active <- plot_tracks_by_week_terrain(weekly_test, metric = "pct_turbines_active")
p_facet  <- plot_tracks_by_week_terrain(weekly_test, facet = TRUE)

## chamado sobre a tabela ERRADA (track_terrain_test, per-track, sem
## nenhuma das colunas agregadas) -- deve devolver NULL com mensagem
p_wrong_table <- plot_tracks_by_week_terrain(track_terrain_test)

cat(sprintf(
  "5 plots gerados sem erro (nao-NULL): %s -- tabela errada devolveu NULL: %s\n",
  if (!is.null(p_mean) && !is.null(p_total) && !is.null(p_median) && !is.null(p_active) && !is.null(p_facet)) "OK" else "FALHOU",
  is.null(p_wrong_table)
))
