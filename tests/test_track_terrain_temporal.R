##
## Teste com dados simulados para R/track_terrain_temporal.R
##
## 2 turbinas sinteticas bem separadas -- "TWA1" (flat) em UTM (1000,1000) e
## "TWB1" (ridge) em UTM (5000,5000) -- para que a atribuicao de turbina
## mais proxima por track seja inequivoca. Tracks espalhados por 2 bins de 7
## dias (semana 0 e semana 1, a partir do 1o dia com dados) para testar o
## ancoramento do bin e o preenchimento de combinacoes semana x classe sem
## dados (0 explicito).
##
## Correr: source("tests/test_track_terrain_temporal.R")
##
## Depende de: data.table, sf
##

source("R/track_terrain_temporal.R")

wtg_test <- sf::st_as_sf(
  data.frame(InternalNa = c("TWA1", "TWB1"), x = c(1000, 5000), y = c(1000, 5000)),
  coords = c("x", "y"), crs = 32641
)

turbine_terrain_dt_test <- data.table::data.table(
  wtg_id = c("TWA1", "TWB1"),
  terrain_class = factor(c("flat", "ridge"), levels = c("flat", "complex", "ridge"))
)

base_date <- as.Date("2026-06-01") # sera' o min_date -> week_start da semana 0

## Tracks perto de TWA1 (flat): 3 na semana 0 (dias 0,1,2), 1 na semana 1 (dia 7)
make_track_at <- function(track_id, day_offset, x, y) {
  data.table::data.table(
    track_id = track_id,
    timestamp = as.POSIXct(base_date + day_offset, tz = "UTC") + 3600, # meio-dia-ish, irrelevante
    utm_x = x, utm_y = y
  )
}

tracks_flat <- data.table::rbindlist(list(
  make_track_at("TA_W0_1", 0, 1000, 1000),
  make_track_at("TA_W0_2", 1, 1005, 995),
  make_track_at("TA_W0_3", 2, 995, 1005),
  make_track_at("TA_W1_1", 7, 1000, 1000)
))

## Tracks perto de TWB1 (ridge): 2 na semana 0 (dias 0,3), 0 na semana 1
tracks_ridge <- data.table::rbindlist(list(
  make_track_at("TB_W0_1", 0, 5000, 5000),
  make_track_at("TB_W0_2", 3, 5005, 4995)
))

track_dt_test <- data.table::rbindlist(list(tracks_flat, tracks_ridge))


## 1. assign_track_terrain_class() -- turbina mais proxima + classe --------

cat("\n===== assign_track_terrain_class(track_dt_test, wtg_test, turbine_terrain_dt_test) =====\n")
track_terrain_test <- assign_track_terrain_class(track_dt_test, wtg_test, turbine_terrain_dt_test)
print(track_terrain_test)

expected_terrain <- data.table::data.table(
  track_id = c("TA_W0_1", "TA_W0_2", "TA_W0_3", "TA_W1_1", "TB_W0_1", "TB_W0_2"),
  expected_turbine = c("TWA1", "TWA1", "TWA1", "TWA1", "TWB1", "TWB1"),
  expected_class   = c("flat", "flat", "flat", "flat", "ridge", "ridge")
)
check1 <- merge(track_terrain_test, expected_terrain, by = "track_id")
check1[, ok := nearest_turbine == expected_turbine & as.character(terrain_class) == expected_class]
cat(sprintf("Resultado: %d/%d tracks com turbina/classe corretas.\n", sum(check1$ok), nrow(check1)))


## 2. summarise_tracks_by_week_terrain() -- bins de 7 dias, 0-fill ----------

cat("\n===== summarise_tracks_by_week_terrain(track_terrain_test, turbine_terrain_dt_test) =====\n")
weekly_test <- summarise_tracks_by_week_terrain(track_terrain_test, turbine_terrain_dt_test)
print(weekly_test)

week0 <- base_date
week1 <- base_date + 7

n_flat_w0  <- weekly_test[week_start == week0 & terrain_class == "flat", n_tracks]
n_flat_w1  <- weekly_test[week_start == week1 & terrain_class == "flat", n_tracks]
n_ridge_w0 <- weekly_test[week_start == week0 & terrain_class == "ridge", n_tracks]
n_ridge_w1 <- weekly_test[week_start == week1 & terrain_class == "ridge", n_tracks]
n_complex_w0 <- weekly_test[week_start == week0 & terrain_class == "complex", n_tracks]

cat(sprintf(
  "flat: semana0=%d (esp. 3), semana1=%d (esp. 1); ridge: semana0=%d (esp. 2), semana1=%d (esp. 0, 0-fill); complex: semana0=%d (esp. 0, 0-fill, sem turbinas 'complex')\n",
  n_flat_w0, n_flat_w1, n_ridge_w0, n_ridge_w1, n_complex_w0
))

n_tpt_flat_w0  <- weekly_test[week_start == week0 & terrain_class == "flat", n_tracks_per_turbine]
n_tpt_ridge_w0 <- weekly_test[week_start == week0 & terrain_class == "ridge", n_tracks_per_turbine]
cat(sprintf(
  "n_tracks_per_turbine (1 turbina em cada classe aqui, por isso == n_tracks): flat semana0=%.2f (esp. 3), ridge semana0=%.2f (esp. 2)\n",
  n_tpt_flat_w0, n_tpt_ridge_w0
))

cat(sprintf(
  "Linhas totais: %d (esperado: 2 semanas x 3 classes = 6)\n", nrow(weekly_test)
))


## 3. plot_tracks_by_week_terrain() -- smoke test ---------------------------

cat("\n===== plot_tracks_by_week_terrain() -- smoke test =====\n")
p_default <- plot_tracks_by_week_terrain(weekly_test)
p_per_turbine <- plot_tracks_by_week_terrain(weekly_test, metric = "n_tracks_per_turbine")
p_facet <- plot_tracks_by_week_terrain(weekly_test, facet = TRUE)

## chamado sobre a tabela ERRADA (track_terrain_test, per-track, sem
## 'n_tracks_per_turbine' nem sequer 'week_start') -- deve devolver NULL com
## mensagem, nao um erro, gracas ao guard de coluna ausente
p_missing_metric <- plot_tracks_by_week_terrain(track_terrain_test, metric = "n_tracks_per_turbine")

cat(sprintf(
  "3 plots gerados sem erro (nao-NULL): %s -- caso de coluna ausente devolveu NULL: %s\n",
  if (!is.null(p_default) && !is.null(p_per_turbine) && !is.null(p_facet)) "OK" else "FALHOU",
  is.null(p_missing_metric)
))
