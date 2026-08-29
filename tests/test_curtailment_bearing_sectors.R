##
## Teste com dados simulados para R/curtailment_bearing_sectors.R
##
## Turbina sintetica "TBS1" fixa em UTM (1000, 1000) (CRS arbitrario, so'
## interessa a geometria plana). Setor Norte tem 5 tracks a distancias
## diferentes (100/200/300/400/500m) para testar os quantis/skew_ratio de
## summarise_bearing_sectors() com uma distribuicao real; E/S/W tem 1 track
## cada (100m), para testar tambem o caso-limite n=1 (quantis colapsam no
## unico valor, skew_ratio fica NaN). Mais 3 casos de exclusao: track curto
## (< min_points), track ausente de track_dt, e turbina ausente do
## shapefile wtg.
##
## Correr: source("tests/test_curtailment_bearing_sectors.R")
##
## Depende de: data.table, sf
##

source("R/curtailment_bearing_sectors.R")

wtg_test <- sf::st_as_sf(
  data.frame(InternalNa = "TBS1", x = 1000, y = 1000),
  coords = c("x", "y"), crs = 32641
)

t0 <- as.POSIXct("2026-05-01 08:00:00", tz = "UTC")

## 5 pontos por track (>= min_points = 5), speed_ms identico em todos
## (8,9,10,11,50 -- 50 e outlier, excluido pelo percentil 95) -- isola a
## verificacao do bearing/setor/distancia/quantis: avg_speed esperado = 9.5
## em TODOS os tracks abaixo (mesma logica de test_curtailment_safe_distance.R)

make_track <- function(track_id, first_x, first_y) {
  data.table::data.table(
    track_id = track_id, timestamp = t0 + 0:4,
    utm_x = c(first_x, first_x, first_x + 1, first_x, first_x - 1),
    utm_y = c(first_y, first_y + 1, first_y + 2, first_y + 3, first_y + 4),
    speed_ms = c(8, 9, 10, 11, 50)
  )
}

## Setor Norte -- 5 tracks a 100/200/300/400/500m, todos exatamente a Norte
## (dx=0) -- distribuicao conhecida para testar os quantis de trigger_dist_m:
## p10=140, p25=200, mediana=300, p75=400, p90=460, max=500, skew_ratio=1
## (quantile() tipo 7, calculo a mao)
track_n1 <- make_track("TB_N1", 1000, 1100) # 100m
track_n2 <- make_track("TB_N2", 1000, 1200) # 200m
track_n3 <- make_track("TB_N3", 1000, 1300) # 300m
track_n4 <- make_track("TB_N4", 1000, 1400) # 400m
track_n5 <- make_track("TB_N5", 1000, 1500) # 500m

## E/S/W -- 1 track cada, exatamente a 100m -- testa o bearing/setor destes
## 3 pontos cardinais e o caso-limite n=1 (quantis == valor unico, skew NaN)
track_e <- make_track("TB_E", 1100, 1000) # bearing 90, setor "E"
track_s <- make_track("TB_S", 1000, 900)  # bearing 180, setor "S"
track_w <- make_track("TB_W", 900, 1000)  # bearing 270, setor "W"

## TB_SHORT -- so' 3 pontos (< min_points = 5) -- deve ficar de fora --------
track_short <- data.table::data.table(
  track_id = "TB_SHORT", timestamp = t0 + 0:2,
  utm_x = c(1050, 1050, 1050), utm_y = c(1050, 1050, 1050),
  speed_ms = c(5, 6, 7)
)

track_dt_test <- data.table::rbindlist(list(
  track_n1, track_n2, track_n3, track_n4, track_n5,
  track_e, track_s, track_w, track_short
))

qualifying_track_ids <- c("TB_N1", "TB_N2", "TB_N3", "TB_N4", "TB_N5", "TB_E", "TB_S", "TB_W")

curtl_dt_test <- data.table::rbindlist(list(
  data.table::data.table(
    turbine = "TBS1", track_id = qualifying_track_ids, species = "Golden-Eagle_test",
    start = t0 + 10
  ),
  data.table::data.table(turbine = "TBS1", track_id = "TB_SHORT", species = "Golden-Eagle_test", start = t0 + 10),
  data.table::data.table(turbine = "TBS1", track_id = "TB_ABSENT", species = "Golden-Eagle_test", start = t0 + 10),
  data.table::data.table(turbine = "T_MISSING", track_id = "TB_N1", species = "Golden-Eagle_test", start = t0 + 10)
))
## TB_SHORT -- excluido por ter < min_points registos em track_dt_test
## TB_ABSENT -- excluido por nao existir de todo em track_dt_test
## T_MISSING -- excluido por nao existir no shapefile wtg_test (nao duplica o TB_N1 valido)


## 1. bearing_to_sector() -- casos de fronteira (calculados a mao) ----------

cat("\n===== bearing_to_sector() -- casos de fronteira =====\n")
boundary_cases <- data.table::data.table(
  bearing_deg = c(0, 44, 22.5, 350, 337.5, 90, 180, 270),
  expected    = c("N", "NE", "NE", "N", "N", "E", "S", "W")
)
boundary_cases[, got := bearing_to_sector(bearing_deg)]
boundary_cases[, ok := got == expected]
print(boundary_cases)
cat(sprintf("Resultado: %d/%d casos corretos.\n", sum(boundary_cases$ok), nrow(boundary_cases)))


## 2. compute_curtailment_bearing() -- setor/distancia/velocidade por evento -

cat("\n===== compute_curtailment_bearing(curtl_dt_test, track_dt_test, wtg_test) =====\n")
bearing_dt_test <- compute_curtailment_bearing(curtl_dt_test, track_dt_test, wtg_test, min_points = 5)
print(bearing_dt_test)

expected_events <- data.table::data.table(
  track_id           = c("TB_N1", "TB_N2", "TB_N3", "TB_N4", "TB_N5", "TB_E", "TB_S", "TB_W"),
  expected_sector    = c("N", "N", "N", "N", "N", "E", "S", "W"),
  expected_dist_m    = c(100, 200, 300, 400, 500, 100, 100, 100),
  expected_avg_speed = 9.5
)
check2 <- merge(bearing_dt_test, expected_events, by = "track_id")
check2[, `:=`(
  sector_ok = as.character(sector) == expected_sector,
  dist_ok   = abs(trigger_dist_m - expected_dist_m) < 0.5,
  speed_ok  = abs(avg_speed_ms - expected_avg_speed) < 0.05
)]
print(check2[, .(track_id, sector, expected_sector, sector_ok,
                 trigger_dist_m, expected_dist_m, dist_ok,
                 avg_speed_ms, expected_avg_speed, speed_ok)])

cat(sprintf(
  "Resultado: %d/%d eventos corretos (sector+distancia+velocidade).\n",
  sum(check2$sector_ok & check2$dist_ok & check2$speed_ok), nrow(check2)
))

cat(sprintf(
  "\nLinhas no resultado: %d (esperado: 8 -- TB_SHORT/TB_ABSENT/T_MISSING excluidos) -- %s\n",
  nrow(bearing_dt_test), if (nrow(bearing_dt_test) == 8L) "OK" else "FALHOU"
))


## 3. summarise_bearing_sectors() -- quantis por setor, nao so' a media ----

cat("\n===== summarise_bearing_sectors(bearing_dt_test) =====\n")
summary_bearing_test <- summarise_bearing_sectors(bearing_dt_test)
print(summary_bearing_test)

n_sector <- summary_bearing_test[sector == "N"]
cat("\n----- Setor N (5 eventos a 100/200/300/400/500m) -----\n")
cat(sprintf(
  "dist_p10=%.1f (esp. 140), dist_p25=%.1f (esp. 200), dist_median=%.1f (esp. 300), dist_p75=%.1f (esp. 400), dist_p90=%.1f (esp. 460), dist_max=%.1f (esp. 500), dist_skew_ratio=%.2f (esp. 1)\n",
  n_sector$dist_p10, n_sector$dist_p25, n_sector$dist_median, n_sector$dist_p75, n_sector$dist_p90, n_sector$dist_max, n_sector$dist_skew_ratio
))
cat(sprintf(
  "speed_median=%.1f (esp. 9.5), speed_skew_ratio=%s (esp. NaN -- sem variabilidade, todos os tracks com a mesma velocidade)\n",
  n_sector$speed_median, as.character(n_sector$speed_skew_ratio)
))

cat("\n----- Setores E/S/W (n=1 -- caso-limite) -----\n")
print(summary_bearing_test[sector %in% c("E", "S", "W"), .(sector, n, dist_p10, dist_median, dist_p90, dist_skew_ratio)])
cat("Esperado: n=1, dist_p10==dist_median==dist_p90==100, dist_skew_ratio=NaN (0/0, sem variabilidade).\n")

cat("\n----- Setores sem eventos (NE/SE/SW/NW) -----\n")
print(summary_bearing_test[sector %in% c("NE", "SE", "SW", "NW"), .(sector, n, dist_median)])
cat("Esperado: n=0, dist_median=NA nos 4 setores.\n")


## 4. plot_bearing_boxplot()/plot_bearing_hist() -- so' verificar que correm
## sem erro e devolvem um ggplot (nao NULL, ha dados) --------------------

cat("\n===== plot_bearing_boxplot()/plot_bearing_hist() -- smoke test =====\n")
p_box_dist  <- plot_bearing_boxplot(bearing_dt_test, metric = "trigger_dist_m")
p_box_speed <- plot_bearing_boxplot(bearing_dt_test, metric = "avg_speed_ms")
p_hist_dist  <- plot_bearing_hist(bearing_dt_test, metric = "trigger_dist_m")
p_hist_speed <- plot_bearing_hist(bearing_dt_test, metric = "avg_speed_ms")
cat(sprintf(
  "4 plots gerados sem erro, todos nao-NULL: %s\n",
  if (!is.null(p_box_dist) && !is.null(p_box_speed) && !is.null(p_hist_dist) && !is.null(p_hist_speed)) "OK" else "FALHOU"
))
