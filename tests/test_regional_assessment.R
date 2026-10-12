##
## Teste com dados simulados para R/regional_assessment.R
##
## Cobre tudo o que nao precisa de ficheiros brutos: dias monitorizados,
## bins de 2 min -> maximo diario/semanal (NA onde nao ha' monitorizacao),
## timing da passagem, correlacao cruzada, pulsos e plots. Nao cobre
## regional_load_farm() (precisa dos ficheiros brutos + fst/janitor).
##
## Correr: source("tests/test_regional_assessment.R")
##
## Depende de: data.table, lubridate, ggplot2, scales
##

library(data.table)
library(ggplot2)
source("R/track_min_individuals.R")
source("R/regional_assessment.R")

check <- function(label, ok) cat(sprintf("%s -- %s\n", label, if (isTRUE(ok)) "OK" else "FALHOU"))
tz_test <- "Asia/Samarkand"


##
## 1. Bins -> maximo diario/semanal, num parque simulado de 10 dias
##
## dia 1-2 (seg-ter): 2 tracks de EV a 100 m no MESMO bin -> 1 individuo
## dia 2: 2 tracks de EV a 1000 m -> 2 individuos (maximo do dia 2)
## dia 3: so' tracks de outra especie (dia monitorizado, EV ausente -> 0)
## dia 4: sem nenhum track -> NAO monitorizado (NA)
## dias 5-7 (qui-sab): 1 track por dia de outra especie (monitorizados, EV 0)
## dia 10 (ter): 1 track EV
##
base_day <- as.POSIXct("2026-03-02 10:00:00", tz = tz_test) # segunda-feira
mk <- function(day_offset, minute, id, x, spec) {
  data.table(track_id = id, timestamp = base_day + day_offset * 86400 + minute * 60,
             utm_x = x, utm_y = 0, spec = spec)
}
track_dt_test <- rbindlist(list(
  mk(0, 0, "a1", 0, "Egyptian-Vulture"), mk(0, 0, "a2", 100, "Egyptian-Vulture"),
  mk(1, 0, "b1", 0, "Egyptian-Vulture"), mk(1, 0, "b2", 1000, "Egyptian-Vulture"),
  mk(2, 0, "c1", 0, "Steppe-Eagle"), mk(2, 1, "c2", 0, "Steppe-Eagle"),
  mk(4, 0, "d1", 0, "Steppe-Eagle"), mk(4, 1, "d2", 0, "Steppe-Eagle"),
  mk(5, 0, "e1", 0, "Steppe-Eagle"), mk(5, 1, "e2", 0, "Steppe-Eagle"),
  mk(6, 0, "f1", 0, "Steppe-Eagle"), mk(6, 1, "f2", 0, "Steppe-Eagle"),
  mk(9, 0, "g1", 0, "Egyptian-Vulture"), mk(9, 1, "g2", 0, "Steppe-Eagle")
))

mon_test <- regional_monitored_days(track_dt_test, tz_test, min_tracks_per_day = 2L)
check("dias monitorizados: 7 (sem tracks nos dias 3, 7 e 8)", length(mon_test$monitored) == 7L)
check("intervalo de dados: 1o = 2026-03-02", mon_test$first_day == as.Date("2026-03-02"))
check("intervalo de dados: ultimo = 2026-03-11", mon_test$last_day == as.Date("2026-03-11"))

bins_test <- regional_min_individuals_bins(track_dt_test, "Egyptian-Vulture", bin_min = 2, merge_dist_m = 200)
daily_test <- regional_daily_max(bins_test, "Egyptian-Vulture", mon_test, tz_test)
dv <- function(d) daily_test[day == as.Date(d), max_individuals]
check("dia 1 (2 tracks a 100 m -> 1 individuo)", dv("2026-03-02") == 1L)
check("dia 2 (2 tracks a 1000 m -> 2 individuos)", dv("2026-03-03") == 2L)
check("dia 3 (monitorizado, EV ausente -> 0)", dv("2026-03-04") == 0L)
check("dia 4 (sem tracks -> NA, nao 0)", is.na(dv("2026-03-05")))
check("dia 10 (1 track EV -> 1)", dv("2026-03-11") == 1L)

weekly_test <- regional_weekly_max(daily_test, min_monitored_days = 4L)
check("semana 1 (seg 2026-03-02): maximo 2 com >=4 dias monitorizados",
      weekly_test[week_start == as.Date("2026-03-02"), max_individuals] == 2L)
check("semana 2 (seg 2026-03-09): poucos dias monitorizados -> NA",
      is.na(weekly_test[week_start == as.Date("2026-03-09"), max_individuals]))


##
## 2. Antecipacao: outono ZRF -> BSH (+5 d) e DGY (+7 d); primavera BSH (+0) -> ZRF (+6 d)
##
make_series <- function(farm, starts, from, to, width = 3L, height = 3L, spec = "Egyptian-Vulture") {
  d <- data.table(farm = farm, spec = spec, day = seq(as.Date(from), as.Date(to), by = "day"))
  d[, `:=`(monitored = TRUE, max_individuals = 0L)]
  for (s in as.Date(starts)) d[day >= s & day < s + width, max_individuals := as.integer(height)]
  d[]
}
autumn_zrf <- as.Date("2025-09-01") + c(0, 20, 45, 70)
spring_bsh <- as.Date("2026-03-01") + c(0, 15, 35, 55)
rng <- c("2025-08-01", "2026-06-15")
daily_pair_test <- rbindlist(list(
  make_series("ZRF", c(autumn_zrf, spring_bsh + 6), rng[1], rng[2]),
  make_series("BSH", c(autumn_zrf + 5, spring_bsh), rng[1], rng[2]),
  make_series("DGY", c(autumn_zrf + 7, spring_bsh), rng[1], rng[2])
))

seasons_test <- list(autumn = c("08-15", "12-15"), spring = c("02-15", "05-31"))
season_dates_test <- rbind(
  regional_season_dates(seasons_test["autumn"], 2025L),
  regional_season_dates(seasons_test["spring"], 2026L)
)
pairs_test <- data.table(
  season   = c("autumn", "autumn", "spring", "spring"),
  leader   = c("ZRF", "ZRF", "BSH", "DGY"),
  follower = c("BSH", "DGY", "ZRF", "ZRF")
)

ccf_test <- regional_ccf(daily_pair_test, pairs_test, season_dates_test,
                         max_lag_days = 15L, smooth_days = 1L, min_overlap_days = 20L)
ccf_sum_test <- regional_ccf_summary(ccf_test)
best <- function(s, l, f) ccf_sum_test[season == s & leader == l & follower == f, best_lag_days]
check("ccf outono ZRF -> BSH: lag = 5", best("autumn", "ZRF", "BSH") == 5L)
check("ccf outono ZRF -> DGY: lag = 7", best("autumn", "ZRF", "DGY") == 7L)
check("ccf primavera BSH -> ZRF: lag = 6", best("spring", "BSH", "ZRF") == 6L)
check("ccf primavera DGY -> ZRF: lag = 6", best("spring", "DGY", "ZRF") == 6L)

timing_test <- regional_passage_timing(daily_pair_test, season_dates_test, min_total = 3)
check("timing: 3 parques x 2 epocas, todos validos", nrow(timing_test) == 6L && all(timing_test$valid))
lead_test <- regional_lead_table(timing_test, pairs_test)
lead <- function(s, l, f) lead_test[season == s & leader == l & follower == f, lead_days_median]
check("timing mediana outono ZRF -> BSH: positivo (ZRF antecipa)", lead("autumn", "ZRF", "BSH") > 0L)
check("timing mediana primavera BSH -> ZRF: positivo (BSH antecipa)", lead("spring", "BSH", "ZRF") > 0L)

pulses_test <- regional_pulses(daily_pair_test, min_individuals = 1L, merge_gap_days = 3L)
check("pulsos: 8 por parque ZRF (4 outono + 4 primavera)", pulses_test[farm == "ZRF", .N] == 8L)
match_test <- regional_match_pulses(pulses_test, pairs_test, season_dates_test, window_days = 10L)
pulse_sum_test <- regional_pulse_summary(match_test)
ps <- function(s, l, f) pulse_sum_test[season == s & leader == l & follower == f]
check("pulsos outono ZRF -> BSH: 4/4 seguidos, lag mediano 5",
      ps("autumn", "ZRF", "BSH")$pct_followed == 100 && ps("autumn", "ZRF", "BSH")$median_lag_days == 5)
check("pulsos outono ZRF -> BSH: nenhum pulso do seguidor ANTES",
      ps("autumn", "ZRF", "BSH")$n_follower_pulse_before == 0L)
check("pulsos primavera BSH -> ZRF: lag mediano 6", ps("spring", "BSH", "ZRF")$median_lag_days == 6)


##
## 3. Epoca sem cobertura -> NA (nao inventa timing)
##
daily_gap_test <- copy(daily_pair_test)
daily_gap_test[farm == "DGY" & day >= as.Date("2025-09-01") & day <= as.Date("2025-12-01"),
               `:=`(monitored = FALSE, max_individuals = NA_integer_)]
timing_gap_test <- regional_passage_timing(daily_gap_test, season_dates_test, min_season_coverage = 0.7)
check("DGY outono com cobertura < 70% -> valid = FALSE e onset NA",
      !timing_gap_test[farm == "DGY" & season == "autumn", valid] &&
        is.na(timing_gap_test[farm == "DGY" & season == "autumn", onset]))


##
## 4. Plots devolvem objetos ggplot (e NULL quando nao ha' dados)
##
weekly_all_test <- rbindlist(lapply(c("ZRF", "BSH", "DGY"), function(f) {
  w <- regional_weekly_max(daily_pair_test[farm == f], min_monitored_days = 4L); w[, farm := f]; w
}))
sp <- "Egyptian-Vulture"
is_gg <- function(p) inherits(p, "ggplot")
check("plot semanal cronologico", is_gg(plot_regional_weekly_timeline(weekly_all_test, sp)))
check("plot sobreposicao anual", is_gg(plot_regional_annual_overlay(weekly_all_test, sp)))
check("plot sobreposicao anual (relativo)", is_gg(plot_regional_annual_overlay(weekly_all_test, sp, normalise = TRUE)))
check("plot series sazonais", is_gg(plot_regional_season_series(daily_pair_test, season_dates_test, sp)))
check("plot acumulado", is_gg(plot_regional_cumulative(daily_pair_test, season_dates_test, sp)))
check("plot ccf", is_gg(plot_regional_ccf(ccf_test, sp)))
check("plot lead days", is_gg(plot_regional_lead_days(lead_test, sp)))
check("plot sem dados -> NULL", is.null(plot_regional_weekly_timeline(weekly_all_test, "Golden-Eagle")))
invisible(lapply(list(
  plot_regional_weekly_timeline(weekly_all_test, sp), plot_regional_annual_overlay(weekly_all_test, sp),
  plot_regional_season_series(daily_pair_test, season_dates_test, sp),
  plot_regional_cumulative(daily_pair_test, season_dates_test, sp),
  plot_regional_ccf(ccf_test, sp), plot_regional_lead_days(lead_test, sp)
), function(p) ggplot2::ggplot_build(p)))
cat("ggplot_build() de todos os plots sem erro -- OK\n")


##
## 5. userSettings_regional_assessment.R + padroes de ficheiros + janela ini/end
##
settings_env_test <- new.env(parent = globalenv())
source("inputs/userSettings_regional_assessment.R", local = settings_env_test)
farms_test <- settings_env_test$regional_farms

check("settings: 3 parques na ordem ZRF, BSH, DGY", identical(farms_test$farm, c("ZRF", "BSH", "DGY")))
check("settings: padroes = prefixo + '.*' + parque",
      identical(farms_test$trackreport_pattern,
                c("TrackReport_Default.*ZRF", "TrackReport_Default.*BSH", "TrackReport_Default.*DGY")))
check("settings: data-raw em todos os parques; pasta de rede UNC so' em BSH e DGY (ZRF sem pasta alternativa)",
      all(grepl("data-raw$", farms_test$databases_dir)) &&
        is.na(farms_test$databases_dir_alt[1]) &&
        grepl("^//192\\.168\\.1\\.11/.*/IDF_PortalData/BSH$", farms_test$databases_dir_alt[2]) &&
        grepl("^//192\\.168\\.1\\.11/.*/IDF_PortalData/DGY$", farms_test$databases_dir_alt[3]))
check("settings: so' as 2 especies", identical(settings_env_test$species_regional, c("Egyptian-Vulture", "Steppe-Eagle")))
check("settings: ini < end e no fuso do projeto",
      settings_env_test$ini < settings_env_test$end && attr(settings_env_test$ini, "tzone") == settings_env_test$proj_timezone)
check("settings: bin_min = 2 e merge_dist_m = 200", settings_env_test$bin_min == 2 && settings_env_test$merge_dist_m == 200)
check("settings: nao define patterns de curtailments/SCADA/heartbeats",
      !any(vapply(c("curtailments_pattern", "scada_pattern", "heartbeats_pattern"), exists, logical(1), envir = settings_env_test, inherits = FALSE)))

files_test <- c("TrackReport_Default_ZRF_20260901_20260930.csv", "TrackReport_Default_BSH_20250101.csv",
                "TrackReport_Default_DGY_20250801.csv", "Curtailments_ZRF_20260901.xlsx",
                "SCADA_ZRF_20260901.csv", "Heartbeats_BSH_20250101.csv")
hit <- function(i) files_test[grepl(farms_test$trackreport_pattern[i], files_test)]
check("padrao ZRF apanha so' o TrackReport de ZRF", identical(hit(1), files_test[1]))
check("padrao BSH apanha so' o TrackReport de BSH", identical(hit(2), files_test[2]))
check("padrao DGY apanha so' o TrackReport de DGY", identical(hit(3), files_test[3]))
check("'TrackReport_Default+ZRF' (regex literal) NAO apanha TrackReport_Default_ZRF_...",
      !grepl("TrackReport_Default+ZRF", files_test[1]))

# limites relativos a ini/end do settings (nao fixos -- a janela pode ser ajustada)
ini_t <- settings_env_test$ini; end_t <- settings_env_test$end
win_test <- regional_filter_window(
  data.table(timestamp = c(ini_t - 1, ini_t, end_t, end_t + 1)),
  ini_t, end_t
)
check("janela ini/end: mantem os limites inclusivos, descarta o resto", nrow(win_test) == 2L)


##
## 6. Linhas duplicadas (mesmo track_id + timestamp)
##
t0_test <- as.POSIXct("2026-03-02 10:00:00", tz = tz_test)
dup_test <- data.table(
  track_id  = c("a", "a", "a", "b", "b"),
  timestamp = t0_test + c(0, 0, 60, 0, 0),
  utm_x     = c(1, 1, 2, 5, 5.5),   # a@0 exato duplicado; b@0 repetido com utm_x diferente
  utm_y     = 0, spec = "Egyptian-Vulture"
)
dedup_test <- regional_dedup_tracks(dup_test)
check("dedup: 5 linhas -> 3 (1 duplicado exato + 1 com valor diferente)", nrow(dedup_test) == 3L && attr(dedup_test, "n_removed") == 2L)
check("dedup: mantem a 1a ocorrencia", dedup_test[track_id == "b", utm_x] == 5)
check("dedup: sem duplicados -> nada removido", attr(regional_dedup_tracks(dedup_test), "n_removed") == 0L)
bins_dup_test <- regional_min_individuals_bins(dup_test, "Egyptian-Vulture")
bins_dedup_test <- regional_min_individuals_bins(dedup_test, "Egyptian-Vulture")
check("duplicados nao alteram o minimo de individuos (a e b a 5 m -> 1)",
      identical(bins_dup_test$n_individuals_min, bins_dedup_test$n_individuals_min))


##
## 7. Cache incremental por ficheiro (so' le ficheiros novos/alterados)
##
## Ficheiros CSV simulados numa pasta temporaria; cache_io em RDS (fst nao
## precisa de ser instalado para testar a logica); read_file_fn conta chamadas.
##
source("R/read_utils.R")
dir_test <- file.path(tempdir(), "regional_cache_test_raw"); unlink(dir_test, recursive = TRUE)
dir.create(dir_test); cache_dir_test <- file.path(tempdir(), "regional_cache_test_cache"); unlink(cache_dir_test, recursive = TRUE)
rds_io_test <- list(read = function(path) readRDS(path), write = function(x, path) saveRDS(x, path))
n_calls_test <- 0L
read_file_test <- function(f) {
  n_calls_test <<- n_calls_test + 1L
  d <- fread(f); d[, timestamp := as.POSIXct(timestamp, tz = tz_test)]; d
}
write_csv_test <- function(name, ids, day) {
  fwrite(data.table(track_id = ids, timestamp = format(as.POSIXct(paste(day, "10:00:00"), tz = tz_test) + seq_along(ids)),
                    utm_x = 1, utm_y = 1, spec = "Egyptian-Vulture"), file.path(dir_test, name))
}
write_csv_test("TrackReport_Default_XXX_a.csv", c("a1", "a2"), "2025-03-01")
write_csv_test("TrackReport_Default_XXX_b.csv", c("b1", "b2", "b3"), "2025-03-02")
write_csv_test("TrackReport_Default_XXX_c.csv", c("c1"), "2025-03-03")
load_test <- function(pattern = "TrackReport_Default.*XXX", force = FALSE) {
  regional_load_farm("XXX", dir_test, pattern, farm_pattern = "XXX", tz = tz_test,
                     ini = as.POSIXct("2025-01-01", tz = tz_test), end = as.POSIXct("2025-12-31", tz = tz_test),
                     cache_dir = cache_dir_test, force_reread = force, read_file_fn = read_file_test, cache_io = rds_io_test)
}

r1 <- load_test()
check("cache: 1a corrida le os 3 ficheiros (6 linhas)", n_calls_test == 3L && nrow(r1$tracks) == 6L)

n_calls_test <- 0L; r2 <- load_test()
check("cache: 2a corrida sem alteracoes NAO le nenhum CSV (0 chamadas) e devolve os mesmos dados",
      n_calls_test == 0L && identical(sort(r2$tracks$track_id), sort(r1$tracks$track_id)))
check("cache: fuso do timestamp preservado", attr(r2$tracks$timestamp, "tzone") == tz_test)

write_csv_test("TrackReport_Default_XXX_b.csv", c("b1", "b2", "b3", "b4"), "2025-03-02")
n_calls_test <- 0L; r3 <- load_test()
check("cache: ficheiro alterado (tamanho) -> le so' esse (1 chamada), substitui as linhas antigas",
      n_calls_test == 1L && nrow(r3$tracks) == 7L && sum(r3$tracks$track_id == "b1") == 1L)

# so' a data de modificacao muda (mesmo tamanho)
Sys.setFileTime(file.path(dir_test, "TrackReport_Default_XXX_a.csv"), Sys.time() + 3600)
n_calls_test <- 0L; r3b <- load_test()
check("cache: so' mtime alterado -> relê esse ficheiro (1 chamada)", n_calls_test == 1L)

write_csv_test("TrackReport_Default_XXX_d.csv", c("d1", "d2"), "2025-03-04")
n_calls_test <- 0L; r4 <- load_test()
check("cache: ficheiro novo -> le so' o novo (1 chamada), 9 linhas", n_calls_test == 1L && nrow(r4$tracks) == 9L)

# ficheiros de outro parque / de outro tipo na mesma pasta nao entram
write_csv_test("TrackReport_Default_YYY_z.csv", c("z1"), "2025-03-05")
write_csv_test("SCADA_XXX_x.csv", c("s1"), "2025-03-05")
n_calls_test <- 0L; r4b <- load_test()
check("padrao/farm_pattern: ficheiros YYY e SCADA ignorados (0 leituras, 9 linhas)", n_calls_test == 0L && nrow(r4b$tracks) == 9L)

file.remove(file.path(dir_test, "TrackReport_Default_XXX_c.csv"))
n_calls_test <- 0L; r5 <- load_test()
check("cache: ficheiro desaparecido (pasta inacessivel) mantem as linhas em cache, sem ler nada",
      n_calls_test == 0L && nrow(r5$tracks) == 9L)

n_calls_test <- 0L; r6 <- load_test(pattern = "TrackReport_Default_XXX_[ab]")
check("cache: padrao mudou -> cache invalidada, relê so' os ficheiros do novo padrao (2 chamadas, 6 linhas)",
      n_calls_test == 2L && nrow(r6$tracks) == 6L)

n_calls_test <- 0L; r7 <- load_test(pattern = "TrackReport_Default_XXX_[ab]", force = TRUE)
check("cache: force_reread = TRUE relê tudo (2 chamadas)", n_calls_test == 2L)

# dedup entre ficheiros sobrepostos: o mesmo ponto em 2 ficheiros so' conta uma vez
write_csv_test("TrackReport_Default_XXX_a2.csv", c("a1", "a2"), "2025-03-01")
r8 <- load_test(pattern = "TrackReport_Default_XXX_(a|a2)[.]csv", force = TRUE)
check("dedup entre ficheiros sobrepostos: 4 linhas lidas -> 2", nrow(r8$tracks) == 2L)

check("0 ficheiros -> erro explicito",
      inherits(try(load_test(pattern = "NaoExiste"), silent = TRUE), "try-error"))


##
## 8. Tabelas e textos do relatorio
##
wp_test <- regional_weekly_peak_summary(weekly_all_test)
check("resumo de picos: 1 linha por especie x parque (3), ordem ZRF, BSH, DGY",
      nrow(wp_test) == 3L && identical(wp_test$farm, c("ZRF", "BSH", "DGY")))
check("resumo de picos: pico = 3 (altura dos pulsos simulados) e semanas com presenca > 0", all(wp_test$peak_weekly_max == 3L) && all(wp_test$weeks_present > 0L))

tt_test <- regional_format_timing_table(lead_test, pairs_test, season_dates_test, "Egyptian-Vulture", "autumn")
check("tabela timing outono: 2 linhas (ZRF->BSH, ZRF->DGY), lead mediana 5 e 7",
      nrow(tt_test) == 2L && identical(tt_test$Pair, c("ZRF -> BSH", "ZRF -> DGY")) && identical(tt_test$`Lead, median (d)`, c("5", "7")))
tt_gap_test <- regional_format_timing_table(lead_test[0], pairs_test, season_dates_test, c("Egyptian-Vulture", "Steppe-Eagle"), "spring")
check("tabela timing sem dados: linhas esperadas aparecem como 'insufficient data'",
      nrow(tt_gap_test) == 4L && all(tt_gap_test$`Lead, median (d)` == "insufficient data"))
ct_test <- regional_format_ccf_table(ccf_sum_test, pairs_test, season_dates_test, "Egyptian-Vulture", "spring")
check("tabela ccf primavera: lag 6 nos 2 pares", nrow(ct_test) == 2L && all(ct_test$`Best lag (d)` == "6"))
pt_test <- regional_format_pulse_table(pulse_sum_test, timing_test, pairs_test, season_dates_test, "Egyptian-Vulture", "autumn")
check("tabela pulsos outono: 4 pulsos do lider, 100% seguidos, lag mediano 5 (ZRF -> BSH)",
      pt_test$`Leader pulses`[1] == "4" && pt_test$`% followed`[1] == "100" && pt_test$`Median lag (d)`[1] == "5")
pt_gap_test <- regional_format_pulse_table(pulse_sum_test[0], timing_gap_test, pairs_test, season_dates_test, "Egyptian-Vulture", "autumn")
check("tabela pulsos: DGY sem cobertura -> 'insufficient data'; BSH sem pulsos mas com dados -> 0",
      pt_gap_test$`Leader pulses`[2] == "insufficient data" && pt_gap_test$`Leader pulses`[1] == "0")
check("titulos: outono e primavera", identical(regional_season_title(pairs_test, "autumn", "Autumn"), "Autumn: does ZRF anticipate BSH and DGY?") &&
        identical(regional_season_title(pairs_test, "spring", "Spring"), "Spring: do BSH and DGY anticipate ZRF?"))
check("altura da figura cresce com os paineis e tem teto",
      regional_fig_height(plot_regional_season_series(daily_pair_test, season_dates_test[season == "autumn"], sp)) <
        regional_fig_height(plot_regional_season_series(daily_pair_test, season_dates_test, sp)))
check("tabelas: Year como texto (sem separador de milhares, ex: 2025 e nao 2,025)",
      identical(tt_test$Year, c("2025", "2025")) && is.character(ct_test$Year) && is.character(pt_test$Year))


##
## 9. Tipos consistentes entre grupos (regressao: erro real com dados reais, 2026-10,
## "Column 6 ... is type 'integer' but expecting type 'double'")
##
na_int_date_test <- structure(NA_integer_, class = "Date") # NA de data com tipo inteiro (como as.Date(NA) em algumas versoes do R)
check("timing: colunas de data sempre double, mesmo com grupos sem cobertura",
      all(vapply(timing_gap_test[, .(onset, median, end)], typeof, "") == "double"))
# 3 grupos farm x spec: valido, valido, sem cobertura (o caso que partiu)
daily_mix_test <- copy(daily_pair_test)
daily_mix_test[farm == "DGY", `:=`(monitored = FALSE, max_individuals = NA_integer_)]
mix_ok <- tryCatch({ regional_passage_timing(daily_mix_test, season_dates_test); TRUE }, error = function(e) FALSE)
check("timing: grupos validos e invalidos no mesmo calculo nao dao erro de tipos", mix_ok)

# mediana de lags: grupos com n impar (inteiro) e n par (double) no mesmo resumo
match_types_test <- data.table(
  spec = "Egyptian-Vulture", season = "autumn", season_year = 2025L, leader = "ZRF",
  follower = rep(c("BSH", "DGY"), c(3L, 2L)),
  leader_pulse_start = as.Date("2025-09-01") + 0:4, leader_pulse_peak = 1L,
  lag_forward_days = c(3L, 5L, 7L, 3L, 4L), follower_pulse_before = FALSE
)
ps_types_test <- tryCatch(regional_pulse_summary(match_types_test), error = function(e) NULL)
check("pulsos: mediana de lags com n impar e par no mesmo resumo (5 e 3.5)",
      !is.null(ps_types_test) && identical(ps_types_test$median_lag_days, c(5, 3.5)))

ccf_types_test <- regional_ccf_summary(ccf_test)
check("ccf: lag e sobreposicao sempre inteiros", is.integer(ccf_types_test$best_lag_days) && is.integer(ccf_types_test$n_overlap_lag0))


##
## 10. Grafico anual relativo: valores entre 0 e 1 (nao truncados a 0/1)
##
weekly_rel_test <- data.table(
  farm = "ZRF", spec = "Egyptian-Vulture", week_start = as.Date("2025-09-01") + 7 * 0:3,
  n_days_monitored = 7L, max_individuals = c(1L, 2L, 3L, 4L), iso_year = 2025L, iso_week = 36:39
)
p_rel_test <- plot_regional_annual_overlay(weekly_rel_test, "Egyptian-Vulture", normalise = TRUE)
y_rel_test <- ggplot2::ggplot_build(p_rel_test)$data[[1]]$y
check("grafico relativo: valores 0.25, 0.5, 0.75, 1 (sem truncar)", isTRUE(all.equal(sort(y_rel_test), c(0.25, 0.5, 0.75, 1))))
check("grafico relativo: nao altera os dados de origem (continuam inteiros)", is.integer(weekly_rel_test$max_individuals))
check("grafico relativo sem avisos", length(withCallingHandlers(
  { plot_regional_annual_overlay(weekly_rel_test, "Egyptian-Vulture", normalise = TRUE); list() },
  warning = function(w) stop("aviso: ", conditionMessage(w)))) == 0L)
