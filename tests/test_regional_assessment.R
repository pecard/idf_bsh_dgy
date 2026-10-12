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
                    utm_x = 1, utm_y = 1, spec = "Egyptian-Vulture", idf = 7L), file.path(dir_test, name))
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


##
## 11. Tudo circunscrito a' janela ini/end
##
win_dates_test <- as.Date(c("2026-02-23", "2026-03-22")) # 4 semanas inteiras (segunda a domingo)
mon_win_test <- mon_test; mon_win_test$first_day <- win_dates_test[1]; mon_win_test$last_day <- win_dates_test[2]
daily_win_test <- regional_daily_max(bins_test, "Egyptian-Vulture", mon_win_test, tz_test)
check("serie diaria cobre exatamente a janela (28 dias)", nrow(daily_win_test) == 28L &&
        min(daily_win_test$day) == win_dates_test[1] && max(daily_win_test$day) == win_dates_test[2])
check("dias da janela sem tracks (antes dos dados) ficam NA, nao 0",
      all(is.na(daily_win_test[day < as.Date("2026-03-02"), max_individuals])))
check("tracks fora da janela nao entram (dados de 2026-03-02 a 2026-03-11 -> dia 2026-03-11 dentro)",
      daily_win_test[day == as.Date("2026-03-03"), max_individuals] == 2L)
check("regional_check_window aceita dados dentro da janela", isTRUE(regional_check_window(daily_win_test, win_dates_test[1], win_dates_test[2])))
check("regional_check_window para se houver dias fora da janela",
      inherits(try(regional_check_window(daily_win_test, win_dates_test[1] + 10, win_dates_test[2]), silent = TRUE), "try-error"))

weekly_win_test <- regional_weekly_max(daily_win_test, min_monitored_days = 4L)
weekly_win_test[, farm := "BSH"]
p_win_test <- plot_regional_weekly_timeline(weekly_win_test, "Egyptian-Vulture", win_dates_test)
xr_test <- ggplot2::ggplot_build(p_win_test)$layout$panel_params[[1]]$x.range
check("grafico semanal: eixo x = janela ini/end (sem folga)", isTRUE(all.equal(xr_test, as.numeric(win_dates_test))))
check("grafico semanal: legenda indica a janela", grepl("2026-02-23 to 2026-03-22", p_win_test$labels$caption, fixed = TRUE))


##
## 12. Esforco: unidades IDF ativas e area vigiada
##
check("numero da unidade: BSH68-01 -> 1, DZH01-01 -> 1, IDF-22 -> 22, '22' -> 22, 09 -> 9",
      identical(regional_unit_number(c("BSH68-01", "DZH01-01", "IDF-22", "22", "09", "sem digitos")), c(1L, 1L, 22L, 22L, 9L, NA)))

t_week1 <- as.POSIXct("2026-03-02 10:00:00", tz = tz_test) # segunda
trk_units_test <- data.table(
  track_id = paste0("u", 1:6), timestamp = t_week1 + c(0, 60, 86400 * 8, 86400 * 8 + 60, 86400 * 9, 86400 * 9),
  utm_x = 0, utm_y = 0, spec = "Golden-Eagle", idf = c(1L, 2L, 2L, 3L, 3L, NA)
)
wu_test <- regional_weekly_active_units(trk_units_test, tz_test)
check("unidades ativas: semana 1 = {1,2}, semana 2 = {2,3} (NA ignorado)",
      identical(wu_test[week_start == as.Date("2026-03-02"), sort(unit)], c(1L, 2L)) &&
        identical(wu_test[week_start == as.Date("2026-03-09"), sort(unit)], c(2L, 3L)))

crs_test <- 32641
r_test <- 1000
pts_test <- sf::st_sf(unit = c(1L, 2L, 3L, 4L),
                      geometry = sf::st_sfc(sf::st_point(c(500000, 4000000)), sf::st_point(c(500000, 4000000)), # 2 = igual a 1
                                            sf::st_point(c(500500, 4000000)), sf::st_point(c(520000, 4000000)), crs = crs_test))
circle_km2 <- pi * (r_test / 1000)^2
wu2_test <- data.table(week_start = as.Date(c("2026-03-02", "2026-03-02", "2026-03-09", "2026-03-09", "2026-03-16", "2026-03-16", "2026-03-23", "2026-03-23")),
                       unit = c(1L, 2L, 1L, 4L, 1L, 3L, 1L, 99L))
area_test <- regional_surveyed_area(wu2_test, pts_test, r_test)
ar <- function(w) area_test[week_start == as.Date(w), area_km2]
check("area vigiada: 2 unidades no mesmo ponto = 1 circulo", abs(ar("2026-03-02") / circle_km2 - 1) < 0.01)
check("area vigiada: 2 unidades afastadas 20 km = 2 circulos", abs(ar("2026-03-09") / (2 * circle_km2) - 1) < 0.01)
check("area vigiada: sobreposicao parcial (500 m) entre 1 e 2 circulos", ar("2026-03-16") > circle_km2 * 1.2 && ar("2026-03-16") < circle_km2 * 1.9)
check("area vigiada: unidade sem coordenadas conta nas ativas mas nao na area",
      area_test[week_start == as.Date("2026-03-23"), n_active_units] == 2L &&
        area_test[week_start == as.Date("2026-03-23"), n_units_with_coords] == 1L &&
        abs(ar("2026-03-23") / circle_km2 - 1) < 0.01)
check("unidades sem coordenadas listadas", identical(regional_units_without_coords(wu2_test, pts_test)$unit, 99L))

weekly_n_test <- data.table(farm = "BSH", spec = "Egyptian-Vulture", week_start = as.Date("2026-03-02") + 7 * 0:1,
                            max_individuals = c(4L, 6L), n_days_monitored = 7L)
eff_n_test <- data.table(farm = "BSH", week_start = as.Date("2026-03-02") + 7 * 0:1, n_active_units = c(2L, 3L),
                         n_units_with_coords = c(2L, 3L), area_km2 = c(10, 20))
norm_test <- regional_normalise_weekly(weekly_n_test, eff_n_test)
check("indice por unidade ativa: 4/2 = 2 e 6/3 = 2", identical(norm_test$max_per_active_unit, c(2, 2)))
check("densidade por 100 km2: 4/10*100 = 40 e 6/20*100 = 30", isTRUE(all.equal(norm_test$density_per_100km2, c(40, 30))))
check("max_individuals continua inteiro", is.integer(norm_test$max_individuals))
pk_n_test <- regional_norm_peak_summary(norm_test)
check("resumo de picos normalizados", pk_n_test$peak_weekly_max == 6L && pk_n_test$peak_per_active_unit == 2 && pk_n_test$peak_density_per_100km2 == 40)
es_test <- regional_effort_summary(eff_n_test)
check("resumo de esforco: media de unidades ativas 2.5 e area 15 km2", es_test$mean_active_units == 2.5 && es_test$mean_surveyed_area_km2 == 15)

# shapefiles reais do projeto
shp_bsh <- regional_load_idf_units("inputs/Bash_IDF_coord.shp", "imaging_he", crs_test)
shp_dgy <- regional_load_idf_units("inputs/IDF_DZH.shp", "Name", crs_test)
shp_zrf <- regional_load_idf_units("inputs/identiflight.shp", "Name", crs_test)
check("shapefile BSH: 43 unidades com numeros 1..43", nrow(shp_bsh) == 43L && identical(sort(shp_bsh$unit), 1:43))
check("shapefile DGY: 4 instaladas + DZH-23 (duplicado so' 1x) = 5, em metros (reprojetado)", nrow(shp_dgy) == 5L && sf::st_crs(shp_dgy)$epsg == crs_test)
check("shapefile ZRF: 74 unidades, numeros unicos", nrow(shp_zrf) == 74L && !anyDuplicated(shp_zrf$unit))
wu_bsh_test <- data.table(week_start = as.Date("2026-03-02"), unit = 1:43)
a_bsh <- regional_surveyed_area(wu_bsh_test, shp_bsh, 1000)$area_km2
check("area BSH (43 unidades, 1 km): com sobreposicao < 43 circulos e > 1 circulo", a_bsh < 43 * circle_km2 && a_bsh > circle_km2)
cat(sprintf("   (area vigiada BSH, 43 unidades x 1 km de raio: %.0f km2 vs %.0f km2 sem sobreposicao)\n", a_bsh, 43 * circle_km2))

##
## 13. Pulsos com limiar relativo ao parque
##
daily_rel_test <- rbindlist(list(
  data.table(farm = "BSH", spec = "Egyptian-Vulture", day = as.Date("2026-03-01") + 0:29,
             max_individuals = 0L, monitored = TRUE),
  data.table(farm = "DGY", spec = "Egyptian-Vulture", day = as.Date("2026-03-01") + 0:29,
             max_individuals = 0L, monitored = TRUE)
))
daily_rel_test[farm == "BSH" & day == as.Date("2026-03-05"), max_individuals := 12L] # pulso grande
daily_rel_test[farm == "BSH" & day == as.Date("2026-03-20"), max_individuals := 2L]  # pulso pequeno (17% do pico)
daily_rel_test[farm == "DGY" & day == as.Date("2026-03-05"), max_individuals := 4L]
daily_rel_test[farm == "DGY" & day == as.Date("2026-03-20"), max_individuals := 1L]
seas_rel_test <- data.table(season = "spring", season_year = 2026L, from = as.Date("2026-03-01"), to = as.Date("2026-03-30"))
pul_abs_test <- regional_pulses(daily_rel_test, min_individuals = 1L, merge_gap_days = 3L)
pul_rel_test <- regional_pulses(daily_rel_test, min_individuals = 1L, merge_gap_days = 3L, relative_share = 0.5, season_dates = seas_rel_test)
check("pulsos absolutos (>= 1): BSH 2, DGY 2", pul_abs_test[farm == "BSH", .N] == 2L && pul_abs_test[farm == "DGY", .N] == 2L)
check("pulsos relativos (>= 50% do pico da epoca): BSH 1 (so' o grande), DGY 1 (pico 4, o dia de 1 cai)",
      pul_rel_test[farm == "BSH", .N] == 1L && pul_rel_test[farm == "BSH", peak] == 12L && pul_rel_test[farm == "DGY", .N] == 1L)
check("pulsos: sem relative_share o comportamento antigo mantem-se", isTRUE(all.equal(pul_abs_test, regional_pulses(daily_rel_test, 1L, 3L, NULL, seas_rel_test), check.attributes = FALSE)))

##
## 14. Rarefacao
##
mk_rare <- function(id, unit, day, minute, x) data.table(track_id = id, timestamp = as.POSIXct(paste(day, "10:00:00"), tz = tz_test) + minute * 60,
                                                          utm_x = x, utm_y = 0, spec = "Egyptian-Vulture", idf = unit)
# semana de 2026-03-02: 6 individuos distintos (1000 m entre si), cada um visto por 1 unidade (1..6), no mesmo bin
trk_rare_test <- rbindlist(lapply(1:6, function(u) mk_rare(paste0("r", u), u, "2026-03-03", 0, u * 1000)))
trk_rare_all <- rbind(trk_rare_test, data.table(track_id = "fill", timestamp = as.POSIXct("2026-03-04 10:00:00", tz = tz_test),
                                                utm_x = 0, utm_y = 0, spec = "Golden-Eagle", idf = 1L))
wu_rare_test <- data.table(week_start = as.Date("2026-03-02"), unit = 1:6)
mon_rare_test <- list(monitored = as.Date("2026-03-02") + 0:6, first_day = as.Date("2026-03-02"), last_day = as.Date("2026-03-08"))
rare_full <- regional_rarefied_weekly(trk_rare_all, "Egyptian-Vulture", wu_rare_test, k = 6, n_draws = 3, bin_min = 2, merge_dist_m = 200,
                                      tz = tz_test, monitored_info = mon_rare_test, min_monitored_days = 4L, seed = 1)
check("rarefacao com k >= unidades ativas = todas as unidades (maximo 6)", rare_full$rarefied_median == 6 && rare_full$rarefied_p10 == 6)
rare_k2 <- regional_rarefied_weekly(trk_rare_all, "Egyptian-Vulture", wu_rare_test, k = 2, n_draws = 10, bin_min = 2, merge_dist_m = 200,
                                    tz = tz_test, monitored_info = mon_rare_test, min_monitored_days = 4L, seed = 1)
check("rarefacao com k = 2: sempre 2 individuos (cada unidade ve 1)", rare_k2$rarefied_median == 2 && rare_k2$rarefied_p90 == 2)
rare_k2b <- regional_rarefied_weekly(trk_rare_all, "Egyptian-Vulture", wu_rare_test, k = 2, n_draws = 10, bin_min = 2, merge_dist_m = 200,
                                     tz = tz_test, monitored_info = mon_rare_test, min_monitored_days = 4L, seed = 1)
check("rarefacao reprodutivel com a mesma seed", identical(rare_k2, rare_k2b))
cache_rare_test <- file.path(tempdir(), "rare_cache_test.rds"); unlink(cache_rare_test)
r_c1 <- regional_rarefied_weekly(trk_rare_all, "Egyptian-Vulture", wu_rare_test, 2, 4, 2, 200, tz_test, mon_rare_test, 4L, 1, cache_rare_test)
n_msg_test <- 0L
r_c2 <- withCallingHandlers(regional_rarefied_weekly(trk_rare_all, "Egyptian-Vulture", wu_rare_test, 2, 4, 2, 200, tz_test, mon_rare_test, 4L, 1, cache_rare_test),
                            message = function(m) { if (grepl("cache", conditionMessage(m))) n_msg_test <<- n_msg_test + 1L; invokeRestart("muffleMessage") })
check("rarefacao: 2a chamada com os mesmos parametros vem da cache", n_msg_test == 1L && isTRUE(all.equal(r_c1, r_c2)))

##
## 15. Plots de esforco e normalizacao
##
eff_plot_test <- rbindlist(lapply(c("ZRF", "BSH", "DGY"), function(f) data.table(farm = f, week_start = as.Date("2026-03-02") + 7 * 0:5,
                          n_active_units = 5L, n_units_with_coords = 5L, area_km2 = 12)))
wk_plot_test <- rbindlist(lapply(c("ZRF", "BSH", "DGY"), function(f) data.table(farm = f, spec = "Egyptian-Vulture",
                          week_start = as.Date("2026-03-02") + 7 * 0:5, max_individuals = 1:6, n_days_monitored = 7L)))
norm_plot_test <- regional_normalise_weekly(wk_plot_test, eff_plot_test)
rare_plot_test <- rbindlist(lapply(c("ZRF", "BSH", "DGY"), function(f) { r <- data.table(farm = f, spec = "Egyptian-Vulture", week_start = as.Date("2026-03-02") + 7 * 0:5,
                          rarefied_median = c(1, 2, 2, 3, 3, 4), rarefied_p10 = 1, rarefied_p90 = 5); r }))
win_plot_test <- as.Date(c("2026-03-02", "2026-04-12"))
check("plot de esforco", is_gg(plot_regional_effort(eff_plot_test, win_plot_test)))
check("plot normalizado (3 paineis)", is_gg(plot_regional_normalised(norm_plot_test, "Egyptian-Vulture", win_plot_test)))
check("plot rarefacao", is_gg(plot_regional_rarefied(rare_plot_test, wk_plot_test, "Egyptian-Vulture", 4L, win_plot_test)))
invisible(lapply(list(plot_regional_effort(eff_plot_test, win_plot_test), plot_regional_normalised(norm_plot_test, "Egyptian-Vulture", win_plot_test),
                      plot_regional_rarefied(rare_plot_test, wk_plot_test, "Egyptian-Vulture", 4L, win_plot_test)), ggplot2::ggplot_build))
cat("ggplot_build() dos plots de esforco sem erro -- OK\n")


##
## 16. Significancia (permutacao), melhor lag no limite e epocas parciais
##
mk_ser_test <- function(farm, starts, from, to, width = 3L, height = 3L, spec = "Egyptian-Vulture") {
  d <- data.table(farm = farm, spec = spec, day = seq(as.Date(from), as.Date(to), by = "day")); d[, `:=`(monitored = TRUE, max_individuals = 0L)]
  for (s in as.Date(starts)) d[day >= s & day < s + width, max_individuals := as.integer(height)]
  d[]
}
a0_test <- as.Date("2025-09-01")
seas_perm_test <- regional_season_dates(list(autumn = c("08-15", "12-15")), 2025L)
pair_perm_test <- data.table(season = "autumn", leader = "ZRF", follower = "BSH")
d_lead_test <- rbind(mk_ser_test("ZRF", a0_test + c(0, 20, 45, 70), "2025-08-01", "2026-01-31"),
                     mk_ser_test("BSH", a0_test + c(0, 20, 45, 70) + 5, "2025-08-01", "2026-01-31"))
d_rand_test <- rbind(mk_ser_test("ZRF", a0_test + c(0, 20, 45, 70), "2025-08-01", "2026-01-31"),
                     mk_ser_test("BSH", a0_test + c(10, 33, 58, 85, 96), "2025-08-01", "2026-01-31"))
perm_lead_test <- regional_ccf_permutation(d_lead_test, pair_perm_test, seas_perm_test, 15L, 1L, 20L, n_perm = 99L, seed = 1)
perm_rand_test <- regional_ccf_permutation(d_rand_test, pair_perm_test, seas_perm_test, 15L, 1L, 20L, n_perm = 99L, seed = 1)
check("permutacao: lead real (lag 5) -> p <= 0.05", perm_lead_test$p_value <= 0.05 && perm_lead_test$n_perm == 99L)
check("permutacao: pulsos sem relacao -> p > 0.05", perm_rand_test$p_value > 0.05)
check("permutacao: reprodutivel com a mesma seed",
      identical(perm_lead_test, regional_ccf_permutation(d_lead_test, pair_perm_test, seas_perm_test, 15L, 1L, 20L, n_perm = 99L, seed = 1)))
ccf_lead_test <- regional_ccf(d_lead_test, pair_perm_test, seas_perm_test, 15L, 1L, 20L)
cov_lead_test <- regional_season_coverage(d_lead_test, pair_perm_test, seas_perm_test)
sum_lead_test <- regional_ccf_summary(ccf_lead_test, perm_lead_test, cov_lead_test)
check("estatistica observada da permutacao = r no melhor lag da tabela", isTRUE(all.equal(perm_lead_test$stat_obs, sum_lead_test$r_best)))
check("resumo junta p-valor e cobertura; melhor lag 5 nao esta' no limite",
      sum_lead_test$best_lag_days == 5L && !sum_lead_test$best_lag_at_limit && sum_lead_test$p_value == perm_lead_test$p_value)
check("cobertura da epoca: 100% quando os 2 parques tem toda a epoca", sum_lead_test$coverage_both == 1 && sum_lead_test$n_season_days == 123L)

# melhor lag no limite do intervalo -> "no peak"
ccf_edge_test <- data.table(spec = "Egyptian-Vulture", season = "autumn", season_year = 2025L, leader = "ZRF", follower = "BSH",
                            lag_days = -15:15, r = seq(0, 1, length.out = 31), n_overlap = 60L)
sum_edge_test <- regional_ccf_summary(ccf_edge_test)
check("melhor lag no limite (+15) -> best_lag_at_limit = TRUE", sum_edge_test$best_lag_days == 15L && sum_edge_test$best_lag_at_limit)
tab_edge_test <- regional_format_ccf_table(sum_edge_test, pair_perm_test, seas_perm_test, "Egyptian-Vulture", "autumn")
check("tabela: melhor lag no limite aparece como 'no peak (at +15 d limit)'", tab_edge_test$`Best lag (d)` == "no peak (at +15 d limit)")
ccf_edge_neg_test <- copy(ccf_edge_test)[, r := rev(r)]
check("limite negativo tambem: 'no peak (at -15 d limit)'",
      regional_format_ccf_table(regional_ccf_summary(ccf_edge_neg_test), pair_perm_test, seas_perm_test, "Egyptian-Vulture", "autumn")$`Best lag (d)` == "no peak (at -15 d limit)")

# epoca parcial: BSH sem monitorizacao em metade da epoca
d_part_test <- copy(d_lead_test)
d_part_test[farm == "BSH" & day >= as.Date("2025-10-15"), `:=`(monitored = FALSE, max_individuals = NA_integer_)]
cov_part_test <- regional_season_coverage(d_part_test, pair_perm_test, seas_perm_test)
check("cobertura parcial: dias com os 2 parques monitorizados / dias da epoca (Aug 15 - Oct 14 = 61 de 123)",
      cov_part_test$n_both_days == 61L && abs(cov_part_test$coverage_both - 61 / 123) < 1e-9)
perm_part_test <- regional_ccf_permutation(d_part_test, pair_perm_test, seas_perm_test, 15L, 1L, 20L, n_perm = 20L, seed = 1)
sum_part_test <- regional_ccf_summary(regional_ccf(d_part_test, pair_perm_test, seas_perm_test, 15L, 1L, 20L), perm_part_test, cov_part_test)
tab_part_test <- regional_format_ccf_table(sum_part_test, pair_perm_test, seas_perm_test, "Egyptian-Vulture", "autumn", 0.7)
check("tabela ccf: epoca com 50% de cobertura marcada '(partial)'", tab_part_test$`Season covered` == "50% (partial)")
tab_full_test <- regional_format_ccf_table(sum_lead_test, pair_perm_test, seas_perm_test, "Egyptian-Vulture", "autumn", 0.7)
check("tabela ccf: epoca completa '100%' sem '(partial)'", tab_full_test$`Season covered` == "100%")
tm_part_test <- regional_passage_timing(d_part_test, seas_perm_test)
lead_part_test <- regional_lead_table(tm_part_test, pair_perm_test)
check("tabela timing: coluna 'Season covered' com a marca de epoca parcial",
      regional_format_timing_table(lead_part_test, pair_perm_test, seas_perm_test, "Egyptian-Vulture", "autumn", cov_part_test, 0.7)$`Season covered` == "50% (partial)")
check("tabela timing sem cobertura mantem o formato antigo (sem a coluna)",
      !"Season covered" %in% names(regional_format_timing_table(lead_part_test, pair_perm_test, seas_perm_test, "Egyptian-Vulture", "autumn")))
ps_part_test <- regional_pulse_summary(regional_match_pulses(regional_pulses(d_part_test, 1L, 3L), pair_perm_test, seas_perm_test, 10L))
check("tabela pulsos: coluna 'Season covered'",
      "Season covered" %in% names(regional_format_pulse_table(ps_part_test, tm_part_test, pair_perm_test, seas_perm_test, "Egyptian-Vulture", "autumn", cov_part_test, 0.7)))

# texto do p-valor
check("p-valor: valor minimo possivel (1/(n+1)) aparece como '<= 0.010', outros com 3 casas",
      identical(.regional_p_text(c(0.01, 0.25, NA), 99L), c("<= 0.010", "0.250", "insufficient data")))


##
## 17. Cobertura diaria dos TrackReports por unidade IDF
##
ini_c <- as.Date("2026-03-01"); end_c <- as.Date("2026-03-30") # 30 dias
days_c <- seq(ini_c, end_c, by = "day")
mk_unit_tracks <- function(unit, missing_days, n_per_day = 3L) {
  d <- setdiff(as.character(days_c), as.character(missing_days))
  rbindlist(lapply(d, function(day) data.table(
    track_id = paste0("u", unit, "_", day, "_", seq_len(n_per_day)),
    timestamp = as.POSIXct(paste(day, "10:00:00"), tz = tz_test) + seq_len(n_per_day), idf = unit)))
}
# unidade 1: sem os dias 12-14 (3 dias); unidade 2: sem 10-14 e 20-29 (15 dias); unidade 3: sem 12-14 e 20-29 (13 dias)
trk_cov_test <- rbindlist(list(
  mk_unit_tracks(1L, as.Date("2026-03-12") + 0:2),
  mk_unit_tracks(2L, c(as.Date("2026-03-10") + 0:4, as.Date("2026-03-20") + 0:9)),
  mk_unit_tracks(3L, c(as.Date("2026-03-12") + 0:2, as.Date("2026-03-20") + 0:9))
))
# unidade 1 tambem sem 20-29 -> dias 12-14 sem nenhuma unidade; 20-29 sem nenhuma unidade
trk_cov_test <- rbindlist(list(
  mk_unit_tracks(1L, c(as.Date("2026-03-12") + 0:2, as.Date("2026-03-25") + 0:5)),   # sem 12-14 e 25-30
  mk_unit_tracks(2L, c(as.Date("2026-03-10") + 0:4, as.Date("2026-03-20") + 0:10)),  # sem 10-14 e 20-30
  mk_unit_tracks(3L, c(as.Date("2026-03-12") + 0:2, as.Date("2026-03-20") + 0:10))   # sem 12-14 e 20-30
))
ud_test <- regional_unit_daily_coverage(trk_cov_test, tz_test, ini_c, end_c, 1L)
us_test <- regional_unit_coverage_summary(ud_test, ini_c, end_c, gap_min_days = 3L)
check("cobertura por unidade: 3 unidades, 30 dias na janela", nrow(us_test) == 3L && all(us_test$days_total == 30L))
check("unidade 1: 30 - 3 - 6 = 21 dias cobertos (70%)", us_test[unit == 1L, days_covered] == 21L && us_test[unit == 1L, pct_covered] == 70)
check("unidade 2: 30 - 5 - 11 = 14 dias cobertos", us_test[unit == 2L, days_covered] == 14L)
check("lacunas >= 3 dias: unidade 1 tem 2 (12-14 e 25-30), a mais longa com 6 dias",
      us_test[unit == 1L, n_gaps] == 2L && us_test[unit == 1L, longest_gap_days] == 6L)
check("primeiro e ultimo dia cobertos da unidade 2", us_test[unit == 2L, first_day] == as.Date("2026-03-01") && us_test[unit == 2L, last_day] == as.Date("2026-03-19"))
check("tracks fora da janela nao contam", nrow(regional_unit_daily_coverage(trk_cov_test, tz_test, as.Date("2026-03-05"), as.Date("2026-03-09"), 1L)[day < as.Date("2026-03-05")]) == 0L)
ud_min_test <- regional_unit_daily_coverage(trk_cov_test, tz_test, ini_c, end_c, min_tracks = 4L)
check("min_tracks = 4 (cada unidade tem 3 tracks/dia) -> nenhum dia coberto", nrow(ud_min_test) == 0L)

fd_test <- regional_farm_daily_coverage(ud_test, ini_c, end_c)
check("cobertura do parque: 30 dias, 3 unidades de referencia", nrow(fd_test) == 30L && all(fd_test$n_units_ref == 3L))
check("dia 12 (todas sem registos) = 0 unidades; dia 1 = 3 unidades",
      fd_test[day == as.Date("2026-03-12"), n_units_reporting] == 0L && fd_test[day == as.Date("2026-03-01"), n_units_reporting] == 3L)
check("dia 22 (so' a unidade 1 reporta) = 1/3 das unidades; dia 10 (unidades 1 e 3) = 2/3",
      abs(fd_test[day == as.Date("2026-03-22"), share_units] - 1 / 3) < 1e-9 && abs(fd_test[day == as.Date("2026-03-10"), share_units] - 2 / 3) < 1e-9)
fg_test <- regional_farm_gaps(fd_test, gap_min_days = 3L, low_share = 0.5)
check("periodos sem nenhuma unidade (>= 3 d): 12-14 (3 dias) e 25-30 (6 dias)",
      identical(fg_test[type == "no unit with records", as.character(start)], c("2026-03-12", "2026-03-25")) &&
        identical(fg_test[type == "no unit with records", n_days], c(3L, 6L)))
check("periodos com < 50% das unidades (>= 3 d): so' 20-24 (5 dias, so' a unidade 1)",
      identical(fg_test[grepl("under 50%", type), as.character(start)], "2026-03-20") && identical(fg_test[grepl("under 50%", type), n_days], 5L))
fsum_test <- regional_farm_coverage_summary(fd_test, us_test)
check("resumo do parque: dias com alguma unidade = 30 - 3 - 6 = 21", fsum_test$days_any_unit == 21L && fsum_test$units_with_records == 3L && fsum_test$units_below_90 == 3L)

files_c_test <- trk_cov_test[, source_file := fifelse(idf == 1L, "a.csv", "b.csv")]
fs_test <- regional_file_summary(files_c_test, tz_test)
check("resumo de ficheiros: 2 ficheiros, a.csv com 21 dias e 63 registos",
      nrow(fs_test) == 2L && fs_test[file == "a.csv", days_with_records] == 21L && fs_test[file == "a.csv", records] == 63L)
check("tabela de unidades formatada", all(c("Coverage (%)", "Longest gap (d)") %in% names(regional_format_unit_coverage_table(us_test))))
check("tabela de lacunas formatada", nrow(regional_format_gaps_table(cbind(farm = "BSH", fg_test))) == nrow(fg_test))
check("tabela de ficheiros formatada", nrow(regional_format_files_table(cbind(farm = "BSH", fs_test))) == 2L)
check("plot de cobertura por unidade", is_gg(plot_regional_unit_coverage(ud_test, us_test, ini_c, end_c, "BSH")))
check("plot de cobertura do parque", is_gg(plot_regional_farm_coverage(cbind(farm = "BSH", fd_test), c(ini_c, end_c))))
invisible(lapply(list(plot_regional_unit_coverage(ud_test, us_test, ini_c, end_c, "BSH"), plot_regional_farm_coverage(cbind(farm = "BSH", fd_test), c(ini_c, end_c))), ggplot2::ggplot_build))
cat("ggplot_build() dos plots de cobertura sem erro -- OK\n")
