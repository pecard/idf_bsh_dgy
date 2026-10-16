## Teste: curtailments contam como evidencia SO' para a unidade que os disparou
suppressPackageStartupMessages({ library(data.table); library(lubridate) })
source("R/offline_curtailment_check.R")
tz_test <- "Asia/Samarkand"
t0_test <- as.POSIXct("2026-06-01 10:00:00", tz = tz_test)
# 2 unidades da mesma turbina, ambas offline 10:00-11:00
offline_dt_test <- data.table(idf = c("GW19-13", "GW23-15"), off_start = t0_test, off_end = t0_test + 3600)
idf_turbines_test <- data.table(idf = c("GW19-13", "GW23-15"), turbine = "T23")
# 1 curtailment da turbina T23 as 10:30, disparado pelo track "tA", visto pela unidade 13
curtl_dt_test <- data.table(turbine = "T23", track_id = "tA", start = t0_test + 1800, end = t0_test + 1900)
track_dt_test <- data.table(track_id = c("tA", "tA", "tB"), timestamp = c(t0_test + 1700, t0_test + 1790, t0_test + 100), idf = c(13L, 13L, 15L))

old_test <- check_offline_curtailment_overlap(offline_dt_test, curtl_dt_test, idf_turbines_test)  # sem track_dt: por turbina
stopifnot(all(old_test$has_curtailment))  # comportamento antigo: ambas as unidades "recebem" o curtailment

new_test <- check_offline_curtailment_overlap(offline_dt_test, curtl_dt_test, idf_turbines_test, track_dt = track_dt_test)
stopifnot(new_test[idf == "GW19-13", has_curtailment], !new_test[idf == "GW23-15", has_curtailment])

# unidade atribuida pelo ponto mais proximo no tempo: track que muda de unidade
track_switch_test <- data.table(track_id = c("tA", "tA"), timestamp = c(t0_test + 1000, t0_test + 1790), idf = c(15L, 13L))
att_test <- attribute_curtailment_unit(curtl_dt_test, track_switch_test)
stopifnot(att_test$curtl_unit == 13L)

# curtailment sem track em track_dt -> nao conta para nenhuma unidade
curtl_orphan_test <- data.table(turbine = "T23", track_id = "zz", start = t0_test + 1800, end = t0_test + 1900)
orph_test <- check_offline_curtailment_overlap(offline_dt_test, curtl_orphan_test, idf_turbines_test, track_dt = track_dt_test)
stopifnot(!any(orph_test$has_curtailment))

stopifnot(identical(idf_unit_number(c("IDF13", "IDF-13", "GW19-13", "BSH68-01")), c(13L, 13L, 13L, 1L)))
cat("test_offline_curtailment_unit OK\n")

## curtailments por unidade (importancia relativa)
suppressPackageStartupMessages(source("R/idf_labels.R")); source("R/fatality_window_analysis.R")
cu_test <- data.table(turbine = "T23", track_id = c("a", "b", "c", "d", "z"), start = t0_test + c(100, 200, 300, 400, 500) , end = t0_test + c(110, 210, 310, 410, 510))
tk_test <- data.table(track_id = c("a", "b", "c", "d"), timestamp = t0_test + c(95, 195, 295, 395), idf = c(13L, 13L, 15L, 99L))
map_test <- c(IDF13 = "GW19-13", IDF15 = "GW23-15")
res_test <- summarise_curtailments_by_unit(cu_test, tk_test, "T23", t0_test, t0_test + 3600, c("GW19-13", "GW23-15", "GW26-70"), map_test)
stopifnot(identical(res_test$unit, c("GW19-13", "GW23-15", "GW26-70", "Other IDF units", "Unattributed")))
stopifnot(identical(res_test$n, c(2L, 1L, 0L, 1L, 1L)), identical(res_test$pct, c(40, 20, 0, 20, 20)))
## outra turbina / janela vazia -> zeros, sem Other/Unattributed
res0_test <- summarise_curtailments_by_unit(cu_test, tk_test, "T99", t0_test, t0_test + 3600, c("GW19-13"), map_test)
stopifnot(identical(res0_test$unit, "GW19-13"), res0_test$n == 0L, is.na(res0_test$pct))
cat("test curtailments por unidade OK\n")

## filtro por especie
cu_sp_test <- copy(cu_test)[, species := c("Egyptian-Vulture", "Egyptian-Vulture", "Steppe-Eagle", "Egyptian-Vulture", "Egyptian-Vulture")]
res_sp_test <- summarise_curtailments_by_unit(cu_sp_test, tk_test, "T23", t0_test, t0_test + 3600, c("GW19-13", "GW23-15"), map_test, species = "Egyptian-Vulture")
stopifnot(res_sp_test[unit == "GW19-13", n] == 2L, res_sp_test[unit == "GW23-15", n] == 0L, sum(res_sp_test$n) == 4L)  # o do GW23-15 era Steppe-Eagle
stopifnot(res_sp_test[unit == "GW19-13", pct] == 50)
cat("test especie OK\n")
