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
