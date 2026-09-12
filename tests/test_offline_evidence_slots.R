##
## Teste com dados simulados para as funcoes 9b-9c de R/availability_daylight.R
## (offline_evidence_slot_grid(), plot_offline_evidence_slots()) -- o novo
## calendario categorico (Night/Online/3 categorias de evidencia) que
## substitui plot_availability_calendar() (gradiente continuo de %) no
## corpo do relatorio, pedido do Paulo, 2026-09.
##
## daylight_cal_test e' construido a mao (nao via build_daylight_calendar(),
## que depende do package suncalc) -- 1 dia, sunrise 06:00, sunset 20:00,
## tz "UTC" so' para o teste (sem geografia real envolvida).
##
## Nao usa testthat -- script normal com dados sinteticos e resultado
## calculado a mao.
##
## Correr: source("tests/test_offline_evidence_slots.R")
##
## Depende de: data.table, lubridate, ggplot2
##

source("R/availability_daylight.R")

daylight_cal_test <- data.table::data.table(
  date         = as.Date("2026-06-01"),
  sunrise      = as.POSIXct("2026-06-01 06:00:00", tz = "UTC"),
  sunset       = as.POSIXct("2026-06-01 20:00:00", tz = "UTC"),
  daylight_mins = 840
)

## IDF-1: 1 intervalo offline 10:00-11:00, classificado "IDF unit
## communication failure" -- deve cobrir exatamente os slots de 30min cujo
## PONTO MEDIO cai dentro de [10:00, 11:00] (10:00-10:30 e 10:30-11:00; o
## slot 11:00-11:30 tem ponto medio 11:15, FORA do intervalo, fica
## "Online"). IDF-2: sem nenhum intervalo offline classificado -- todos os
## slots diurnos devem ficar "Online" (nunca NA), noturnos "Night".
offline_evidence_dt_test <- data.table::data.table(
  idf        = "IDF-1",
  off_start  = as.POSIXct("2026-06-01 10:00:00", tz = "UTC"),
  off_end    = as.POSIXct("2026-06-01 11:00:00", tz = "UTC"),
  classification = "IDF unit communication failure"
)

slot_grid_test <- offline_evidence_slot_grid(
  daylight_cal_test, tz = "UTC",
  start_date = "2026-06-01", end_date = "2026-06-01",
  offline_evidence_dt = offline_evidence_dt_test,
  idf_sel = c("IDF-1", "IDF-2"), slot_mins = 30
)

cat(sprintf(
  "\n===== offline_evidence_slot_grid(): %d linha(s) (esperado 48 slots x 2 unidades = 96) =====\n",
  nrow(slot_grid_test)
))

## IDF-1
comm_failure_slots <- slot_grid_test[idf == "IDF-1" & slot_status == "IDF unit communication failure"]
cat(sprintf(
  "IDF-1, slots 'IDF unit communication failure': %d (esperado 2, as 10:00 e 10:30) -- obtido: %s\n",
  nrow(comm_failure_slots), paste(format(comm_failure_slots$slot, "%H:%M"), collapse = ", ")
))

night_slots_idf1   <- slot_grid_test[idf == "IDF-1" & slot_status == "Night"]
online_slots_idf1  <- slot_grid_test[idf == "IDF-1" & slot_status == "Online"]
# 48 slots/dia; noite = ponto medio < sunrise(06:00) ou >= sunset(20:00) --
# 12 slots (00:00-05:30) + 8 slots (20:00-23:30) = 20; dia = 48-20 = 28,
# dos quais 2 sao "IDF unit communication failure" -> 26 "Online"
cat(sprintf(
  "IDF-1, slots 'Night': %d (esperado 20), 'Online': %d (esperado 26)\n",
  nrow(night_slots_idf1), nrow(online_slots_idf1)
))

## IDF-2 -- sem nenhum intervalo offline, nunca deve ficar com
## classification NA por herdar (todos os slots diurnos = "Online")
n_na_idf2 <- slot_grid_test[idf == "IDF-2" & is.na(slot_status), .N]
comm_failure_idf2 <- slot_grid_test[idf == "IDF-2" & slot_status == "IDF unit communication failure", .N]
cat(sprintf(
  "IDF-2 (sem evidencia offline nenhuma): %d slot(s) NA (esperado 0), %d slot(s) 'IDF unit communication failure' indevidamente herdado de IDF-1 (esperado 0)\n",
  n_na_idf2, comm_failure_idf2
))

## Fronteira sunrise/sunset -- mesmo criterio >= sunrise & < sunset de
## heartbeat_slot_grid() (funcao 8)
boundary_dawn  <- slot_grid_test[idf == "IDF-1" & time_decimal == 5.5, slot_status]  # 05:30, ponto medio 05:45 -> Night
boundary_day   <- slot_grid_test[idf == "IDF-1" & time_decimal == 6.0, slot_status]  # 06:00, ponto medio 06:15 -> Online
cat(sprintf(
  "Fronteira do amanhecer: slot 05:30 = '%s' (esperado Night), slot 06:00 = '%s' (esperado Online)\n",
  boundary_dawn, boundary_day
))

p_offline_evidence <- plot_offline_evidence_slots(slot_grid_test, slot_mins = 30)
cat(sprintf(
  "\n===== plot_offline_evidence_slots() =====\nObjeto ggplot criado: %s\n",
  class(p_offline_evidence)[1]
))
