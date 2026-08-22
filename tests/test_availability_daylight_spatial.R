##
## Teste com dados simulados para as funcoes 10-11 de R/availability_daylight.R
## (join_availability_to_turbine(), plot_availability_spatial())
##
## Objetivo: confirmar o cruzamento disponibilidade-por-unidade-IDF <->
## localizacao-de-turbina via a matriz manual (Primary IDF), incluindo os 2
## casos de "no data" que NAO podem ser descartados em silencio:
##   - turbina sem unidade IDF primaria na matriz manual (TC, abaixo)
##   - turbina cuja unidade primaria nao tem heartbeats neste periodo,
##     logo nao aparece em by_idf_summary (TD, abaixo)
##
## Nao usa testthat -- script normal com dados sinteticos e resultado
## calculado a mao.
##
## Correr: source("tests/test_availability_daylight_spatial.R")
##
## Depende de: data.table, sf, ggplot2
##

source("R/availability_daylight.R")

## 4 turbinas: TA e TB tem unidade primaria com dados; TC nao tem unidade
## primaria na matriz manual; TD tem unidade primaria (IDF-TD) mas essa
## unidade nao enviou heartbeats este mes (nao esta em by_idf_summary_test)
wtg_test <- sf::st_as_sf(
  data.table(
    InternalNa = c("TA", "TB", "TC", "TD"),
    x = c(0, 1000, 2000, 3000),
    y = c(0, 0, 0, 0)
  ),
  coords = c("x", "y")
)

turbine_idf_manual_dt_test <- data.table(
  `Turbine ID` = c("TA", "TB", "TD"), # TC de proposito sem linha aqui
  `Primary IDF` = c("IDF-TA", "IDF-TB", "IDF-TD")
)

by_idf_summary_test <- data.table(
  idf = c("IDF-TA", "IDF-TB"), # IDF-TD de proposito ausente (sem heartbeats)
  offline_mins_total = c(60, 600),
  daylight_mins_total = c(1000, 1000),
  monitoring_period_pct = c(6.0, 60.0),
  availability_pct = c(94.0, 40.0)
)

turbine_avail_dt <- join_availability_to_turbine(
  by_idf_summary_test, wtg_test, turbine_idf_manual_dt_test, wtg_id_col = "InternalNa"
)

cat("\n===== join_availability_to_turbine() =====\n")
print(turbine_avail_dt[, .(turbine, primary_idf, x, y, monitoring_period_pct)])
cat(paste(
  "\nEsperado: TA monitoring_period_pct=6.0, TB=60.0, TC primary_idf=NA",
  "(sem linha na matriz manual), TD primary_idf='IDF-TD' mas",
  "monitoring_period_pct=NA (unidade sem heartbeats este mes) -- 1 aviso",
  "acima (\"sem disponibilidade calculavel\") deve contar 2 turbinas",
  "(TC + TD).\n"
))

p_spatial <- plot_availability_spatial(turbine_avail_dt)
cat("\n===== plot_availability_spatial() =====\n")
cat(sprintf(
  "Objeto ggplot criado: %s. Esperado: 2 pontos coloridos/dimensionados (TA, TB) + 2 marcadores 'x' cinzentos (TC, TD).\n",
  class(p_spatial)[1]
))
