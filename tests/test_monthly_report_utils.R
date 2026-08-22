##
## Teste com dados simulados para R/monthly_report_utils.R
##
## Objetivo: confirmar que month_bounds() devolve o primeiro e o ultimo
## instante (23:59:59) de um mes de calendario, no fuso horario indicado,
## incluindo o caso Fevereiro (mes curto) e Dezembro (mudanca de ano).
##
## Nao usa testthat -- script normal com resultado calculado a mao.
##
## Correr: source("tests/test_monthly_report_utils.R")
##
## Depende de: lubridate
##

source("R/monthly_report_utils.R")

test_tz <- "Asia/Samarkand"

cat("\n===== month_bounds() -- mes normal (Julho, 31 dias) =====\n")
bounds_jul <- month_bounds("2026-07", test_tz)
print(bounds_jul)
cat(sprintf(
  "Esperado: ini = 2026-07-01 00:00:00, end = 2026-07-31 23:59:59. Obtido: ini = %s, end = %s\n",
  format(bounds_jul$ini), format(bounds_jul$end)
))

cat("\n===== month_bounds() -- Fevereiro nao-bissexto (28 dias) =====\n")
bounds_feb <- month_bounds("2026-02", test_tz)
print(bounds_feb)
cat(sprintf(
  "Esperado: ini = 2026-02-01 00:00:00, end = 2026-02-28 23:59:59. Obtido: ini = %s, end = %s\n",
  format(bounds_feb$ini), format(bounds_feb$end)
))

cat("\n===== month_bounds() -- Dezembro (mudanca de ano) =====\n")
bounds_dec <- month_bounds("2025-12", test_tz)
print(bounds_dec)
cat(sprintf(
  "Esperado: ini = 2025-12-01 00:00:00, end = 2025-12-31 23:59:59. Obtido: ini = %s, end = %s\n",
  format(bounds_dec$ini), format(bounds_dec$end)
))

cat("\n===== month_bounds() -- formato invalido =====\n")
result_invalid <- tryCatch(
  month_bounds("not-a-month", test_tz),
  error = function(e) conditionMessage(e)
)
cat(sprintf("Esperado: erro explicito. Obtido: %s\n", result_invalid))
