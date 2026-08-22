##
## Teste com dados simulados para R/monthly_report_utils.R
##
## Objetivo: confirmar que month_bounds() devolve o primeiro e o ultimo
## instante (23:59:59) de um mes de calendario, no fuso horario indicado,
## incluindo o caso Fevereiro (mes curto) e Dezembro (mudanca de ano); e que
## resolve_turbinas_scada() (a) devolve um vetor explicito tal e qual, sem
## verificar dados, e (b) para "all", devolve so' as turbinas com dados de
## SCADA DENTRO da janela pedida (nao em todo o historico do scada_dt_test).
##
## Nao usa testthat -- script normal com resultado calculado a mao.
##
## Correr: source("tests/test_monthly_report_utils.R")
##
## Depende de: data.table, lubridate
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


## resolve_turbinas_scada() -- vetor explicito passa tal e qual (BSH99 nem
## sequer existe no scada_dt_test, de proposito: confirma que a funcao NAO
## verifica dados quando ja recebe um vetor explicito)
scada_dt_test <- data.table::data.table(
  turbinelabel = c("BSH54", "BSH54", "BSH62", "BSH14"),
  datetime     = as.POSIXct(c(
    "2026-07-15 10:00:00", # BSH54, DENTRO da janela de teste
    "2026-06-20 10:00:00", # BSH54, FORA da janela (mes anterior)
    "2026-07-16 10:00:00", # BSH62, DENTRO
    "2026-06-25 10:00:00"  # BSH14, FORA (so' tem dados fora da janela)
  ), tz = test_tz)
)

window_ini_test <- as.POSIXct("2026-07-01 00:00:00", tz = test_tz)
window_end_test <- as.POSIXct("2026-07-31 23:59:59", tz = test_tz)

cat("\n===== resolve_turbinas_scada() -- vetor explicito =====\n")
result_explicit <- resolve_turbinas_scada(c("BSH99"), scada_dt_test, window_ini_test, window_end_test)
cat(sprintf(
  "Esperado: c(\"BSH99\") (devolvido tal e qual, sem verificar dados). Obtido: %s\n",
  paste(result_explicit, collapse = ", ")
))

cat("\n===== resolve_turbinas_scada() -- \"all\", resolvido contra a janela =====\n")
result_all <- resolve_turbinas_scada("all", scada_dt_test, window_ini_test, window_end_test)
cat(sprintf(
  "Esperado: c(\"BSH54\", \"BSH62\") -- so' as 2 turbinas com dados DENTRO de Julho/2026 (BSH14 so' tem dados em Junho, fica de fora). Obtido: %s\n",
  paste(result_all, collapse = ", ")
))
