##
## Teste com dados simulados para R/monthly_report_utils.R
##
## Objetivo: confirmar que month_bounds() devolve o primeiro e o ultimo
## instante (23:59:59) de um mes de calendario, no fuso horario indicado,
## incluindo o caso Fevereiro (mes curto) e Dezembro (mudanca de ano); e que
## resolve_turbinas_scada() (a) devolve um vetor explicito tal e qual, sem
## verificar dados, e (b) para "all", devolve TODAS as turbinas com
## QUALQUER leitura SCADA no scada_dt recebido, mesmo que so' num mes
## diferente do mes do relatorio -- corrigido 2026-08 depois de um caso
## real em que filtrar "all" so' pelo mes excluiu uma turbina com SCADA
## instalado mas sem leituras nesse mes especifico (buraco temporario).
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
## verifica dados quando ja recebe um vetor explicito). BSH14 so' tem
## leitura em Junho (mes DIFERENTE do "mes do relatorio", Julho, nos
## restantes casos deste teste) -- de proposito, para confirmar que "all"
## ainda a inclui (nao filtra por mes, ver correcao acima).
scada_dt_test <- data.table::data.table(
  turbinelabel = c("BSH54", "BSH54", "BSH62", "BSH14"),
  datetime     = as.POSIXct(c(
    "2026-07-15 10:00:00", # BSH54, no mes do relatorio (Julho)
    "2026-06-20 10:00:00", # BSH54, tambem tem leitura em Junho
    "2026-07-16 10:00:00", # BSH62, no mes do relatorio
    "2026-06-25 10:00:00"  # BSH14, SO' tem leitura em Junho -- nao em Julho
  ), tz = test_tz)
)

cat("\n===== resolve_turbinas_scada() -- vetor explicito =====\n")
result_explicit <- resolve_turbinas_scada(c("BSH99"), scada_dt_test)
cat(sprintf(
  "Esperado: c(\"BSH99\") (devolvido tal e qual, sem verificar dados). Obtido: %s\n",
  paste(result_explicit, collapse = ", ")
))

cat("\n===== resolve_turbinas_scada() -- \"all\", TODAS as turbinas com dados (nao so' o mes do relatorio) =====\n")
result_all <- resolve_turbinas_scada("all", scada_dt_test)
cat(sprintf(
  "Esperado: c(\"BSH14\", \"BSH54\", \"BSH62\") -- as 3 turbinas, incluindo BSH14 (so' tem leitura em Junho, nao em Julho, mas \"all\" nao filtra por mes). Obtido: %s\n",
  paste(result_all, collapse = ", ")
))
