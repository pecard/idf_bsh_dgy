##
## Utilitario de apoio ao relatorio mensal -- calcula a janela [ini, end] de
## um mes de calendario, no fuso horario do projeto, para reutilizar o mesmo
## mecanismo de filtro por data (ini/end) que IDF_analysis.R ja usa para a
## sua propria janela de relatorio (ver "0. Filter data").
##
## Depende de: lubridate
##
## Uso:
##   bounds <- month_bounds("2026-07", "Asia/Samarkand")
##   ini <- bounds$ini
##   end <- bounds$end
##

## report_month: string "YYYY-MM" (ex: "2026-07"). Devolve list(ini, end) em
## POSIXct no fuso horario tz -- ini e' o primeiro instante do mes, end e' o
## ultimo instante (23:59:59) do mesmo mes (mesma granularidade usada em
## userSettings_BSH.R para ini/end).
month_bounds <- function(report_month, tz) {

  first_day <- as.POSIXct(paste0(report_month, "-01 00:00:00"), tz = tz)
  if (is.na(first_day)) {
    stop(sprintf("report_month invalido: '%s' -- usar formato \"YYYY-MM\".", report_month))
  }

  # first_day esta sempre no dia 1 -- %m+% months(1) avanca exatamente 1 mes
  # de calendario sem ambiguidade de clamping (essa so' afeta dias > 28), e
  # evita a semantica de fronteira pouco clara de lubridate::ceiling_date()
  # quando o valor de entrada ja esta exatamente no limite do mes.
  next_month <- first_day %m+% months(1)

  list(
    ini = first_day,
    end = next_month - 1
  )
}
