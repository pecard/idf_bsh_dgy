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


## Resolve `turbinas_scada` (monthlyReportSettings_BSH.R/_DGY.R) para o vetor real de
## turbinas a usar nas seccoes de resposta/shutdown-time/safe-distance --
## "all" e' recalculado a cada corrida a partir do scada_dt recebido (nao
## uma lista fixa), para acompanhar automaticamente se mais turbinas
## passarem a ter SCADA instalado; um vetor explicito (ex:
## c('BSH54','BSH62','BSH14')) e' devolvido tal e qual, sem verificar se
## essas turbinas tem mesmo dados no periodo (mesmo comportamento de
## sempre, antes de existir a opcao "all").
##
## CORRECAO (2026-08, caso real): a 1ª versao filtrava "all" so' pelo MES do
## relatorio (scada_ini_monthly/scada_end_monthly) -- isso excluiu BSH14 num
## mes em que essa turbina nao teve NENHUMA leitura SCADA (buraco temporario
## de 1 mes), apesar de ter SCADA instalado e dados noutros meses. Passar
## aqui scada_dt SEM filtrar por mes (ex: scada_dt_unfilt, ou o scada_dt do
## relatorio mensal -- que ja NAO e' filtrado por ini/end, ver "0. Filter
## data" em IDF_monthly_report.R) -- "all" passa a significar "teve SCADA
## alguma vez", nao "teve SCADA NESTE mes especifico". Uma turbina com
## SCADA instalado mas sem leituras neste mes continua na lista -- so'
## contribui 0 curtailments SCADA-validados nesse mes, nao e' descartada
## por um buraco temporario.
resolve_turbinas_scada <- function(turbinas_scada, scada_dt) {

  if (!identical(turbinas_scada, "all")) return(turbinas_scada)

  sort(unique(scada_dt$turbinelabel))
}
