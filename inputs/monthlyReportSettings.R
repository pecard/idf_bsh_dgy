##
## Monthly report settings
##
## Reusa TODA a configuracao de projeto (especies, ficheiros, thresholds de
## analise, etc.) de userSettings_BSH.R -- este ficheiro so' redefine o
## periodo (ini/end) para 1 mes de calendario, e os switches de que seccoes
## correm neste relatorio mensal (run_sections_monthly, abaixo). NAO
## redefinir aqui nenhum parametro ja definido em userSettings_BSH.R -- so
## ini/end/run_sections_monthly e os literais especificos do relatorio
## mensal, para evitar duplicar (e desalinhar) configuracao entre os 2
## ficheiros.
##
## Uso (em IDF_monthly_report.R):
##   report_month <- "2026-07"  # definir ANTES de dar source a este ficheiro
##   source(file.path(folder_input, "monthlyReportSettings.R"))
##

source(file.path(folder_input, "userSettings_BSH.R"))


##
## Periodo do relatorio -- 1 mes de calendario, sobrepoe ini/end definidos
## acima (em userSettings_BSH.R, que definem a janela do relatorio "geral")
##

if (!exists("report_month")) {
  stop("Definir report_month (formato \"YYYY-MM\", ex: \"2026-07\") antes de dar source a monthlyReportSettings.R")
}

source("R/monthly_report_utils.R")

report_month_bounds <- month_bounds(report_month, proj_timezone)
ini <- report_month_bounds$ini
end <- report_month_bounds$end


##
## Quais seccoes correm neste relatorio mensal
##
## Excluidas deste relatorio (decisao Paulo, 2026-08):
##   - Fatality investigation (secção 4 de IDF_analysis.R) -- analise de
##     incidente especifico, nao e' uma metrica mensal
##   - Coverage 3D/topografia (secção 5) -- idem
## Tambem excluidas por nao serem metricas periodicas/mensais (nao pedido
## pelo Paulo, mas fora de escopo por construcao):
##   - Curtailment removal risk (Kestrel) -- decisao de politica, corre 1 vez
##     sobre o historico completo (_unfilt), ver R/curtailment_removal_risk.R
##   - Turbine spatial/temporal clustering (secção 10) -- analise sobre o
##     historico completo, nao sobre 1 mes
##   - System performance vs. bird phenology (secção 8) -- serie temporal de
##     tendencia, precisa de varios meses para fazer sentido
##   - Turbine recent activity (secção 9) -- apoio ao protocolo de resposta a
##     outages, janela propria (recent_activity_days), nao o mes do relatorio
##
run_sections_monthly <- list(
  system_availability          = TRUE,  # 3.1 (heartbeats)
  observed_species             = TRUE,  # 3.2, parte 1 -- riqueza de especies observadas (track_dt)
  id_transitions                = TRUE,  # 3.2, parte 2 -- risco de transicao bi-direcional P<->NP + matriz de confusao
  curtailment_by_species        = TRUE,  # 3.3
  short_track_curtailment       = TRUE,  # 3.4
  curtailment_response_delays   = TRUE,  # 3.5-3.7 (resposta, shutdown time, safe distance) -- so corre se scada_dt tambem existir
  bio_flight_metrics             = TRUE,  # 6.1-6.2 (velocidade/altura de voo por especie)
  min_individuals                = TRUE   # 6.4
)

report_title_monthly <- sprintf("%s - Monthly Report (%s)", project_ref, report_month)
