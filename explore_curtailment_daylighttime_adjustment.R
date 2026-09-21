##
## Script de consola para testar a hipotese do Paulo (2026-09): com os dias
## a encurtar, o 1o/ultimo curtailment do dia estao a "encolher" para dentro
## de uma janela mais estreita do que o dia inteiro de luz -- o que
## suportaria reajustar o horario diario de curtailment das turbinas
## envolvidas em incidentes com abutres/aguias para uma janela fixa mais
## favoravel ao cliente (ex: 7h-18h), sem comprometer a seguranca.
##
## NAO faz parte do pipeline de producao (IDF_analysis.R/IDF_monthly_report.R
## nunca o chamam) -- so' imprime tabelas/graficos na consola/Viewer, nao
## escreve nada em outputs/. Mesmo padrao de explore_terrain_bearing_section.R.
##
## Pre-requisitos (correr isto DEPOIS de uma corrida normal de
## IDF_analysis.R, na mesma sessao, para o parque que quiseres analisar --
## BSH ou DGY) -- objetos ja' tem de existir:
##   curtl_dt, daylight_cal, fatality_incidents, proj_timezone
##   (CONFIRMAR o range de datas -- range(curtl_dt$start) -- antes de correr,
##   sobretudo se quiseres o historico completo em vez do periodo do
##   relatorio: usa curtl_dt_unfilt em vez de curtl_dt nesse caso)
##
## Correr: source("explore_curtailment_daylighttime_adjustment.R")
##

source("R/curtailment_daylighttime_adjustment.R")

## Janela de analise -- ultimos 6 meses de curtailments disponiveis (a
## partir do curtailment mais recente, nao de Sys.time(), para nao incluir
## um "buraco" se a cache nao tiver sido atualizada hoje). Ajustar
## window_months para reveres um periodo mais longo/curto.
window_months <- 6
window_end   <- max(curtl_dt$start)
## %m-% (nao so' "-"): "-" com um Period de meses pode devolver NA quando o
## dia-do-mes de window_end nao existe no mes alvo (ex: 31 ago - 6 meses =
## "28/29 fev" nao "31 fev") -- lubridate::"-.Period" nao faz clamping,
## so' %m-%/%m+% fazem (mesmo cuidado ja' documentado/usado em
## R/monthly_report_utils.R, month_bounds()).
window_start <- window_end %m-% months(window_months)

## Janela fixa proposta a testar -- ajustar livremente para experimentar
## outras horas (ex: "06:30"/"18:30") antes de decidir a recomendacao final.
proposed_start_clock <- "07:00"
proposed_end_clock   <- "18:00"

## Janela so' para os GRAFICOS (contexto mais longo, ex: 12 meses) -- as
## tabelas de decisao (coverage_dt/trend_dt/violations_dt, abaixo) ficam
## sempre restritas a window_start/window_end (os "ultimos 6 meses" que
## interessam a recomendacao); plot_start e' so' visual, para dar ao Paulo
## o mesmo tipo de contexto "historico + periodo revisto marcado" do
## grafico de referencia (Nota Tecnica Brasil -- dados PACAAL + periodo da
## Nota Tecnica, com 1 linha branca a separar os 2). window_marker_date
## desenha essa linha branca em window_start.
plot_context_months <- 12
plot_start <- window_end %m-% months(plot_context_months)

cat(sprintf(
  "\n===== Janela de decisao (recomendacao): %s a %s (%d meses) =====\n",
  format(window_start, "%Y-%m-%d"), format(window_end, "%Y-%m-%d"), window_months
))
cat(sprintf(
  "===== Janela do grafico (contexto): %s a %s (%d meses) =====\n",
  format(plot_start, "%Y-%m-%d"), format(window_end, "%Y-%m-%d"), plot_context_months
))

twilight_cal <- build_twilight_calendar(plot_start, window_end, proj_lat, proj_lon, proj_timezone)


## 1. Farm-wide (todas as turbinas) -- padrao geral de atividade das aves ---

## Tabela de decisao -- so' os "ultimos 6 meses" (window_start..window_end)
daily_dt <- daily_curtailment_bounds(curtl_dt, window_start, window_end, tz = proj_timezone)
daily_dt <- join_curtailment_bounds_daylight(daily_dt, daylight_cal)

cat("\n===== Farm-wide: resumo diario (amostra) =====\n")
print(head(daily_dt[, .(date, first_curtailment_start, last_curtailment_end, gap_sunrise_min, gap_sunset_min)], 10))

## Grafico -- contexto mais longo (plot_start..window_end), com marcador
## branco em window_start
daily_dt_plot <- daily_curtailment_bounds(curtl_dt, plot_start, window_end, tz = proj_timezone)
daily_dt_plot <- join_curtailment_bounds_daylight(daily_dt_plot, daylight_cal, twilight_cal)

p_farmwide <- plot_daily_curtailment_bounds(
  daily_dt_plot, proposed_start_clock, proposed_end_clock,
  window_marker_date = window_start, date_breaks = "3 weeks"
)
print(p_farmwide)

coverage_dt <- summarise_curtailment_window_coverage(daily_dt, proposed_start_clock, proposed_end_clock, window_start, window_end, tz = proj_timezone)
cat("\n===== Farm-wide: cobertura da janela proposta =====\n")
print(coverage_dt)

trend_dt <- test_curtailment_gap_trend(daily_dt)
cat("\n===== Farm-wide: tendencia (gap vs. duracao do dia) =====\n")
print(trend_dt)

violations_dt <- list_curtailment_window_violations(daily_dt, proposed_start_clock, proposed_end_clock)
cat(sprintf("\n===== Farm-wide: %d dias com curtailment fora da janela proposta =====\n", nrow(violations_dt)))
print(violations_dt)


## 2. So' as turbinas de incidentes com abutres/aguias -- o alvo real da
## recomendacao (fatality_incidents, userSettings_BSH.R/userSettings_DGY.R) --

critical_turbines <- unique(fatality_incidents$turbine)
cat(sprintf("\n===== Turbinas de incidentes (abutres/aguias): %s =====\n", paste(critical_turbines, collapse = ", ")))

## Tabela de decisao -- so' os "ultimos 6 meses"
daily_dt_critical <- daily_curtailment_bounds(curtl_dt, window_start, window_end, tz = proj_timezone, turbines = critical_turbines)
daily_dt_critical <- join_curtailment_bounds_daylight(daily_dt_critical, daylight_cal)

## Grafico -- contexto mais longo, mesmo marcador de window_start
daily_dt_critical_plot <- daily_curtailment_bounds(curtl_dt, plot_start, window_end, tz = proj_timezone, turbines = critical_turbines)
daily_dt_critical_plot <- join_curtailment_bounds_daylight(daily_dt_critical_plot, daylight_cal, twilight_cal)

p_critical <- plot_daily_curtailment_bounds(
  daily_dt_critical_plot, proposed_start_clock, proposed_end_clock,
  window_marker_date = window_start, date_breaks = "3 weeks"
)
print(p_critical)

coverage_dt_critical <- summarise_curtailment_window_coverage(daily_dt_critical, proposed_start_clock, proposed_end_clock, window_start, window_end, tz = proj_timezone)
cat("\n===== Turbinas criticas: cobertura da janela proposta =====\n")
print(coverage_dt_critical)

trend_dt_critical <- test_curtailment_gap_trend(daily_dt_critical)
cat("\n===== Turbinas criticas: tendencia (gap vs. duracao do dia) =====\n")
print(trend_dt_critical)

violations_dt_critical <- list_curtailment_window_violations(daily_dt_critical, proposed_start_clock, proposed_end_clock)
cat(sprintf("\n===== Turbinas criticas: %d dias com curtailment fora da janela proposta =====\n", nrow(violations_dt_critical)))
print(violations_dt_critical)

## ATENCAO: coverage_dt_critical$n_days_violation_start/end > 0 significa
## que, no periodo revisto, a janela proposta TERIA deixado bird activity
## real (nestas turbinas especificas, ligadas a incidentes) sem curtailment
## -- nesse caso a janela proposta NAO e' segura como esta', mesmo que a
## media/tendencia farm-wide pareca favoravel. Ver violations_dt_critical
## para os dias/casos concretos antes de qualquer recomendacao ao cliente.

# ggsave("bird_daylighttime_farmwide.png", p_farmwide, width = 9, height = 4.5, dpi = 300, bg = "white")
# ggsave("bird_daylighttime_critical.png", p_critical, width = 9, height = 4.5, dpi = 300, bg = "white")
