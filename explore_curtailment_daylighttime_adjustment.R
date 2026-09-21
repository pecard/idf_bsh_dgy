##
## Script de consola para testar a hipotese do Paulo (2026-09): com os dias
## a encurtar, o 1o/ultimo curtailment do dia estao a "encolher" para dentro
## de uma janela mais estreita do que o dia inteiro de luz -- o que
## suportaria reajustar o horario diario de curtailment das turbinas
## envolvidas em incidentes com abutres/aguias para uma janela fixa mais
## favoravel ao cliente (ex: 7h-18h), sem comprometer a seguranca.
##
## FARM-WIDE APENAS -- decisao do Paulo (2026-09): as turbinas de
## fatality_incidents (as que a recomendacao vai mudar) JA' tem curtailment
## implementado, possivelmente sob um regime mais conservador por causa do
## proprio incidente -- o padrao historico de curtailment NESSAS turbinas
## reflete esse regime atual, nao a atividade "natural" das aves, e usa-lo
## para justificar mudar o proprio regime que o produziu seria circular.
## Sao tambem turbinas escolhidas PRECISAMENTE por terem tido um incidente
## (viés de selecao -- podem ter um fator local proprio, ex: ninho perto,
## corredor de voo, nao so' o padrao sazonal geral) e uma amostra muito mais
## pequena/ruidosa do que o parque inteiro (problematico sobretudo para a
## abordagem por percentil da secção 2, que precisa de min_n curtailments
## por periodo). A evidencia usada para a recomendacao e' sempre FARM-WIDE
## (caracteriza o padrao geral, mais robusto); as turbinas criticas sao so'
## o ALVO de aplicacao dessa recomendacao, nao a fonte da evidencia -- por
## isso aqui aparecem so' listadas (contexto), sem nenhuma analise restrita
## a elas. daily_curtailment_bounds()/curtailment_edge_bins_by_period()
## (R/curtailment_daylighttime_adjustment.R) continuam a aceitar
## turbines = <vector> se algum dia precisares de um subconjunto por outra
## razao -- so' nao e' usado aqui para esta recomendacao especifica.
##
## NAO faz parte do pipeline de producao (IDF_analysis.R/IDF_monthly_report.R
## nunca o chamam) -- so' imprime tabelas/graficos na consola/Viewer, nao
## escreve nada em outputs/. Mesmo padrao de explore_terrain_bearing_section.R.
##
## NAO precisa de uma corrida completa de IDF_analysis.R primeiro -- carrega
## so' os curtailments (+ calendario de luz do dia) atraves de
## load_curtailments.R (script dedicado, le a cache/os brutos, NAO altera
## nenhum settings existente). Forma mais simples de correr: dar Source a
## run_curtailment_daylighttime_adjustment_BSH.R (ou _DGY.R) -- ja' trata
## de tudo (project_settings_file, force_reread_cache, e as opcoes abaixo)
## num so' passo. Equivalente manual (sessao R nova, a partir da raiz do
## projeto):
##
##   project_settings_file <- "userSettings_BSH.R"  # ou "userSettings_DGY.R"
##   force_reread_cache <- TRUE
##   source("load_curtailments.R")
##   source("explore_curtailment_daylighttime_adjustment.R")
##

source("R/curtailment_daylighttime_adjustment.R")

## plot_daily_curtailment_bounds()/plot_curtailment_edge_trend() (sourced
## acima) usam ggplot()/aes()/geom_*() sem prefixo -- precisam do pacote
## anexado via library(), nao so' instalado. load_curtailments.R so' carrega
## o minimo para ler/cachear curtailments (nao inclui ggplot2, de proposito
## -- e' so' um loader de dados) -- por isso o grafico, especificamente,
## precisa do pacote aqui.
if (!require("ggplot2", character.only = TRUE)) install.packages("ggplot2")
library(ggplot2)

if (!exists("curtl_dt_unfilt")) {
  stop("curtl_dt_unfilt nao existe -- correr primeiro: project_settings_file <- \"userSettings_BSH.R\" (ou _DGY.R); source(\"load_curtailments.R\").")
}

## Todas as opcoes abaixo seguem o mesmo padrao de force_reread_cache/
## generate_report (IDF_analysis.R): so' definem um valor por omissao SE
## ainda nao existir -- define-as ANTES de source() (ex: num script
## lancador, ver run_curtailment_daylighttime_adjustment_BSH.R/_DGY.R) para
## as sobrepor sem editar este ficheiro.

## Janela de analise -- ultimos N meses de curtailments disponiveis (a
## partir do curtailment mais recente, nao de Sys.time(), para nao incluir
## um "buraco" se a cache nao tiver sido atualizada hoje).
if (!exists("window_months")) window_months <- 6
window_end <- max(curtl_dt_unfilt$start)
## %m-% (nao so' "-"): "-" com um Period de meses pode devolver NA quando o
## dia-do-mes de window_end nao existe no mes alvo (ex: 31 ago - 6 meses =
## "28/29 fev" nao "31 fev") -- lubridate::"-.Period" nao faz clamping,
## so' %m-%/%m+% fazem (mesmo cuidado ja' documentado/usado em
## R/monthly_report_utils.R, month_bounds()).
window_start <- window_end %m-% months(window_months)

## Janela fixa proposta a testar -- ajustar livremente para experimentar
## outras horas (ex: "06:30"/"18:30") antes de decidir a recomendacao final.
if (!exists("proposed_start_clock")) proposed_start_clock <- "07:00"
if (!exists("proposed_end_clock"))   proposed_end_clock   <- "18:00"

## Janela so' para os GRAFICOS (contexto mais longo, ex: 12 meses) -- as
## tabelas de decisao (coverage_dt/trend_dt/violations_dt, abaixo) ficam
## sempre restritas a window_start/window_end (os "ultimos N meses" que
## interessam a recomendacao); plot_start e' so' visual, para dar ao Paulo
## o mesmo tipo de contexto "historico + periodo revisto marcado" do
## grafico de referencia (Nota Tecnica Brasil -- dados PACAAL + periodo da
## Nota Tecnica, com 1 linha branca a separar os 2). window_marker_date
## desenha essa linha branca em window_start.
if (!exists("plot_context_months")) plot_context_months <- 12
plot_start <- window_end %m-% months(plot_context_months)

## Bordos robustos (secção 2) -- periodo de agregacao, bin de arredondamento
## e percentil usados por curtailment_edge_bins_by_period() (funcao 6,
## R/curtailment_daylighttime_adjustment.R).
if (!exists("edge_period"))   edge_period   <- "month" # ou "week"
if (!exists("edge_bin_mins")) edge_bin_mins <- 10
if (!exists("edge_pct"))      edge_pct      <- 0.01
if (!exists("edge_min_n"))    edge_min_n    <- 20

cat(sprintf(
  "\n===== Janela de decisao (recomendacao): %s a %s (%d meses) =====\n",
  format(window_start, "%Y-%m-%d"), format(window_end, "%Y-%m-%d"), window_months
))
cat(sprintf(
  "===== Janela do grafico (contexto): %s a %s (%d meses) =====\n",
  format(plot_start, "%Y-%m-%d"), format(window_end, "%Y-%m-%d"), plot_context_months
))

## So' para contexto/referencia -- NAO usado para filtrar nenhuma analise
## abaixo (ver nota no topo do ficheiro sobre porque a evidencia e' sempre
## farm-wide).
critical_turbines <- unique(fatality_incidents$turbine)
cat(sprintf("\n===== Turbinas-alvo da recomendacao (contexto, nao filtradas abaixo): %s =====\n", paste(critical_turbines, collapse = ", ")))

twilight_cal <- build_twilight_calendar(plot_start, window_end, proj_lat, proj_lon, proj_timezone)


## 1. Bordos diarios (min/max literal) -- padrao geral, farm-wide ----

## Tabela de decisao -- so' os "ultimos window_months meses" (window_start..window_end)
daily_dt <- daily_curtailment_bounds(curtl_dt_unfilt, window_start, window_end, tz = proj_timezone)
daily_dt <- join_curtailment_bounds_daylight(daily_dt, daylight_cal)

cat("\n===== Resumo diario (amostra) =====\n")
print(head(daily_dt[, .(date, first_curtailment_start, last_curtailment_end, gap_sunrise_min, gap_sunset_min)], 10))

## Grafico -- contexto mais longo (plot_start..window_end), com marcador
## branco em window_start
daily_dt_plot <- daily_curtailment_bounds(curtl_dt_unfilt, plot_start, window_end, tz = proj_timezone)
daily_dt_plot <- join_curtailment_bounds_daylight(daily_dt_plot, daylight_cal, twilight_cal)

p_daily <- plot_daily_curtailment_bounds(
  daily_dt_plot, proposed_start_clock, proposed_end_clock,
  window_marker_date = window_start, date_breaks = "3 weeks"
)
print(p_daily)

coverage_dt <- summarise_curtailment_window_coverage(daily_dt, proposed_start_clock, proposed_end_clock, window_start, window_end, tz = proj_timezone)
cat("\n===== Cobertura da janela proposta (bordo diario, min/max) =====\n")
print(coverage_dt)

trend_dt <- test_curtailment_gap_trend(daily_dt)
cat("\n===== Tendencia (gap vs. duracao do dia) -- bordo diario =====\n")
print(trend_dt)

violations_dt <- list_curtailment_window_violations(daily_dt, proposed_start_clock, proposed_end_clock)
cat(sprintf("\n===== %d dias com curtailment fora da janela proposta (bordo diario) =====\n", nrow(violations_dt)))
print(violations_dt)


## 2. Bordos robustos (percentil 1%/99%, bins de 10 min, por periodo) --
## complementa a secção 1: o min/max diario e' sensivel a 1 unico outlier;
## agrupando por periodo (mes por omissao -- mais curtailments por periodo
## do que semana, percentil mais estavel) e olhando ao percentil em vez do
## literal min/max, ate' 1% dos curtailments desse periodo pode ser um caso
## atipico sem deslocar a estimativa. CONFIRMAR sempre n_curtailments por
## periodo (min_n = 20 por omissao -- periodos com menos ficam NA) ----

edge_bins_dt <- curtailment_edge_bins_by_period(
  curtl_dt_unfilt, daylight_cal, window_start, window_end, tz = proj_timezone,
  period = edge_period, bin_mins = edge_bin_mins, edge_pct = edge_pct, min_n = edge_min_n
)
cat("\n===== Bordos robustos por mes (percentil 1%/99%, bin de 10 min) =====\n")
print(edge_bins_dt)

## Grafico -- contexto mais longo (plot_start..window_end), mesmo periodo
edge_bins_dt_plot <- curtailment_edge_bins_by_period(
  curtl_dt_unfilt, daylight_cal, plot_start, window_end, tz = proj_timezone,
  period = edge_period, bin_mins = edge_bin_mins, edge_pct = edge_pct, min_n = edge_min_n
)
p_edge <- plot_curtailment_edge_trend(
  edge_bins_dt_plot, daylight_cal, twilight_cal,
  proposed_start_clock, proposed_end_clock,
  window_marker_date = window_start
)
print(p_edge)

coverage_edge_dt <- summarise_curtailment_edge_coverage(edge_bins_dt, proposed_start_clock, proposed_end_clock)
cat("\n===== Cobertura da janela proposta (bordo robusto, por mes) =====\n")
print(coverage_edge_dt)

trend_edge_dt <- test_curtailment_gap_trend(edge_bins_dt)
cat("\n===== Tendencia (gap vs. duracao do dia) -- bordo robusto =====\n")
print(trend_edge_dt)

## ATENCAO: comparar coverage_dt (secção 1, bordo diario) com
## coverage_edge_dt (secção 2, bordo robusto) e' o proprio argumento de
## robustez -- o bordo diario e' mais conservador (qualquer outlier conta),
## o robusto tolera ate' 1% de casos atipicos por periodo. Se AINDA ASSIM
## houver violacao no bordo robusto (n_periods_violation_start/end > 0),
## e' um sinal mais forte de que a janela proposta nao e' segura como esta'
## -- nao decidir so' com base num dos dois.

# ggsave("bird_daylighttime_daily.png", p_daily, width = 9, height = 4.5, dpi = 300, bg = "white")
# ggsave("bird_daylighttime_edge_trend.png", p_edge, width = 9, height = 4.5, dpi = 300, bg = "white")
