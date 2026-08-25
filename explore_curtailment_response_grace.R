##
## Script de exploracao/afinacao dos limiares de missed/delayed -- NAO faz
## parte do pipeline de producao (IDF_analysis.R nunca o chama, e ele
## proprio nao corre o relatorio nem escreve nada em outputs/).
##
## Motivacao (Paulo, 2026-08): nos plots de exemplo da secção "Example
## Response Profiles" (report_template.rmd), varios curtailments
## classificados "missed" mostram o RPM a continuar a descer claramente
## DEPOIS do "stop" (fim da propria ordem de curtailment), chegando perto
## de 0 uns 40-90s mais tarde -- ou seja, a turbina respondeu, so' que mais
## devagar do que a duracao da ordem de curtailment permitiu verificar
## (classify_response_flag() so' olha para o RPM exatamente no timestamp
## de "end"). Este script deixa testar a hipotese de dar mais tempo (uma
## "grace window" depois do "end") antes de decidir que um curtailment foi
## mesmo "missed" -- ver R/curtailment_response_grace.R,
## classify_response_flag_grace(). Testa tambem a mesma ideia diretamente em
## time_to_rpm_thresholds() (R/curtailment_shutdown_time.R,
## compare_shutdown_time_grace()) -- essa funcao alimenta tambem a secção
## "Shutdown Time" do relatorio (nao so' a classificacao missed/delayed),
## pelo mesmo motivo: so' procura leituras de RPM dentro da duracao da
## propria ordem de curtailment.
##
## Reutiliza a MESMA cache (fst) ja' gravada por uma corrida anterior de
## run_annual_analysis.R (ou run_annual_analysis_DGY.R) -- NAO rele os
## ficheiros brutos, so' os 2 datasets grandes precisos aqui
## (curtl_dt_unfilt, scada_dt_unfilt). Se ainda nao correste o pipeline
## pelo menos uma vez para o parque que queres explorar, corre isso
## primeiro (so' precisas da cache, nao precisas de esperar o relatorio
## docx completo).
##
## Uso:
##   1) Ajustar project_settings_file abaixo para o parque a explorar.
##   2) Dar Source A ESTE FICHEIRO (Ctrl+Shift+S no RStudio, ou
##      source("explore_curtailment_response_grace.R") na consola).
##   3) Olhar para grace_comparison_dt (quantos "missed" mudam para
##      "delayed", para cada grace_after_end_sec testado).
##   4) Ajustar grace_for_plots consoante o que vires em 3), e correr so'
##      esse bloco outra vez para comparar os plots novos com os do
##      relatorio original.
##

project_settings_file <- "userSettings_BSH.R"  ## ajustar para "userSettings_DGY.R" se for o caso

source(file.path("inputs", project_settings_file))
source("R/data_cache.R")
source("R/curtailment_response.R")
source("R/curtailment_shutdown_time.R")
source("R/curtailment_response_classify.R")
source("R/curtailment_response_grace.R")
source("R/curtailment_forensic_trace.R")

## cache/<farm_code>/ (layout atual) -- se ainda nao existir, tenta cache/
## direto (layout anterior a' mudanca de 2026-08 que separou a cache por
## farm_code; a tua ultima corrida pode ter sido feita antes dessa
## mudanca). So' usa o layout novo se ambos existirem.
folder_cache <- file.path("cache", farm_code)
if (!file.exists(file.path(folder_cache, "curtl_dt_unfilt.fst")) && file.exists(file.path("cache", "curtl_dt_unfilt.fst"))) {
  message("Aviso: a usar cache/ (layout antigo, sem separacao por farm_code) -- nao encontrada em ", folder_cache, ". Corre run_annual_analysis.R outra vez para regravar no layout novo.")
  folder_cache <- "cache"
}

## load_or_read_cache() com force_reread=FALSE só carrega o .fst já
## gravado -- o read_fn só seria chamado se a cache não existisse, o que
## aqui é tratado como erro (este script não deve reler ficheiros brutos).
.no_raw_reread <- function() stop(
  "Cache nao encontrada em '", folder_cache, "' -- corre run_annual_analysis",
  if (farm_code != "BSH") paste0("_", farm_code) else "", ".R pelo menos uma vez primeiro."
)

curtl_dt_unfilt <- load_or_read_cache(
  file.path(folder_cache, "curtl_dt_unfilt.fst"), .no_raw_reread, force_reread = FALSE, tz = proj_timezone
)
scada_dt_unfilt <- load_or_read_cache(
  file.path(folder_cache, "scada_dt_unfilt.fst"), .no_raw_reread, force_reread = FALSE, tz = proj_timezone
)

## Mesmo recorte de curtl_scada_dt que IDF_analysis.R usa na secção 3.5
## (curtailment response assessment) -- turbinas_scada/scada_ini/scada_end
## vem do settings file sourced acima.
curtl_scada_dt <- curtl_dt_unfilt[
  turbine %in% turbinas_scada & start >= scada_ini & start <= scada_end
]

cat(sprintf("%d curtailments no universo (turbinas_scada, janela scada_ini-scada_end).\n", nrow(curtl_scada_dt)))


## ---- 1) Tabela comparativa -- quantos "missed" mudam para "delayed",
##         para cada grace_after_end_sec testado (0 = comportamento atual,
##         sem grace window nenhuma) ----------------------------------

grace_candidates_sec <- c(0, 20, 40, 60, 90, 120)

grace_comparison_dt <- compare_grace_windows(
  curtl_scada_dt, scada_dt_unfilt, grace_candidates_sec = grace_candidates_sec,
  start_end_gap_sec = curtailment_start_end_gap_sec, max_next_gap_sec = curtailment_max_next_gap_sec,
  drop_pct_threshold = curtailment_drop_pct_threshold, rpm_threshold = safe_shutdown_rpm,
  shutdown_thresholds = shutdown_time_thresholds, shutdown_high_cut_sec = shutdown_time_high_cut
)

print(grace_comparison_dt)


## ---- 1.b) Mesma ideia, mas para time_to_rpm_thresholds() diretamente --
##           esta funcao alimenta tanto a secção "Shutdown Time" do
##           relatorio (summarise_time_to_threshold() -- %Reached, tempo
##           medio/mediano por limiar) como o "delayed" acima -- so' olha
##           para leituras dentro de [start, end] da propria ordem de
##           curtailment; grace_after_end_sec estende essa janela para
##           [start, end + grace]. Ver se %Reached sobe e o tempo medio se
##           torna mais realista com mais grace, para os 3 limiares (2, 1,
##           0 rpm) em conjunto (nao so' o "1o limiar" usado na
##           classificacao missed/delayed acima). --------------------------

shutdown_time_grace_dt <- compare_shutdown_time_grace(
  curtl_scada_dt, scada_dt_unfilt, grace_candidates_sec = grace_candidates_sec,
  thresholds = shutdown_time_thresholds, start_end_gap_sec = curtailment_start_end_gap_sec
)

print(shutdown_time_grace_dt)


## ---- 2) Replot dos mesmos exemplos do relatorio, com a classificacao
##         nova -- ajustar grace_for_plots consoante o que vires acima ---

grace_for_plots <- 60

response_flag_grace_dt <- classify_response_flag_grace(
  curtl_scada_dt, scada_dt_unfilt, grace_after_end_sec = grace_for_plots,
  start_end_gap_sec = curtailment_start_end_gap_sec, max_next_gap_sec = curtailment_max_next_gap_sec,
  drop_pct_threshold = curtailment_drop_pct_threshold, rpm_threshold = safe_shutdown_rpm,
  shutdown_thresholds = shutdown_time_thresholds, shutdown_high_cut_sec = shutdown_time_high_cut
)

missed_examples_new  <- select_curtailment_examples(response_flag_grace_dt, "missed",  n = curtailment_example_n)
delayed_examples_new <- select_curtailment_examples(response_flag_grace_dt, "delayed", n = curtailment_example_n)

p_missed_new <- plot_curtailment_events_rpm(
  missed_examples_new, scada_dt_unfilt,
  window_before_min = curtailment_example_window_before_min, window_after_min = curtailment_example_window_after_min,
  title = sprintf("Missed Curtailments -- grace_after_end_sec=%s", grace_for_plots)
)
p_delayed_new <- plot_curtailment_events_rpm(
  delayed_examples_new, scada_dt_unfilt,
  window_before_min = curtailment_example_window_before_min, window_after_min = curtailment_example_window_after_min,
  title = sprintf("Delayed Curtailments -- grace_after_end_sec=%s", grace_for_plots)
)

p_missed_new
p_delayed_new

## Casos reclassificados "missed" -> "delayed" com este grace_for_plots --
## grace_stop_sec e' quanto tempo (desde o INICIO do curtailment) ate' a
## turbina realmente cair abaixo do limiar, jah incluindo a grace window
reclassified_dt <- response_flag_grace_dt[!is.na(grace_stop_time)]
cat(sprintf("\n%d curtailment(s) reclassificados missed -> delayed com grace_after_end_sec=%s:\n", nrow(reclassified_dt), grace_for_plots))
print(reclassified_dt[, .(turbine, track_id, species, start, end, time_to_first_threshold_sec, grace_stop_sec)])


## ---- 3) Inspecao detalhada dos casos "delayed" ATUAIS (grace=0) -- ex:
##         um exemplo do relatorio aparecia com RPM=0 durante toda a janela
##         plotada, o que sugere um problema de match/gap de dados (o
##         turbina ja estava parada, nao foi um atraso real), nao a mesma
##         causa dos "missed" acima. Ver as colunas de match/validade para
##         perceber a origem exata. ------------------------------------

response_flag_dt_current <- classify_response_flag(
  curtl_scada_dt, scada_dt_unfilt,
  start_end_gap_sec = curtailment_start_end_gap_sec, max_next_gap_sec = curtailment_max_next_gap_sec,
  drop_pct_threshold = curtailment_drop_pct_threshold, rpm_threshold = safe_shutdown_rpm,
  shutdown_thresholds = shutdown_time_thresholds, shutdown_high_cut_sec = shutdown_time_high_cut
)
delayed_examples_current <- select_curtailment_examples(response_flag_dt_current, "delayed", n = curtailment_example_n)

cat("\nDetalhe dos exemplos 'delayed' atuais (grace=0, mesmos do relatorio):\n")
print(delayed_examples_current[, .(
  turbine, track_id, species, start, end,
  start_rpm, start_match_valid, end_rpm, end_match_valid,
  final_status, time_to_first_threshold_sec, response_flag
)])
