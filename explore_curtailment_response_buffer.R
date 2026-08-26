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
## "buffer window" depois do "end") antes de decidir que um curtailment foi
## mesmo "missed" -- ver R/curtailment_response_buffer.R,
## classify_response_flag_buffer(). Testa tambem a mesma ideia diretamente em
## time_to_rpm_thresholds() (R/curtailment_shutdown_time.R,
## compare_shutdown_time_buffer()) -- essa funcao alimenta tambem a secção
## "Shutdown Time" do relatorio (nao so' a classificacao missed/delayed),
## pelo mesmo motivo: so' procura leituras de RPM dentro da duracao da
## propria ordem de curtailment. Testa tambem, em conjunto com a buffer
## window, se rpm_threshold (nao drop_pct_threshold -- ver secção 1.a) e'
## demasiado apertado.
##
## Estado (2026-08): a secção 4 (Latency Test) e' o que foi efetivamente
## adotado em producao (IDF_analysis.R/IDF_monthly_report.R, com
## curtailment_latency_decline_pct/shutdown_time_buffer_sec/curtailment_cutin_rpm)
## -- ja' inclui o filtro de cutin_rpm (curtailments abaixo da velocidade de
## cut-in no start ficam de fora do no-response, ver R/curtailment_response_latency.R).
## As secções 1-3 (classify_response_flag()/buffer window sobre
## missed/delayed) exploram o esquema ANTERIOR, que o relatorio ja' nao usa
## em NENHUMA secção (a timeline de resposta vs. fenologia e a janela de
## resposta por incidente de fatalidade foram tambem migradas para
## time_to_first_decline()/cutin_rpm, 2026-08) -- ficam so' como registo de
## como se chegou a' decisao de adotar latencia em vez de missed/delayed.
## classify_response_flag() continua a existir e a ser testada (tests/
## test_curtailment_response_classify.R), so' deixou de alimentar o
## relatorio.
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
##      source("explore_curtailment_response_buffer.R") na consola).
##   3) Olhar para buffer_comparison_dt (quantos "missed" mudam para
##      "delayed", para cada buffer_after_end_sec testado).
##   4) Ajustar buffer_for_plots consoante o que vires em 3), e correr so'
##      esse bloco outra vez para comparar os plots novos com os do
##      relatorio original.
##

project_settings_file <- "userSettings_BSH.R"  ## ajustar para "userSettings_DGY.R" se for o caso

source(file.path("inputs", project_settings_file))
source("R/data_cache.R")
source("R/curtailment_response.R")
source("R/curtailment_shutdown_time.R")
source("R/curtailment_response_classify.R")
source("R/curtailment_response_buffer.R")
source("R/curtailment_response_latency.R")
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
##         para cada buffer_after_end_sec testado (0 = comportamento atual,
##         sem buffer window nenhuma) ----------------------------------

buffer_candidates_sec <- c(0, 20, 40, 60, 90, 120)

buffer_comparison_dt <- compare_buffer_windows(
  curtl_scada_dt, scada_dt_unfilt, buffer_candidates_sec = buffer_candidates_sec,
  start_end_gap_sec = curtailment_start_end_gap_sec, max_next_gap_sec = curtailment_max_next_gap_sec,
  drop_pct_threshold = curtailment_drop_pct_threshold, rpm_threshold = safe_shutdown_rpm,
  shutdown_thresholds = shutdown_time_thresholds, shutdown_high_cut_sec = shutdown_time_high_cut
)

print(buffer_comparison_dt)


## ---- 1.a) Sweep conjunto de rpm_threshold x buffer_after_end_sec -- Paulo
##          (2026-08) perguntou se o "drop measure" (drop_pct_threshold, 10%)
##          e' demasiado rigido/curto. NAO e' -- drop_pct_threshold so'
##          alimenta no_immediate_response (sinal a parte, nao entra em
##          response_flag). O que de facto decide "missed" e' rpm_threshold,
##          comparado ao RPM dentro de start_end_gap_sec (2s) do timestamp
##          exato de "end" (ver final_status em assess_curtailment_response(),
##          R/curtailment_response.R linha ~314) -- nao ha nenhuma medida de
##          "quanto caiu", so' "estava ou nao abaixo do limiar naquele
##          instante". Este sweep testa se um rpm_threshold mais alto (ex:
##          "abaixo de 2 ou 3 rpm ja conta como parada", nao so' <1) muda a
##          leitura, em conjunto com buffer_after_end_sec (secção 1 acima) --
##          cada linha reusa compare_buffer_windows() tal como esta' (ja'
##          otimizada por dentro), so' repetida para cada rpm_threshold
##          candidato (poucos valores, custo aceitavel). -------------------

rpm_threshold_candidates <- c(1, 1.5, 2, 3)

rpm_threshold_sweep_dt <- data.table::rbindlist(lapply(rpm_threshold_candidates, function(rt) {
  dt <- compare_buffer_windows(
    curtl_scada_dt, scada_dt_unfilt, buffer_candidates_sec = buffer_candidates_sec,
    start_end_gap_sec = curtailment_start_end_gap_sec, max_next_gap_sec = curtailment_max_next_gap_sec,
    drop_pct_threshold = curtailment_drop_pct_threshold, rpm_threshold = rt,
    shutdown_thresholds = shutdown_time_thresholds, shutdown_high_cut_sec = shutdown_time_high_cut
  )
  dt[, rpm_threshold := rt]
  dt[]
}))
data.table::setcolorder(rpm_threshold_sweep_dt, c("rpm_threshold", "buffer_after_end_sec", "response_flag", "n", "pct_of_total", "pct_of_known"))

print(rpm_threshold_sweep_dt)


## ---- 1.b) Mesma ideia, mas para time_to_rpm_thresholds() diretamente --
##           esta funcao alimenta tanto a secção "Shutdown Time" do
##           relatorio (summarise_time_to_threshold() -- %Reached, tempo
##           medio/mediano por limiar) como o "delayed" acima -- so' olha
##           para leituras dentro de [start, end] da propria ordem de
##           curtailment; buffer_after_end_sec estende essa janela para
##           [start, end + buffer]. Ver se %Reached sobe e o tempo medio se
##           torna mais realista com mais buffer, para os 3 limiares (2, 1,
##           0 rpm) em conjunto (nao so' o "1o limiar" usado na
##           classificacao missed/delayed acima). --------------------------

shutdown_time_buffer_dt <- compare_shutdown_time_buffer(
  curtl_scada_dt, scada_dt_unfilt, buffer_candidates_sec = buffer_candidates_sec,
  thresholds = shutdown_time_thresholds, start_end_gap_sec = curtailment_start_end_gap_sec,
  cutin_rpm = curtailment_cutin_rpm
)

print(shutdown_time_buffer_dt)


## ---- 2) Replot dos mesmos exemplos do relatorio, com a classificacao
##         nova -- ajustar buffer_for_plots consoante o que vires acima ---

buffer_for_plots <- 60

response_flag_buffer_dt <- classify_response_flag_buffer(
  curtl_scada_dt, scada_dt_unfilt, buffer_after_end_sec = buffer_for_plots,
  start_end_gap_sec = curtailment_start_end_gap_sec, max_next_gap_sec = curtailment_max_next_gap_sec,
  drop_pct_threshold = curtailment_drop_pct_threshold, rpm_threshold = safe_shutdown_rpm,
  shutdown_thresholds = shutdown_time_thresholds, shutdown_high_cut_sec = shutdown_time_high_cut
)

missed_examples_new  <- select_curtailment_examples(response_flag_buffer_dt, "missed",  n = curtailment_example_n)
delayed_examples_new <- select_curtailment_examples(response_flag_buffer_dt, "delayed", n = curtailment_example_n)

p_missed_new <- plot_curtailment_events_rpm(
  missed_examples_new, scada_dt_unfilt,
  window_before_min = curtailment_example_window_before_min, window_after_min = curtailment_example_window_after_min,
  title = sprintf("Missed Curtailments -- buffer_after_end_sec=%s", buffer_for_plots)
)
p_delayed_new <- plot_curtailment_events_rpm(
  delayed_examples_new, scada_dt_unfilt,
  window_before_min = curtailment_example_window_before_min, window_after_min = curtailment_example_window_after_min,
  title = sprintf("Delayed Curtailments -- buffer_after_end_sec=%s", buffer_for_plots)
)

p_missed_new
p_delayed_new

## Casos reclassificados "missed" -> "delayed" com este buffer_for_plots --
## buffer_stop_sec e' desde o INICIO do curtailment (comparavel a
## time_to_first_threshold_sec); buffer_stop_after_end_sec e' desde o FIM da
## ordem -- e' este ultimo que e' comparado contra buffer_after_end_sec, por
## isso deve aparecer sempre <= buffer_for_plots aqui (bom sanity check: se
## nao aparecer, algo esta errado)
reclassified_dt <- response_flag_buffer_dt[!is.na(buffer_stop_time)]
cat(sprintf("\n%d curtailment(s) reclassificados missed -> delayed com buffer_after_end_sec=%s:\n", nrow(reclassified_dt), buffer_for_plots))
print(reclassified_dt[, .(turbine, track_id, species, start, end, time_to_first_threshold_sec, buffer_stop_sec, buffer_stop_after_end_sec)])


## ---- 3) Inspecao detalhada dos casos "delayed" ATUAIS (buffer=0) -- ex:
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

cat("\nDetalhe dos exemplos 'delayed' atuais (buffer=0, mesmos do relatorio):\n")
print(delayed_examples_current[, .(
  turbine, track_id, species, start, end,
  start_rpm, start_match_valid, end_rpm, end_match_valid,
  final_status, time_to_first_threshold_sec, response_flag
)])


## ---- 4) Latency Test -- tempo desde o "start" ate a turbina COMECAR a
##         reagir (nao ate parar) -- pergunta separada da dos "missed"/
##         "delayed" acima. A equipa de desenvolvimento do IDF sugere uma
##         latencia esperada de ~20s; o Paulo acha que pode ser maior,
##         ~30s. Ver R/curtailment_response_latency.R para o racional do
##         limiar RELATIVO (decline_pct_threshold, fracao do RPM de
##         baseline) em vez de um valor absoluto de RPM, para o filtro de
##         cutin_rpm (curtailments ja abaixo da velocidade de cut-in no
##         start ficam de fora do reached/no-response, contados a parte em
##         n_below_cutin/pct_below_cutin), e para o de no_data (curtailments
##         sem leitura SCADA fiavel no start -- contados em
##         n_no_data/pct_no_data, ja NAO desaparecem silenciosamente de
##         latency_dt como acontecia antes de 2026-08). ------------------

latency_threshold_sweep_dt <- compare_latency_thresholds(
  curtl_scada_dt, scada_dt_unfilt, buffer_after_end_sec = shutdown_time_buffer_sec,
  cutin_rpm = curtailment_cutin_rpm
)
print(latency_threshold_sweep_dt)

## Detalhe (media/mediana, % <=20s, % <=30s) para o limiar de producao
## (curtailment_latency_decline_pct/shutdown_time_buffer_sec/curtailment_cutin_rpm -- userSettings_BSH.R)
latency_dt <- time_to_first_decline(
  curtl_scada_dt, scada_dt_unfilt, decline_pct_threshold = curtailment_latency_decline_pct,
  buffer_after_end_sec = shutdown_time_buffer_sec, cutin_rpm = curtailment_cutin_rpm
)
latency_summary <- summarise_latency(latency_dt)
print(latency_summary)
print(summarise_latency_bands(latency_dt))
print(summarise_latency_by_turbine(latency_dt))

p_latency <- plot_latency_histogram(latency_dt)
p_latency


## ---- 5) Diagnostico de um caso especifico -- BSH54, 2025-11-11 13:47:38
##         (Steppe-Eagle), RPM achatado ~6.5 durante toda a janela do plot
##         "Missed Curtailments" (secções 2/3 acima, esquema ANTIGO).
##         Paulo perguntou (2026-08) se este caso continua a ser apanhado
##         pelo esquema NOVO (latency_dt/cutin_rpm) ou se "desaparecemos"
##         dele. Ajustar case_turbine/case_start para investigar outro caso.
##
##         RESULTADO CONFIRMADO (2026-08): este caso tinha 0 linhas em
##         latency_dt -- o match do baseline no start falhava
##         (start_end_gap_sec=2s demasiado apertado face a' amostragem
##         SCADA, nao falta real de dados) e o caso desaparecia por completo
##         da analise, sem contar em lado nenhum. Corrigido: latency_dt
##         agora inclui TODOS os curtailments, com no_data=TRUE para este
##         tipo de caso (ver R/curtailment_response_latency.R,
##         time_to_first_decline()) -- este caso deve aparecer agora com 1
##         linha, no_data=TRUE, start_rpm/below_cutin/latency_sec = NA.
##
##         Leitura do resultado (para outros casos que se investiguem aqui):
##           - no_data=TRUE -- sem leitura SCADA fiavel no start
##             (start_end_gap_sec); contado em n_no_data/pct_no_data em vez
##             de desaparecer.
##           - no_data=FALSE, below_cutin=TRUE -- excluido por estar abaixo
##             do cut-in no start.
##           - no_data=FALSE, below_cutin=FALSE, latency_sec=NA -- conta
##             como no-response no relatorio atual.
##           - no_data=FALSE, below_cutin=FALSE, latency_sec preenchido -- o
##             algoritmo encontrou uma queda algures na janela [start, end +
##             buffer]; a impressao das leituras SCADA em bruto abaixo mostra
##             se essa queda e' real (a turbina respondeu mais tarde do que o
##             plot mostra, cortado a poucos minutos) ou um artefacto (ex:
##             "end" anomalamente longe no futuro, alargando a janela de
##             procura muito alem do que faz sentido para esta ordem). ------

start_end_gap_candidates <- c(2, 5, 10, 15)

case_turbine <- "BSH54"
case_start   <- as.POSIXct("2025-11-11 13:47:38", tz = proj_timezone)

case_dt <- latency_dt[
  turbine == case_turbine & abs(as.numeric(difftime(start, case_start, units = "secs"))) < 5
]
cat(sprintf("\nCaso especifico (%s, %s) em latency_dt:\n", case_turbine, case_start))
print(case_dt)

if (nrow(case_dt) == 1L) {
  window_end_full <- case_dt$end + shutdown_time_buffer_sec
  cat(sprintf(
    "\nDuracao da propria ordem (end - start): %.1f s -- janela de procura completa (start ate' end+buffer): [%s, %s]\n",
    as.numeric(difftime(case_dt$end, case_dt$start, units = "secs")),
    format(case_dt$start), format(window_end_full)
  ))

  cat("\nLeituras SCADA (RPM) nessa turbina, em TODA a janela de procura (nao so' o recorte do plot):\n")
  print(scada_dt_unfilt[
    turbinelabel == case_turbine & readingname == "RPM" &
      datetime >= case_dt$start & datetime <= window_end_full
  ][order(datetime)])

  ## leituras ANTES do start -- necessarias para ver a que distancia esta'
  ## a leitura que o match backward-only (roll = start_end_gap_sec, ver
  ## R/curtailment_response.R) precisaria de aceitar; o bloco acima so'
  ## mostra a janela [start, end+buffer], que nunca contem essa leitura
  cat("\nLeituras SCADA (RPM) nessa turbina, nos 60s ANTES do start:\n")
  print(scada_dt_unfilt[
    turbinelabel == case_turbine & readingname == "RPM" &
      datetime >= case_dt$start - 60 & datetime < case_dt$start
  ][order(datetime)])

  ## a que start_end_gap_sec e' que este caso deixa de ser no_data, com o
  ## match backward-only? -- confere se o resultado do case_dt acima (com
  ## o gap por omissao, 2s) muda nos valores testados no sweep da secção 6
  case_events <- data.table::data.table(id = 1L, turbine = case_turbine, event_time = case_dt$start)
  for (g in start_end_gap_candidates) {
    m <- match_nearest_rpm(case_events, scada_dt_unfilt, max_gap_sec = g, roll = g)
    cat(sprintf(
      "  start_end_gap_sec=%2ds -> valid_match=%s, start_rpm=%s, gap_sec=%s\n",
      g, m$valid_match, round(m$rpm, 2), round(m$gap_sec, 1)
    ))
  }
} else {
  cat("Caso nao encontrado em latency_dt (0 ou >1 linhas) -- a ver curtl_scada_dt diretamente:\n")
  print(curtl_scada_dt[turbine == case_turbine & abs(as.numeric(difftime(start, case_start, units = "secs"))) < 5])
}


## ---- 6) Sweep de start_end_gap_sec -- agora seguro para relaxar --------
##
##         Pergunta do Paulo (2026-08): relaxar start_end_gap_sec (a
##         tolerancia do match do baseline no "start") reduz no_data, mas
##         arrisca "mascarar o estado real" se a leitura aceite como
##         baseline vier de DEPOIS do sinal, ja com alguma desaceleracao
##         real incluida -- o que enviesaria a queda percentual medida a
##         partir dai (o limiar de queda passaria a ser calculado sobre um
##         baseline ja mais baixo, exigindo MAIS queda adicional para
##         disparar, o que tenderia a SOBRESTIMAR a latencia -- classificar
##         respostas rapidas como lentas ou ate' como no-response --, nao a
##         subestima-la).
##
##         FIX (2026-08, ver R/curtailment_response.R, match_nearest_rpm(),
##         novo parametro `roll`): o baseline do "start" em
##         time_to_first_decline() passou a usar roll = start_end_gap_sec
##         (LOCF -- so' aceita leitura ANTES do start, nunca depois) em vez
##         de roll = "nearest". Isto elimina por construcao a via de
##         contaminacao acima -- uma leitura anterior ao sinal de start
##         nunca pode conter a resposta a esse mesmo sinal, seja qual for a
##         tolerancia usada. Com isto, relaxar start_end_gap_sec so' pode
##         reduzir no_data (aceitando leituras SCADA mais antigas como
##         baseline), sem enviesar a latencia medida -- o sweep abaixo
##         confirma isso: mean/median latency devem manter-se estaveis
##         entre 2/5/10/15s, mudando so' n_no_data/pct_no_data. -----------

start_end_gap_sweep_dt <- data.table::rbindlist(lapply(start_end_gap_candidates, function(g) {
  lat_dt <- time_to_first_decline(
    curtl_scada_dt, scada_dt_unfilt, decline_pct_threshold = curtailment_latency_decline_pct,
    start_end_gap_sec = g, buffer_after_end_sec = shutdown_time_buffer_sec, cutin_rpm = curtailment_cutin_rpm
  )
  summ <- summarise_latency(lat_dt)
  summ[, start_end_gap_sec := g]
  data.table::setcolorder(summ, c("start_end_gap_sec", setdiff(names(summ), "start_end_gap_sec")))
  summ[]
}))

print(start_end_gap_sweep_dt[, .(
  start_end_gap_sec, n_no_data, pct_no_data, n_eligible,
  n_reached, pct_no_response, mean_latency_sec, median_latency_sec
)])


## ---- 6b) Distribuicao real do gap ate' a leitura SCADA ANTERIOR --------
##
##         O sweep acima (secção 6) mostrou pct_no_data muito mais alto do
##         que seria de esperar so' pelo ciclo SCADA (~10s): 87.8% a 2s,
##         70.1% a 5s, e ainda 41.0%/40.1% a 10s/15s (2026-08, primeira
##         corrida do Paulo). Se o SCADA tivesse sempre um "tick" regular
##         de ~10s, 15s de tolerancia backward-only deveria cobrir quase
##         100% dos casos -- os 40% que sobram a 15s sugerem lacunas MAIORES
##         e sistematicas nalgumas turbinas/periodos (ex: RPM pode nao ser
##         reportado enquanto a turbina ja esta' parada/em curtailment,
##         nao so' amostrado a cada 10s), nao so' o efeito esperado do
##         "roll" ser so' num sentido. Este bloco mede o gap REAL ate' a
##         leitura anterior mais proxima (sem limite de producao, so' para
##         caracterizar), para distinguir as duas hipoteses antes de
##         escolher um valor de start_end_gap_sec.

diag_events <- curtl_scada_dt[, .(id = .I, turbine, event_time = start)]
diag_match  <- match_nearest_rpm(diag_events, scada_dt_unfilt, max_gap_sec = 3600, roll = 3600)

cat(sprintf(
  "\nGap ate' a leitura SCADA anterior ao start (%d curtailments; sem NENHUMA leitura anterior em 1h: %d, %.1f%%):\n",
  nrow(diag_match), sum(is.na(diag_match$gap_sec)),
  100 * sum(is.na(diag_match$gap_sec)) / nrow(diag_match)
))
print(summary(diag_match$gap_sec))
print(quantile(diag_match$gap_sec, probs = c(0.5, 0.75, 0.9, 0.95, 0.99), na.rm = TRUE))

for (g in c(2, 5, 10, 15, 30, 60, 120)) {
  cat(sprintf(
    "  <= %3ds: %5.1f%%\n", g,
    100 * sum(diag_match$gap_sec <= g, na.rm = TRUE) / nrow(diag_match)
  ))
}

## repartido por turbina -- se a lacuna for sistematica nalgumas turbinas
## em particular (em vez de espalhada por todas), aparece aqui como
## pct_le_15s muito mais baixo nessas linhas
diag_by_turbine <- diag_match[, .(
  n = .N,
  pct_le_15s = round(100 * sum(gap_sec <= 15, na.rm = TRUE) / .N, 1),
  pct_no_prior_1h = round(100 * sum(is.na(gap_sec)) / .N, 1)
), by = turbine]
data.table::setorder(diag_by_turbine, pct_le_15s)
print(diag_by_turbine)
