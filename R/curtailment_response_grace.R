##
## Variante experimental de classify_response_flag() (R/curtailment_response_classify.R)
## -- testa a hipotese (Paulo, 2026-08, a partir dos plots de exemplo da
## secção "Example Response Profiles") de que alguns curtailments
## classificados "missed" (final_status == partial_or_no_stop -- RPM ainda
## acima do limiar EXATAMENTE no timestamp de fim da propria ordem de
## curtailment) na verdade PARARAM, so' que depois do fim da ordem -- a
## inercia mecanica da turbina pode levar mais tempo a completar a paragem
## do que a duracao da propria ordem de curtailment.
##
## Nao substitui classify_response_flag() na pipeline de producao
## (IDF_analysis.R) -- e' so' para o script de exploracao
## (explore_curtailment_response_grace.R) comparar classificacoes
## side-by-side antes de decidir se isto deve passar a ser o comportamento
## por omissao (e, se sim, com que grace_after_end_sec).
##
## Nota de performance (2026-08, apos o Paulo reportar demoras): a 1ª
## versao deste ficheiro recalculava classify_response_flag() (o roll join
## caro com scada_dt) uma vez por CADA grace_after_end_sec testado, e para
## cada curtailment "missed" fazia um for loop com um filtro NAO indexado
## sobre a tabela de RPM completa (todas as turbinas, historico completo).
## Com N candidatos de grace window e M curtailments "missed", isso e'
## O(N x M x tamanho da tabela de RPM) -- lento mesmo com poucos exemplos.
## Esta versao separa o trabalho caro (classify_response_flag(), 1 vez so')
## do trabalho por-candidato (barato): find_grace_stop_times() calcula, de
## uma vez, a 1ª leitura de RPM abaixo do limiar depois do "end" de CADA
## curtailment "missed" (1 non-equi join do data.table, sem for loop nem
## table scan por evento); apply_grace_window() so' compara esse instante
## contra cada grace_after_end_sec candidato (comparacao vetorizada,
## essencialmente instantanea). compare_grace_windows() e'
## classify_response_flag_grace() usam os 2 -- o sweep de varios candidatos
## deixa de ter custo extra por candidato.
##
## Depende de: data.table, R/curtailment_response.R, R/curtailment_shutdown_time.R,
## R/curtailment_response_classify.R (fazer source destes 3 antes)
##


## Para cada curtailment "missed" (1 linha por curtailment_id/turbine/
## start/end), encontra a 1ª leitura de SCADA com RPM < rpm_threshold
## DEPOIS do "end" -- sem limite de tempo aqui (o limite de
## grace_after_end_sec e' aplicado depois, em apply_grace_window(), sem
## precisar de refazer este calculo). Devolve 1 linha por curtailment_id
## de missed_dt, com grace_stop_time = NA se nao houver nenhuma leitura
## abaixo do limiar depois do "end" (turbina nunca mais para, ou os dados
## de SCADA acabam antes disso).
##
## Implementacao: 1 non-equi rolling join (rpm_dt[missed_dt, on = .(turbine,
## datetime > end), mult = "first"]) -- o data.table usa pesquisa binaria
## pela key, nao um table scan; e' o equivalente eficiente de "para cada
## evento, a proxima leitura depois de X", sem for loop.

## Nota: grace_stop_sec e' relativo ao INICIO do curtailment (start), tal
## como time_to_first_threshold_sec (para serem diretamente comparaveis).
## grace_stop_after_end_sec e' relativo ao FIM da ordem (end) -- e' esse
## valor, nao grace_stop_sec, que e' comparado contra grace_after_end_sec
## em apply_grace_window(); incluido aqui tambem so' para tornar a
## inspecao/debug mais direta (evita ter de subtrair a duracao da ordem a
## olho para perceber se um evento cai dentro da janela).

find_grace_stop_times <- function(missed_dt, scada_dt, rpm_threshold = 1) {

  empty <- data.table::data.table(
    curtailment_id = integer(), grace_stop_time = as.POSIXct(character()),
    grace_stop_sec = numeric(), grace_stop_after_end_sec = numeric()
  )
  if (nrow(missed_dt) == 0L) return(empty)

  turbines_of_interest <- unique(missed_dt$turbine)
  # filtrar por turbina E por rpm < limiar ANTES do join -- so' as turbinas
  # e leituras relevantes ficam na tabela usada pelo join, nao o SCADA
  # inteiro do parque
  rpm_dt <- scada_dt[
    readingname == "RPM" & turbinelabel %in% turbines_of_interest & value < rpm_threshold,
    .(turbine = turbinelabel, datetime, rpm = value)
  ]
  if (nrow(rpm_dt) == 0L) {
    out <- data.table::copy(missed_dt)[, `:=`(
      grace_stop_time = as.POSIXct(NA), grace_stop_sec = NA_real_, grace_stop_after_end_sec = NA_real_
    )]
    return(out[, .(curtailment_id, grace_stop_time, grace_stop_sec, grace_stop_after_end_sec)])
  }
  data.table::setkey(rpm_dt, turbine, datetime)

  qy <- missed_dt[, .(curtailment_id, turbine, start, end)]
  match_dt <- rpm_dt[qy, on = .(turbine, datetime > end), mult = "first"]

  match_dt[, .(
    curtailment_id,
    grace_stop_time = datetime,
    grace_stop_sec           = as.numeric(difftime(datetime, start, units = "secs")),
    grace_stop_after_end_sec = as.numeric(difftime(datetime, end, units = "secs"))
  )]
}


## Reclassifica "missed" -> "delayed" onde grace_stop_time (de
## find_grace_stop_times(), calculado 1 vez so') cai dentro de
## grace_after_end_sec depois do "end" -- so' um merge + comparacao
## vetorizada, seguro para chamar repetidamente com valores diferentes de
## grace_after_end_sec sem recalcular nada caro.

apply_grace_window <- function(base_dt, grace_stop_dt, grace_after_end_sec) {

  # data.table::copy() explicito -- merge.data.table() pode devolver
  # colunas nao tocadas pelo join (ex: response_flag, que vem so' de
  # base_dt) como o MESMO objeto em memoria de base_dt, nao uma copia. Sem
  # este copy(), o := abaixo mutava base_dt por referencia -- bug real
  # encontrado 2026-08: numa sweep que reutiliza o mesmo base_dt em varias
  # chamadas (compare_grace_windows()), a mutacao da 1ª chamada "vazava"
  # para todas as seguintes, fazendo TODOS os "missed" passarem a
  # "delayed" logo no 1o grace_after_end_sec testado, em vez de subir
  # gradualmente como devia.
  out <- data.table::copy(merge(base_dt, grace_stop_dt, by = "curtailment_id", all.x = TRUE))

  accept <- out$response_flag == "missed" &
    !is.na(out$grace_stop_after_end_sec) &
    out$grace_stop_after_end_sec <= grace_after_end_sec

  out[accept, response_flag := "delayed"]
  out[]
}


## Wrapper de conveniencia para um SO' valor de grace_after_end_sec (usado
## no script para replotar os exemplos com uma classificacao especifica) --
## grace_after_end_sec = 0 reproduz exatamente classify_response_flag(),
## sem reclassificacao nenhuma.

classify_response_flag_grace <- function(curtl_dt, scada_dt, grace_after_end_sec = 20,
                                         start_end_gap_sec = 2, max_next_gap_sec = 20,
                                         drop_pct_threshold = 0.10, rpm_threshold = 1,
                                         shutdown_thresholds = c(2, 1, 0), shutdown_high_cut_sec = 50) {

  base_dt <- classify_response_flag(
    curtl_dt, scada_dt, start_end_gap_sec = start_end_gap_sec,
    max_next_gap_sec = max_next_gap_sec, drop_pct_threshold = drop_pct_threshold,
    rpm_threshold = rpm_threshold, shutdown_thresholds = shutdown_thresholds,
    shutdown_high_cut_sec = shutdown_high_cut_sec
  )

  # so' adiciona as colunas grace_stop_* "vazias" aqui quando
  # apply_grace_window() NAO vai correr (early return) -- caso contrario o
  # merge() la' dentro ja' as traz, e adiciona-las aqui tambem causava
  # colisao de nomes (merge() sufixava .x/.y em vez de manter os nomes
  # simples)
  tz <- attr(curtl_dt$start, "tzone")
  add_empty_grace_cols <- function(dt) {
    dt[, grace_stop_time := as.POSIXct(NA, tz = tz)]
    dt[, grace_stop_sec  := NA_real_]
    dt[, grace_stop_after_end_sec := NA_real_]
    dt[]
  }

  if (nrow(base_dt) == 0L || grace_after_end_sec <= 0) return(add_empty_grace_cols(base_dt))

  missed_dt <- base_dt[response_flag == "missed"]
  if (nrow(missed_dt) == 0L) return(add_empty_grace_cols(base_dt))

  grace_stop_dt <- find_grace_stop_times(missed_dt, scada_dt, rpm_threshold = rpm_threshold)
  apply_grace_window(base_dt, grace_stop_dt, grace_after_end_sec)[]
}


## Resumo comparativo -- para cada grace_after_end_sec testado, quantos
## eventos ficam em cada response_flag (mesmas colunas de
## summarise_response_by_flag(), R/curtailment_response_classify.R, com
## grace_after_end_sec como coluna extra para comparar lado a lado).
##
## classify_response_flag() e find_grace_stop_times() (as 2 partes caras)
## so' correm 1 vez cada, independentemente de quantos valores existirem em
## grace_candidates_sec -- ver nota de performance no topo do ficheiro.

compare_grace_windows <- function(curtl_dt, scada_dt, grace_candidates_sec = c(0, 20, 40, 60, 90, 120),
                                  start_end_gap_sec = 2, max_next_gap_sec = 20,
                                  drop_pct_threshold = 0.10, rpm_threshold = 1,
                                  shutdown_thresholds = c(2, 1, 0), shutdown_high_cut_sec = 50) {

  base_dt <- classify_response_flag(
    curtl_dt, scada_dt, start_end_gap_sec = start_end_gap_sec,
    max_next_gap_sec = max_next_gap_sec, drop_pct_threshold = drop_pct_threshold,
    rpm_threshold = rpm_threshold, shutdown_thresholds = shutdown_thresholds,
    shutdown_high_cut_sec = shutdown_high_cut_sec
  )
  missed_dt <- base_dt[response_flag == "missed"]
  grace_stop_dt <- find_grace_stop_times(missed_dt, scada_dt, rpm_threshold = rpm_threshold)

  out <- data.table::rbindlist(lapply(grace_candidates_sec, function(g) {
    dt <- if (g <= 0 || nrow(missed_dt) == 0L) base_dt else apply_grace_window(base_dt, grace_stop_dt, g)
    by_flag <- summarise_response_by_flag(dt)
    by_flag[, grace_after_end_sec := g]
    by_flag[]
  }))

  data.table::setcolorder(out, c("grace_after_end_sec", "response_flag", "n", "pct_of_total", "pct_of_known"))
  data.table::setorder(out, grace_after_end_sec, response_flag)
  out[]
}
