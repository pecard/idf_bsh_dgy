##
## Cruza intervalos "offline" de uma unidade IDF (heartbeats) com evidencia
## de que o sistema continuava operacional -- curtailments disparados e/ou
## leituras SCADA de RPM -- pedido do Paulo, 2026-09.
##
## Um intervalo offline (sem heartbeat, ver compute_offline_intervals(),
## R/availability_daylight.R) e' interpretado, por omissao, como a
## protecao das aves desligada nesse periodo. Mas pode ser so' uma falha de
## comunicacao do PROPRIO heartbeat -- com o sistema de deteção/curtailment
## a continuar operacional. 2 fontes de evidencia independentes:
##   - curtailment disparado pela turbina DENTRO do intervalo -- evidencia
##     forte de que a unidade IDF estava mesmo a decidir/atuar (nao so' o
##     heartbeat que falhou)
##   - leitura SCADA de RPM da turbina DENTRO do intervalo -- evidencia de
##     que a turbina/SCADA estava a reportar normalmente (mecanicamente
##     operacional), independente da unidade IDF em si
## Um caso real (DGY, 2026-07, ver historial) mostrou as 2 fontes a
## divergir: heartbeat E scada ausentes num sub-periodo, mas com
## curtailments a disparar na mesma -- por isso classify_offline_evidence()
## abaixo distingue os 2 sinais em vez de os fundir num so' boolean.
##
## Depende de: data.table
##
## Uso:
##   source("R/availability_daylight.R")   # compute_offline_intervals()
##   source("R/turbine_idf_coverage.R")    # turbines_by_idf_threshold() (fallback, ver abaixo)
##   source("R/offline_curtailment_check.R")
##
##   offline_dt <- compute_offline_intervals(heartb_dt, offline_gap_min, online_grace_min)
##
##   ## so' a porcao DIURNA de cada gap (clip_offline_intervals_to_daylight(),
##   ## R/availability_daylight.R) -- OBRIGATORIO passar isto (nao offline_dt
##   ## diretamente) aos 2 checks abaixo, senao os minutos classificados
##   ## podem exceder offline_mins_total (sempre so' diurno,
##   ## summarise_availability()), dando net_offline_pct negativo em
##   ## summarise_net_availability() (ver 5b abaixo):
##   offline_dt_daylight <- clip_offline_intervals_to_daylight(offline_dt, daylight_cal, tz)
##
##   ## turbina(s) a verificar por unidade IDF -- 2 fontes possiveis, matriz
##   ## manual e' SEMPRE preferida quando existir (ver 1a abaixo):
##   idf_turbines_dt <- idf_turbines_from_manual_matrix(turbine_idf_manual_dt)
##   # OU, so' se nao houver ficheiro de matriz manual identificado para
##   # este parque (turbine_idf_manual_dt inexistente/NULL):
##   idf_turbines_dt <- idf_turbines_from_coverage(turbine_idf_coverage_dt)
##
##   curtl_checked_dt <- check_offline_curtailment_overlap(offline_dt_daylight, curtl_dt, idf_turbines_dt)
##   scada_checked_dt <- check_offline_scada_presence(offline_dt_daylight, scada_dt, idf_turbines_dt)
##   combined_dt <- classify_offline_evidence(curtl_checked_dt, scada_checked_dt)
##   offline_evidence_summary <- summarise_offline_evidence(combined_dt)
##
##   ## disponibilidade "para efeitos de contrato" -- raw vs. net/confirmado
##   ## vs. sem evidencia (ver 5b abaixo); availability_overall vem de
##   ## summarise_availability_overall(), R/monthly_technical_summary.R
##   ## (funcao generica, apesar do nome do ficheiro):
##   summarise_net_availability(availability_overall, offline_evidence_summary$overall)
##


## 1. Turbina(s) a verificar por unidade IDF -- 2 formas de as obter --------

## 1a. A partir da matriz manual (Turbine ID -> Primary IDF) -- fonte
## PREFERIDA sempre que o ficheiro existir para o parque (decisao do
## Paulo, 2026-09, apos rever o caso da DZH62-04, primaria genuina de 2
## turbinas, DZH62 E DZH63 -- reflete conhecimento operacional real, que
## nenhum metodo geometrico consegue replicar sozinho). So' a atribuicao
## Primary conta (colunas "Turbine ID"/"Primary IDF", ou ja' renomeadas
## "turbine"/"primary_idf") -- mesma fonte de join_availability_to_turbine(),
## R/availability_daylight.R.
idf_turbines_from_manual_matrix <- function(turbine_idf_manual_dt) {
  manual_dt <- data.table::as.data.table(turbine_idf_manual_dt)
  data.table::setnames(
    manual_dt,
    old = c("Turbine ID", "Primary IDF"),
    new = c("turbine", "primary_idf"),
    skip_absent = TRUE
  )
  unique(manual_dt[!is.na(primary_idf), .(idf = primary_idf, turbine)])
}

## 1b. A partir da cobertura geometrica 2D (turbines_by_idf_threshold(),
## R/turbine_idf_coverage.R) -- fallback, so' usado quando NAO existe
## ficheiro de matriz manual identificado para este parque (turbine_idf_manual_dt
## inexistente/NULL) -- ver explore_offline_curtailment_check.R para a
## logica de escolha entre 1a/1b.
##
## min_pct_coverage (limiar de %, NAO um numero fixo de turbinas) --
## decisao do Paulo, 2026-09: uma unidade IDF pode ser genuinamente
## primaria para mais do que 1 turbina, em numero VARIAVEL (nem sempre o
## mesmo entre unidades/parques) -- um Top-N fixo tanto podia deixar de
## fora uma turbina genuina como incluir uma de cobertura fraca so' por
## calhar ser a 2a melhor (caso real, DGY: DZH63 atribuida tanto a
## DZH62-04 como a DZH64-03 com Top-2, mesmo a DZH64 -- a turbina PROPRIA
## da DZH64-03 -- estando confirmadamente sem sinal SCADA nesse periodo).
## Ver OFFLINE_EVIDENCE_SCOPE_NOTE abaixo para a salvaguarda a incluir no
## relatorio sempre que este fallback for usado.
idf_turbines_from_coverage <- function(coverage_dt, min_pct_coverage = 20) {
  turbines_by_idf_threshold(coverage_dt, min_pct_coverage = min_pct_coverage)[, .(idf, turbine)]
}

## 1c. resolve_idf_turbines() -- decide entre 1a/1b (matriz manual SEMPRE
## preferida quando existir; fallback geometrico so' quando nao existir),
## calculando a cobertura geometrica na hora se ainda nao existir na
## sessao. Extraido do if/else que estava duplicado em
## explore_offline_curtailment_check.R, IDF_analysis.R e
## IDF_monthly_report.R -- os 3 devem chamar esta funcao em vez de
## repetirem a logica.
##
## turbine_idf_manual_dt: NULL/inexistente se nao houver ficheiro de matriz
## manual identificado para o parque -- nesse caso ativa o fallback.
## turbine_idf_coverage_dt: cobertura geometrica ja calculada nesta sessao
## (compute_turbine_idf_coverage(), R/turbine_idf_coverage.R), se existir --
## poupa recalcular. So' usada/exigida quando o fallback for necessario.
## wtg/idf_sf/buffer_m/wtg_id_col/idf_id_col: so' usados para calcular
## turbine_idf_coverage_dt quando esta ainda nao existir E o fallback for
## necessario -- idf_sf ja' deve estar projetado no CRS planar e com a
## coluna idf_id_col preparada (mesma normalizacao de IDF_analysis.R,
## secção "0. Import data").
##
## Devolve list(idf_turbines_dt, used_geometric_fallback,
## turbine_idf_coverage_dt) -- este ultimo NULL quando a matriz manual foi
## usada (nao ha' cobertura geometrica a devolver), ou a tabela calculada/
## reutilizada quando o fallback foi ativado (para reuso a jusante, ex:
## escrever num xlsx de validacao).

resolve_idf_turbines <- function(turbine_idf_manual_dt = NULL,
                                 turbine_idf_coverage_dt = NULL,
                                 wtg = NULL, idf_sf = NULL,
                                 wtg_id_col = "InternalNa", idf_id_col = "imaging_he",
                                 buffer_m = NULL,
                                 min_pct_coverage = 20) {

  if (!is.null(turbine_idf_manual_dt)) {
    return(list(
      idf_turbines_dt = idf_turbines_from_manual_matrix(turbine_idf_manual_dt),
      used_geometric_fallback = FALSE,
      turbine_idf_coverage_dt = NULL
    ))
  }

  message(sprintf(
    "Nenhuma matriz manual (turbine_idf_manual_dt) identificada para este parque -- a usar cobertura geometrica 2D como fallback (turbinas com >= %d%% de sobreposicao de buffer).",
    min_pct_coverage
  ))

  if (is.null(turbine_idf_coverage_dt)) {
    if (is.null(wtg) || is.null(idf_sf) || is.null(buffer_m)) {
      stop("resolve_idf_turbines(): nem turbine_idf_manual_dt nem turbine_idf_coverage_dt disponiveis, e faltam wtg/idf_sf/buffer_m para a calcular.")
    }
    turbine_idf_coverage_dt <- compute_turbine_idf_coverage(
      wtg, idf_sf, buffer_m = buffer_m, wtg_id_col = wtg_id_col, idf_id_col = idf_id_col
    )
  }

  idf_turbines_dt <- idf_turbines_from_coverage(turbine_idf_coverage_dt, min_pct_coverage = min_pct_coverage)

  n_no_turbine <- data.table::uniqueN(turbine_idf_coverage_dt$idf) - data.table::uniqueN(idf_turbines_dt$idf)
  if (n_no_turbine > 0) {
    message(sprintf(
      "Aviso: %d unidade(s) IDF sem nenhuma turbina >= %d%% de cobertura -- ficam sem verificacao possivel (has_curtailment/has_scada_rpm = FALSE sempre).",
      n_no_turbine, min_pct_coverage
    ))
  }

  list(
    idf_turbines_dt = idf_turbines_dt,
    used_geometric_fallback = TRUE,
    turbine_idf_coverage_dt = turbine_idf_coverage_dt
  )
}


## 2. Curtailments disparados DENTRO do intervalo offline, pelas turbinas
## de idf_turbines_dt -------------------------------------------------------
##
## Unidades IDF ausentes de idf_turbines_dt (sem nenhuma turbina atribuida)
## ficam com n_curtailments_during_offline = 0/has_curtailment = FALSE --
## sem turbina para verificar, nao ha' como confirmar operacionalidade por
## esta via.

check_offline_curtailment_overlap <- function(offline_dt, curtl_dt, idf_turbines_dt) {

  off <- data.table::copy(offline_dt)
  off[, offline_id := .I]

  off_turbines <- merge(off, idf_turbines_dt, by = "idf", allow.cartesian = TRUE)

  if (nrow(off_turbines) == 0L || nrow(curtl_dt) == 0L) {
    off[, `:=`(n_curtailments_during_offline = 0L, has_curtailment = FALSE)]
    off[, offline_id := NULL]
    return(off[])
  }

  # mesmo padrao de interval join ja usado em time_to_first_decline()/
  # time_to_rpm_thresholds() (R/curtailment_response_latency.R,
  # R/curtailment_shutdown_time.R) -- curtl_dt e' "x", off_turbines e' "i".
  # Um curtailment conta como "dentro do intervalo offline" pelo seu
  # instante de start (>= off_start e <= off_end, fronteiras inclusive --
  # mesma logica >=/<= ja usada nesses joins).
  hits <- curtl_dt[
    off_turbines,
    on = .(turbine, start >= off_start, start <= off_end),
    allow.cartesian = TRUE,
    .(offline_id = i.offline_id, curtl_start = x.start)
  ]
  hits <- hits[!is.na(curtl_start)] # sem match -- o join deixa NA em vez de remover a linha

  curtl_counts <- hits[, .(n_curtailments_during_offline = .N), by = offline_id]

  out <- merge(off, curtl_counts, by = "offline_id", all.x = TRUE)
  out[is.na(n_curtailments_during_offline), n_curtailments_during_offline := 0L]
  out[, has_curtailment := n_curtailments_during_offline > 0L]
  out[, offline_id := NULL]

  out[]
}


## 3. Leituras SCADA de RPM DENTRO do intervalo offline, pelas turbinas de
## idf_turbines_dt -----------------------------------------------------------
##
## has_scada_rpm = TRUE so' confirma que HOUVE alguma leitura SCADA (a
## turbina estava a reportar, mecanicamente) -- nao diz nada sobre o valor
## do RPM em si (podia estar parada com rpm=0 e mesmo assim ter leitura).
## Mesma logica de idf_turbines_dt de check_offline_curtailment_overlap()
## acima -- unidades sem turbina atribuida ficam has_scada_rpm = FALSE.

check_offline_scada_presence <- function(offline_dt, scada_dt, idf_turbines_dt) {

  off <- data.table::copy(offline_dt)
  off[, offline_id := .I]

  off_turbines <- merge(off, idf_turbines_dt, by = "idf", allow.cartesian = TRUE)

  rpm_dt <- scada_dt[readingname == "RPM", .(turbine = turbinelabel, datetime)]

  if (nrow(off_turbines) == 0L || nrow(rpm_dt) == 0L) {
    off[, has_scada_rpm := FALSE]
    off[, offline_id := NULL]
    return(off[])
  }

  hits <- rpm_dt[
    off_turbines,
    on = .(turbine, datetime >= off_start, datetime <= off_end),
    allow.cartesian = TRUE,
    .(offline_id = i.offline_id, rpm_time = x.datetime)
  ]
  hits <- hits[!is.na(rpm_time)]

  scada_ids <- unique(hits$offline_id)

  out <- off
  out[, has_scada_rpm := offline_id %in% scada_ids]
  out[, offline_id := NULL]

  out[]
}


## 4. Junta os 2 sinais e classifica cada intervalo offline -----------------
##
## curtl_checked_dt/scada_checked_dt: devolvidos por
## check_offline_curtailment_overlap()/check_offline_scada_presence() sobre
## o MESMO offline_dt (por isso o merge por idf/off_start/off_end e'
## exato, sem folga de tempo a resolver).
##
## classification, por ordem de prioridade (caso real que motivou esta
## distincao, DGY 2026-07: um sub-periodo sem heartbeat NEM SCADA, mas
## com curtailments a disparar -- a evidencia mais forte, o curtailment,
## decide, mesmo quando o SCADA tambem esta em falha).
##
## Valores em INGLES de proposito (nao traduzir) -- este resultado vai
## para xlsx partilhados com a equipa IDF e o cliente dono do parque
## (pedido do Paulo, 2026-09: todos os xlsx produzidos pelo pipeline
## devem ficar integralmente em ingles, ver CLAUDE.md):
##   "IDF unit communication failure" -- has_curtailment = TRUE (a unidade
##     estava mesmo a decidir/atuar; SCADA em falha ao mesmo tempo so'
##     mostra que o SCADA tem a sua propria falha de comunicação
##     independente, nao que o sistema de protecao estivesse parado)
##   "Turbine operational, no detection" -- has_curtailment = FALSE E
##     has_scada_rpm = TRUE (SCADA confirma a turbina a reportar
##     normalmente; sem curtailment so' porque nao passou nenhuma ave)
##   "No evidence (heartbeat and SCADA both missing)" -- nenhum dos 2
##     sinais -- o caso mais ambiguo: pode ser indisponibilidade genuina da
##     protecao, ou so' um gap de dados que afeta heartbeat E SCADA em
##     simultaneo (ex: ficheiros brutos em falta para esse periodo) --
##     requer confirmacao manual, nao assumir nenhuma das duas leituras
##     sozinha

classify_offline_evidence <- function(curtl_checked_dt, scada_checked_dt) {

  combined <- merge(
    curtl_checked_dt, scada_checked_dt,
    by = c("idf", "off_start", "off_end"),
    all = TRUE
  )

  combined[, classification := data.table::fcase(
    has_curtailment,                     "IDF unit communication failure",
    !has_curtailment & has_scada_rpm,     "Turbine operational, no detection",
    default = "No evidence (heartbeat and SCADA both missing)"
  )]

  combined[]
}


## 5. Resumo -- minutos offline por classificacao, por unidade IDF e
## farm-wide ------------------------------------------------------------

summarise_offline_evidence <- function(combined_dt) {

  dt <- data.table::copy(combined_dt)
  dt[, duration_mins := as.numeric(difftime(off_end, off_start, units = "mins"))]

  by_idf <- dt[, .(
    n_intervals = .N,
    total_mins  = round(sum(duration_mins), 1)
  ), by = .(idf, classification)]
  data.table::setorder(by_idf, idf, classification)

  overall <- dt[, .(
    n_intervals = .N,
    total_mins  = round(sum(duration_mins), 1)
  ), by = classification]
  overall[, pct_of_total := round(100 * total_mins / sum(total_mins), 1)]
  data.table::setorder(overall, classification)

  list(by_idf = by_idf[], overall = overall[])
}


## 5b. Disponibilidade "para efeitos de contrato" -- raw vs. net/confirmado
## vs. sem evidencia (pendente revisao manual), farm-wide -- pedido do
## Paulo, 2026-09, apos discussao sobre qual metrica reportar como mais
## fiavel de indisponibilidade quando ha implicacoes contratuais.
##
## 3 numeros, TODOS em % do MESMO denominador (daylight_mins_total,
## minutos totais de monitorizacao diurna) -- nao confundir com
## summarise_offline_evidence()$overall$pct_of_total, que e' % do tempo
## OFFLINE (as 3 classificacoes somam 100% entre si), nao % do tempo de
## monitorizacao total:
##   - raw_offline_pct: qualquer gap de heartbeat >= heartbeat_offline_gap_min,
##     independentemente do que mais se observou -- o numero que um
##     contrato que define disponibilidade so' por presenca de heartbeat
##     mediria.
##   - net_offline_pct: raw_offline_pct MENOS os minutos classificados "IDF
##     unit communication failure" -- gaps em que um curtailment foi mesmo
##     disparado, prova de que o sistema de deteção/decisao estava
##     operacional apesar do heartbeat em si ter falhado a reportar.
##     Exclui-los evita penalizar o sistema por uma falha de comunicação
##     que na realidade nao teve.
##   - no_evidence_pct: minutos classificados "No evidence (heartbeat and
##     SCADA both missing)" -- o caso genuinamente ambiguo. NAO e'
##     subtraido de net_offline_pct (fica incluido nele) -- e' mostrado a
##     parte por precisar de revisao manual caso a caso, nao por dever ser
##     resolvido automaticamente numa direcao ou noutra.
##
## availability_overall: summarise_availability_overall(idf_availability_summary$by_idf),
## R/monthly_technical_summary.R -- daylight_mins_total, offline_mins_total, offline_pct
## (funcao generica, apesar do nome do ficheiro -- reutilizada tambem pelo
## relatorio anual so' para este calculo).
## offline_evidence_overall: summarise_offline_evidence(combined_dt)$overall
## (funcao 5 acima) -- classification, n_intervals, total_mins, pct_of_total

summarise_net_availability <- function(availability_overall, offline_evidence_overall) {

  get_mins <- function(cls) {
    v <- offline_evidence_overall[classification == cls, total_mins]
    if (length(v) == 0L) 0 else sum(v)
  }

  daylight_mins_total <- availability_overall$daylight_mins_total
  raw_offline_mins    <- availability_overall$offline_mins_total
  comm_failure_mins   <- get_mins("IDF unit communication failure")
  no_evidence_mins    <- get_mins("No evidence (heartbeat and SCADA both missing)")
  # raw_offline_mins (summarise_availability(), sempre so' diurno) e
  # comm_failure_mins (summarise_offline_evidence(), agora tambem so'
  # diurno desde que o caller recorta offline_dt com
  # clip_offline_intervals_to_daylight() ANTES de classify_offline_evidence())
  # vem de 2 pipelines de recorte diurno SEPARADOS -- pmax(0, ...) e' so'
  # uma salvaguarda aritmetica contra um residuo de arredondamento entre os
  # 2, NAO o mecanismo principal para evitar net_offline_pct negativo (ver
  # a nota completa em clip_offline_intervals_to_daylight(),
  # R/availability_daylight.R, sobre o caso real que motivou isto).
  net_offline_mins    <- max(0, raw_offline_mins - comm_failure_mins)

  pct_of_daylight <- function(mins) {
    if (daylight_mins_total == 0) NA_real_ else round(100 * mins / daylight_mins_total, 1)
  }

  data.table::data.table(
    daylight_mins_total = daylight_mins_total,
    raw_offline_mins     = raw_offline_mins,
    raw_offline_pct      = pct_of_daylight(raw_offline_mins),
    comm_failure_mins    = comm_failure_mins,
    comm_failure_pct     = pct_of_daylight(comm_failure_mins),
    net_offline_mins     = net_offline_mins,
    net_offline_pct      = pct_of_daylight(net_offline_mins),
    no_evidence_mins     = no_evidence_mins,
    no_evidence_pct      = pct_of_daylight(no_evidence_mins)
  )
}


## 6. Nota metodologica -- a incluir no relatorio (mensal e anual) quando
## esta analise for formalizada numa secção propria (pedido do Paulo,
## 2026-09) -- em ingles, ja pronta para reutilizar no texto do relatorio
## ou como sheet de metodologia num xlsx exploratorio como este.
##
## So' se aplica quando idf_turbines_dt veio do FALLBACK geometrico
## (idf_turbines_from_coverage(), secção 1b acima) -- nao da matriz manual
## (secção 1a, a fonte preferida, que reflete atribuicao operacional real
## e nao tem esta limitacao). O caller (ex: explore_offline_curtailment_check.R)
## decide qual das duas fontes foi usada e so' inclui esta nota nesse caso.

OFFLINE_EVIDENCE_SCOPE_NOTE <- paste(
  "No operational turbine<->IDF-unit matrix (Primary IDF) was identified for this analysis, so",
  "turbine coverage per IDF unit was instead estimated from 2D geometric proximity -- every",
  "turbine whose detection buffer overlaps that IDF unit's buffer by at least the configured",
  "threshold, based only on turbine and IDF-unit positions available from the IDF portal export,",
  "not a verified camera orientation or detection range. Where two IDF units have overlapping",
  "detection ranges, evidence attributed to one unit could in principle reflect a neighbouring",
  "unit's turbine instead. This is a fallback only: wherever an operational matrix is available,",
  "it is used in preference to this geometric estimate."
)
