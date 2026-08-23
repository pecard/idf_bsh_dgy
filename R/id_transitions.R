##
## Transicoes de especie dentro do mesmo track_id (species ID transitions)
##
## Migrado/estendido de scripts_IDF/ID_transitions.R e
## scripts_IDF/curtailments_ID_transitions.R.
##
## O algoritmo do IDF pode mudar a classificacao de especie de um track ao
## longo do proprio track (mais deteccoes => mais confianca). Isso importa
## para curtailments porque a decisao de parar (ou nao) a turbina e' tomada
## com a classificacao *nesse momento*, que pode nao ser a classificacao
## final do track:
##
##   - P -> NP (prioritaria -> nao-prioritaria): foi ativado curtailment a
##     meio do track por ele parecer ser de especie prioritaria, mas a
##     classificacao final e' nao-prioritaria -- turbina parada
##     desnecessariamente (custo de producao).
##   - NP -> P (nao-prioritaria -> prioritaria): a classificacao final e'
##     prioritaria -- individuo de especie sensivel pode ter ficado sem
##     protecao adequada (risco biologico). Esta direcao nao estava coberta
##     no script original (so' via P->NP), foi adicionada por pedido do
##     Paulo (2026-08), com 4 niveis de gravidade:
##       - NP_to_P_no_curtailment_near -- NUNCA disparou curtailment, E a ave
##         ja estava dentro de late_dist_threshold_m quando foi reclassificada
##         -- gap de proteção real. O caso mais grave.
##       - NP_to_P_no_curtailment_far -- NUNCA disparou curtailment, mas a
##         ave estava longe quando foi reclassificada -- nao e' alarmante em
##         si (nunca chegou a justificar curtailment por proximidade), so'
##         descritivo. Separado do "_near" em 2026-08 depois de, nos dados
##         reais de Bash, ~99% dos "no_curtailment" carem neste balde --
##         sem esta distincao a contagem agregada sugeria um problema de
##         proteccao ~85x maior do que o que a distancia justifica (29.837
##         vs 354 tracks).
##       - NP_to_P_late_curtailment -- disparou curtailment, mas tarde
##         demais por pelo menos 1 de 2 criterios avaliados em paralelo (ver
##         late_by_time/late_by_dist abaixo) -- protecao pode nao ter
##         chegado a tempo apesar de "ter havido curtailment".
##       - no_risk -- disparou curtailment a tempo (ou o track nunca foi
##         reclassificado como prioritario).
##
## Criterios de "tarde demais" para o curtailment, calculados os DOIS em
## paralelo (pedido do Paulo, 2026-08) para comparar e depois eliminar um
## deles por eficiencia:
##   - late_by_time -- gap (segundos) entre o 1º registo do track ja
##     classificado como prioritario (first_priority_ts) e o inicio do
##     curtailment, acima de late_time_threshold_sec
##     (id_transition_late_time_sec no userSettings). Limiar arbitrario, sem
##     base biologica direta.
##   - late_by_dist -- distancia da ave a turbina mais proxima (dist3d, tal
##     como calculado pelo proprio algoritmo do IdentiFlight) no momento de
##     first_priority_ts, dentro de late_dist_threshold_m
##     (track_proximity_threshold_m no userSettings, o mesmo limiar usado na
##     investigacao forense de fatalidades -- ver R/fatality_track_investigation.R
##     e CLAUDE.md). Nao depende de quando o curtailment disparou -- so' de a
##     ave ja estar perto demais QUANDO foi reclassificada, o que por si so
##     ja pode nao deixar margem para uma paragem eficaz. ATENCAO: usa
##     dist3d (NearestTurbineDistance3d do proprio IdentiFlight), nao a
##     distancia recalculada a partir do shapefile de turbinas como em
##     fatality_track_investigation.R -- pode atribuir mal pontos de
##     fronteira (mesma ressalva ja documentada nesse modulo); aceitavel
##     aqui por ser um rastreio farm-wide, nao uma analise forense por
##     incidente especifico.
##
## Aproximacao usada (tal como no script original): usa-se a ultima especie
## classificada no track (last_species) como proxy da classificacao final,
## em vez da sequencia completa -- e' a especie que o algoritmo "ficou a
## achar" depois de toda a informacao do track. So' se olha para tracks com
## >=2 especies distintas (multi-ID); tracks com 1 unica especie nunca
## mudaram de classificacao, logo nao tem risco de transicao.
##
## Depende de: data.table, ggplot2
##
## Uso:
##   source("R/id_transitions.R")
##
##   richness_dt <- track_species_summary(track_dt)
##   richness_summary <- summarise_species_richness(richness_dt)
##
##   p_hist <- plot_species_richness_hist(richness_dt)
##   p_entropy <- plot_entropy_hist(richness_dt)
##
##   risk_dt <- classify_id_transition_risk(
##     richness_dt, track_dt, curtl_dt, prioritysp,
##     late_time_threshold_sec = id_transition_late_time_sec,
##     late_dist_threshold_m = track_proximity_threshold_m
##   )
##   risk_summary <- summarise_id_transition_risk(risk_dt, curtl_dt)
##
##   # vista caso a caso dos "NP_to_P_late_curtailment" (both > dist_only > time_only)
##   late_cases_dt <- id_transition_late_cases(risk_dt, curtl_dt)
##
##   # sensibilidade dos 2 limiares -- despistar se algum e' pouco efetivo
##   time_sens_dt <- id_transition_late_time_sensitivity(risk_dt)
##   dist_sens_dt <- id_transition_late_dist_sensitivity(risk_dt)
##   p_late_time <- plot_late_time_distribution(risk_dt, threshold_sec = id_transition_late_time_sec)
##   p_late_dist <- plot_late_dist_distribution(risk_dt, threshold_m = track_proximity_threshold_m)
##
##   # matriz de confusao de especies -- que pares aparecem no mesmo track,
##   # em geral e so' para tracks com curtailment -- ver secção dedicada
##   # mais abaixo no ficheiro
##   confusion_summary <- summarise_species_confusion(track_dt, richness_dt, curtl_dt, "Kestrel")
##   print_species_confusion_summary(confusion_summary, "Kestrel")
##

##
## Especies distintas por track_id (riqueza), sequencia e entropia ----
##

track_species_summary <- function(track_dt) {

  dt <- track_dt[!is.na(spec), .(track_id, spec)]

  out <- dt[, {
    sp <- unique(spec) # ja em ordem cronologica -- track_dt vem ordenado por track_id, timestamp
    n_spec <- .N
    p_i <- as.numeric(table(spec)) / n_spec
    n_sp <- length(sp)
    h <- -sum(p_i * log(p_i))
    .(
      n_species             = n_sp,
      species               = paste(sp, collapse = ", "),
      first_species         = sp[1],
      last_species          = sp[length(sp)],
      shannon_entropy       = h,
      # Pielou's evenness (H / H_max, H_max = log(n_species)) -- normaliza a
      # entropia para 0-100%, independente de n_species. Sem isto, H "em
      # bruto" nao e' comparavel entre tracks com numeros de especies
      # diferentes (o maximo teorico de H e' log(n_species), nao um valor
      # fixo) -- foi por isso que o Paulo achou o indice "abstrato, sem
      # referencia numerica" (2026-08). 0% = track estavel (1 unica
      # especie, sem alternancia -- H_max tambem e' 0 aqui, 0/0 definido
      # como 0%, nao NaN); 100% = alternancia maximamente equilibrada entre
      # as n_species do track (o pior caso possivel PARA ESSE track).
      shannon_evenness_pct = if (n_sp > 1) round(100 * h / log(n_sp), 1) else 0
    )
  }, by = track_id]

  out[]
}


## Sumario da distribuicao de n_species por track (histograma) + taxa de
## transicao (>=2 especies) + sumario da entropia (bruta e normalizada)
summarise_species_richness <- function(richness_dt) {

  if (nrow(richness_dt) == 0L) {
    return(list(
      by_n_species = data.table::data.table(n_species = integer(), n_tracks = integer(), pct = numeric()),
      rate = data.table::data.table(
        total_tracks = integer(), tracks_with_transition = integer(),
        id_transition_rate = numeric(), id_transition_rate_pct = numeric()
      ),
      entropy = data.table::data.table(
        n_tracks = integer(), mean_entropy = numeric(), median_entropy = numeric(),
        mean_evenness_pct = numeric(), median_evenness_pct = numeric()
      )
    ))
  }

  by_n_species <- richness_dt[, .(n_tracks = .N), by = n_species]
  data.table::setorder(by_n_species, n_species)
  by_n_species[, pct := round(100 * n_tracks / sum(n_tracks), 1)]

  rate <- richness_dt[, .(
    total_tracks           = .N,
    tracks_with_transition = sum(n_species >= 2)
  )][, `:=`(
    id_transition_rate     = tracks_with_transition / total_tracks,
    id_transition_rate_pct = round(100 * tracks_with_transition / total_tracks, 1)
  )]

  entropy <- richness_dt[, .(
    n_tracks             = .N,
    mean_entropy         = mean(shannon_entropy, na.rm = TRUE),
    median_entropy       = median(shannon_entropy, na.rm = TRUE),
    mean_evenness_pct    = round(mean(shannon_evenness_pct, na.rm = TRUE), 1),
    median_evenness_pct  = round(median(shannon_evenness_pct, na.rm = TRUE), 1)
  )]

  list(by_n_species = by_n_species[], rate = rate[], entropy = entropy[])
}


## Histograma do numero de especies distintas por track (barras, uma por
## valor inteiro de n_species)
plot_species_richness_hist <- function(richness_dt) {
  ggplot2::ggplot(richness_dt, ggplot2::aes(n_species)) +
    ggplot2::geom_bar() +
    ggplot2::labs(x = "Num species/single track", y = "Number of tracks") +
    ggplot2::theme_minimal()
}


## Distribuicao da entropia de Shannon por track, NORMALIZADA (Pielou's
## evenness, shannon_evenness_pct -- ver track_species_summary()) em vez do
## indice bruto -- este e' comparavel entre tracks com numeros de especies
## diferentes e tem um teto fixo e interpretavel (100%), o que o indice
## bruto nao tem (o maximo teorico de H e' log(n_species), diferente por
## track). 0% = estavel (1 unica especie); 100% = alternancia maximamente
## equilibrada entre as especies desse track.
plot_entropy_hist <- function(richness_dt) {
  ggplot2::ggplot(richness_dt, ggplot2::aes(shannon_evenness_pct)) +
    ggplot2::geom_histogram(binwidth = 5, fill = "steelblue", color = "black", boundary = 0) +
    ggplot2::scale_x_continuous(limits = c(0, 100)) +
    ggplot2::labs(x = "Shannon evenness (%) -- 0% stable, 100% maximally mixed for its own species count",
                  y = "Number of tracks") +
    ggplot2::theme_minimal()
}


## Timestamp e distancia (dist3d) do 1º registo de cada track ja
## classificado como especie prioritaria -- so' faz sentido para tracks que
## em algum momento tiveram uma leitura prioritaria (inner: tracks 100%
## nao-prioritarios nao aparecem aqui, e nao precisam: last_is_priority sera'
## FALSE e a fcase abaixo nunca chega a olhar para estes campos)
track_first_priority_state <- function(track_dt, prioritysp) {

  dt <- track_dt[!is.na(spec) & spec %in% prioritysp, .(track_id, timestamp, dist3d)]
  if (nrow(dt) == 0L) {
    return(data.table::data.table(
      track_id = dt$track_id, first_priority_ts = as.POSIXct(character()), dist_at_first_priority = numeric()
    ))
  }
  data.table::setorder(dt, track_id, timestamp)

  dt[, .(
    first_priority_ts      = data.table::first(timestamp),
    dist_at_first_priority = data.table::first(dist3d)
  ), by = track_id]
}


##
## Risco de transicao P<->NP para tracks multi-ID (>=2 especies) ----
##

classify_id_transition_risk <- function(richness_dt, track_dt, curtl_dt, prioritysp,
                                        late_time_threshold_sec = 50,
                                        late_dist_threshold_m = 100) {

  multi_dt <- richness_dt[n_species >= 2]

  if (nrow(multi_dt) == 0L) {
    return(multi_dt[, `:=`(
      triggered_curtailment = logical(), last_is_priority = logical(), first_is_priority = logical(),
      first_priority_ts = as.POSIXct(character()), first_curtailment_start = as.POSIXct(character()),
      dist_at_first_priority = numeric(), time_to_curtailment_after_priority_sec = numeric(),
      late_by_time = logical(), late_by_dist = logical(), risk_direction = character()
    )][])
  }

  # 1º curtailment por track (um track pode ter varios) -- track_id_chr
  # porque curtl_dt$track_id e' character (ver read_curtailments.R), o
  # track_id de track_dt/richness_dt normalmente nao e' -- comparar sem
  # converter e' o bug recorrente deste projeto (ver CLAUDE.md/PROJECT_CHECKPOINT.md)
  curtl_first <- curtl_dt[, .(first_curtailment_start = min(start)), by = track_id]
  curtl_first[, track_id_chr := as.character(track_id)]

  priority_state <- track_first_priority_state(track_dt, prioritysp)

  out <- data.table::copy(multi_dt)
  out[, track_id_chr := as.character(track_id)]
  out[, triggered_curtailment := track_id_chr %in% curtl_first$track_id_chr]
  out[, last_is_priority  := last_species %in% prioritysp]
  out[, first_is_priority := first_species %in% prioritysp]

  out <- merge(out, priority_state, by = "track_id", all.x = TRUE)
  out <- merge(out, curtl_first[, .(track_id_chr, first_curtailment_start)], by = "track_id_chr", all.x = TRUE)

  out[, time_to_curtailment_after_priority_sec :=
    as.numeric(difftime(first_curtailment_start, first_priority_ts, units = "secs"))]

  out[, late_by_time := triggered_curtailment & !is.na(time_to_curtailment_after_priority_sec) &
        time_to_curtailment_after_priority_sec > late_time_threshold_sec]
  out[, late_by_dist := last_is_priority & !is.na(dist_at_first_priority) &
        dist_at_first_priority <= late_dist_threshold_m]

  out[, risk_direction := data.table::fcase(
    triggered_curtailment  & !last_is_priority,                                     "P_to_NP_unnecessary_curtailment",
    last_is_priority       & !triggered_curtailment & late_by_dist,                  "NP_to_P_no_curtailment_near",
    last_is_priority       & !triggered_curtailment & !late_by_dist,                 "NP_to_P_no_curtailment_far",
    last_is_priority       & triggered_curtailment  & (late_by_time | late_by_dist), "NP_to_P_late_curtailment",
    default = "no_risk"
  )]

  out[, track_id_chr := NULL]
  data.table::setcolorder(out, c("track_id", "n_species", "species", "first_species", "last_species"))
  out[]
}


## Sumarios para relatorio: custo de producao (P->NP, com contagem de
## curtailments efetivamente disparados por essas tracks), risco biologico
## (NP->P, com os 4 niveis de gravidade -- ver R/id_transitions.R topo,
## incluindo a separacao near/far do "sem curtailment" que se revelou
## essencial nos dados reais de Bash) e a comparacao late_by_time vs
## late_by_dist pedida pelo Paulo, para decidir qual dos 2 criterios manter
## no pipeline final
summarise_id_transition_risk <- function(risk_dt, curtl_dt) {

  empty_tracks <- data.table::data.table(
    n_multi_id_tracks = integer(), n_tracks = integer(), pct_of_multi_id = numeric()
  )
  if (nrow(risk_dt) == 0L) {
    return(list(
      by_direction  = empty_tracks,
      pnp_curtailments = data.table::data.table(
        total_curtailments = integer(), curtailments_from_multi_id_tracks = integer(),
        curtailments_due_to_p_to_np = integer(), pct_of_total = numeric()
      ),
      late_criteria_compare = data.table::data.table(
        late_by_time = logical(), late_by_dist = logical(), n_tracks = integer()
      )
    ))
  }

  n_multi <- nrow(risk_dt)

  by_direction <- risk_dt[, .(n_tracks = .N), by = risk_direction]
  by_direction[, n_multi_id_tracks := n_multi]
  by_direction[, pct_of_multi_id := round(100 * n_tracks / n_multi_id_tracks, 1)]
  data.table::setcolorder(by_direction, c("risk_direction", "n_tracks", "n_multi_id_tracks", "pct_of_multi_id"))
  data.table::setorder(by_direction, -n_tracks)

  # Curtailments individuais (um track pode disparar varios) originados por
  # tracks P->NP -- quanto de "producao perdida" isto representa em numero
  # de eventos, nao so' de tracks
  pnp_track_ids <- as.character(risk_dt[risk_direction == "P_to_NP_unnecessary_curtailment", track_id])
  multi_id_ids  <- as.character(risk_dt$track_id)

  # vetor local, sem tocar em curtl_dt (data.table muta por referencia com
  # := -- nao queremos alterar o objeto do chamador aqui)
  curtl_track_id_chr <- as.character(curtl_dt$track_id)

  n_total_curt   <- nrow(curtl_dt)
  n_multi_curt   <- sum(curtl_track_id_chr %in% multi_id_ids)
  n_pnp_curt     <- sum(curtl_track_id_chr %in% pnp_track_ids)

  pnp_curtailments <- data.table::data.table(
    total_curtailments                = n_total_curt,
    curtailments_from_multi_id_tracks = n_multi_curt,
    curtailments_due_to_p_to_np       = n_pnp_curt,
    pct_of_total                      = round(100 * n_pnp_curt / n_total_curt, 1)
  )

  # Comparacao dos 2 criterios de "tarde demais", so' entre tracks onde os
  # dois fazem sentido em simultaneo (reclassificados para prioritaria E com
  # curtailment disparado) -- concordam? um sinaliza mais casos que o outro?
  # base para decidir qual manter (pedido do Paulo, 2026-08)
  late_base <- risk_dt[last_is_priority == TRUE & triggered_curtailment == TRUE]
  late_criteria_compare <- late_base[, .(n_tracks = .N), by = .(late_by_time, late_by_dist)]
  data.table::setorder(late_criteria_compare, -late_by_time, -late_by_dist)

  list(
    by_direction          = by_direction[],
    pnp_curtailments      = pnp_curtailments[],
    late_criteria_compare = late_criteria_compare[]
  )
}


##
## Vista detalhada dos casos "NP_to_P_late_curtailment", para inspecao caso
## a caso (pedido do Paulo, 2026-08 -- passo 1 de "rever os 146 casos ->
## decidir late_by_time vs late_by_dist -> relatorio mensal").
##
## Ordenados em 3 grupos, do mais para o menos preocupante, e dentro de cada
## grupo do caso mais extremo para o menos extremo:
##   1. "both"      -- sinalizado pelos 2 criterios; ordenado por distancia
##                      ascendente (mais perto primeiro)
##   2. "dist_only"  -- so' late_by_dist; ordenado por distancia ascendente
##   3. "time_only"  -- so' late_by_time; ordenado por atraso descendente
##                      (mais tempo primeiro)
##
## Inclui a turbina do 1º curtailment do track (contexto para cruzar com o
## portal IdentiFlight) -- um track pode ter varios curtailments; fica a do
## primeiro, o mesmo que os campos de tempo desta tabela descrevem.
##
id_transition_late_cases <- function(risk_dt, curtl_dt) {

  cols <- c(
    "track_id", "n_species", "species", "first_species", "last_species",
    "first_priority_ts", "dist_at_first_priority",
    "first_curtailment_start", "time_to_curtailment_after_priority_sec",
    "late_by_time", "late_by_dist"
  )
  out <- risk_dt[risk_direction == "NP_to_P_late_curtailment", ..cols]

  if (nrow(out) == 0L) {
    out[, late_severity := character()]
    out[, turbine := character()]
    return(out[])
  }

  out[, late_severity := data.table::fcase(
    late_by_time & late_by_dist, "both",
    late_by_dist,                "dist_only",
    late_by_time,                "time_only"
  )]

  # filtra curtl_dt aos tracks late ANTES do group-by -- curtl_dt real tem
  # centenas de milhares de linhas, a maioria irrelevante aqui (so' os
  # ~146 tracks "late" importam)
  curtl_late <- curtl_dt[as.character(track_id) %in% as.character(out$track_id)]
  curtl_first_turbine <- curtl_late[, .SD[which.min(start)], by = track_id][, .(track_id, turbine)]
  curtl_first_turbine[, track_id_chr := as.character(track_id)]
  out[, track_id_chr := as.character(track_id)]
  out <- merge(out, curtl_first_turbine[, .(track_id_chr, turbine)], by = "track_id_chr", all.x = TRUE)
  out[, track_id_chr := NULL]

  both_group <- out[late_severity == "both"]
  data.table::setorder(both_group, dist_at_first_priority)

  dist_group <- out[late_severity == "dist_only"]
  data.table::setorder(dist_group, dist_at_first_priority)

  time_group <- out[late_severity == "time_only"]
  data.table::setorder(time_group, -time_to_curtailment_after_priority_sec)

  out <- data.table::rbindlist(list(both_group, dist_group, time_group))
  data.table::setcolorder(out, c("track_id", "turbine", "late_severity"))
  out[]
}


##
## Sensibilidade dos limiares late_by_time / late_by_dist ----
##
## Paulo decidiu manter os 2 criterios (2026-08), mas pediu forma de
## despistar se algum esta pouco efetivo/irrelevante -- mesmo raciocinio ja
## usado no projeto para outros limiares (ver Parte B de
## tests/test_curtailment_response_classify.R): varia-se o limiar candidato
## a candidato e ve-se como a contagem sinalizada muda. Um criterio "pouco
## efetivo" mostra a mesma contagem (ou quase) em toda a gama plausivel --
## sinal de que o valor concreto do limiar pouco importa, porque a
## distribuicao nao tem massa relevante perto dele (ou esta toda de um
## lado). Um criterio a "fazer trabalho real" mostra uma subida clara na
## contagem a medida que o limiar afrouxa, com inclinacao mais acentuada
## perto do valor atual do projeto.
##
## NAO recalculam risk_dt -- os campos continuos (time_to_curtailment_after_priority_sec,
## dist_at_first_priority) ja estao lá, so se reaplica o corte a varios
## limiares candidatos, mais barato que rechamar classify_id_transition_risk().
##

## Sensibilidade de late_by_time a id_transition_late_time_sec -- base =
## tracks reclassificados para prioritaria E com curtailment disparado (so'
## onde o "atraso em tempo" faz sentido)
id_transition_late_time_sensitivity <- function(risk_dt, thresholds_sec = c(10, 20, 30, 50, 75, 100, 150, 200, 300)) {

  base <- risk_dt[
    last_is_priority == TRUE & triggered_curtailment == TRUE &
      !is.na(time_to_curtailment_after_priority_sec)
  ]
  n_base <- nrow(base)

  out <- data.table::rbindlist(lapply(thresholds_sec, function(th) {
    n_flag <- base[, sum(time_to_curtailment_after_priority_sec > th)]
    data.table::data.table(
      threshold_sec = th,
      n_flagged     = n_flag,
      n_base        = n_base,
      pct_flagged   = if (n_base > 0) round(100 * n_flag / n_base, 1) else NA_real_
    )
  }))
  out[]
}


## Sensibilidade de late_by_dist a track_proximity_threshold_m -- base =
## todos os tracks reclassificados para prioritaria (com OU sem
## curtailment) -- e' o mesmo campo (dist_at_first_priority) que decide
## near/far no "sem curtailment" e o "_dist" no "late_curtailment"
id_transition_late_dist_sensitivity <- function(risk_dt, thresholds_m = c(25, 50, 75, 100, 150, 200, 300, 500)) {

  base <- risk_dt[last_is_priority == TRUE & !is.na(dist_at_first_priority)]
  n_base <- nrow(base)

  out <- data.table::rbindlist(lapply(thresholds_m, function(th) {
    n_flag <- base[, sum(dist_at_first_priority <= th)]
    data.table::data.table(
      threshold_m = th,
      n_flagged   = n_flag,
      n_base      = n_base,
      pct_flagged = if (n_base > 0) round(100 * n_flag / n_base, 1) else NA_real_
    )
  }))
  out[]
}


## Distribuicao de time_to_curtailment_after_priority_sec (base igual a
## id_transition_late_time_sensitivity()), com linha vertical no limiar
## atual -- para ver visualmente se o corte cai num vale natural da
## distribuicao ou a meio de uma nuvem continua
plot_late_time_distribution <- function(risk_dt, threshold_sec = 50, xlim_max = 300) {

  base <- risk_dt[
    last_is_priority == TRUE & triggered_curtailment == TRUE &
      !is.na(time_to_curtailment_after_priority_sec)
  ]

  ggplot2::ggplot(base, ggplot2::aes(time_to_curtailment_after_priority_sec)) +
    ggplot2::geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
    ggplot2::geom_vline(xintercept = threshold_sec, color = "firebrick", linetype = "dashed", linewidth = 0.7) +
    ggplot2::coord_cartesian(xlim = c(NA, xlim_max)) +
    ggplot2::labs(
      x = sprintf("Time from 1st priority detection to curtailment start (s) -- current threshold = %ds", threshold_sec),
      y = "Number of tracks"
    ) +
    ggplot2::theme_minimal()
}


## Distribuicao de dist_at_first_priority (base igual a
## id_transition_late_dist_sensitivity()), com linha vertical no limiar
## atual
plot_late_dist_distribution <- function(risk_dt, threshold_m = 100, xlim_max = 1000) {

  base <- risk_dt[last_is_priority == TRUE & !is.na(dist_at_first_priority)]

  ggplot2::ggplot(base, ggplot2::aes(dist_at_first_priority)) +
    ggplot2::geom_histogram(binwidth = 20, fill = "steelblue", color = "black") +
    ggplot2::geom_vline(xintercept = threshold_m, color = "firebrick", linetype = "dashed", linewidth = 0.7) +
    ggplot2::coord_cartesian(xlim = c(NA, xlim_max)) +
    ggplot2::labs(
      x = sprintf("Distance to nearest turbine at 1st priority detection (m) -- current threshold = %dm", threshold_m),
      y = "Number of tracks"
    ) +
    ggplot2::theme_minimal()
}


##
## Species confusion matrix -- que pares de especies aparecem no MESMO track
## (evidencia direta de confusao do algoritmo, nao so' "mudou de opiniao"
## generico) -- pedido do Paulo (2026-08) para comparar confusoes
## especificas, com foco inicial em Kestrel. species_of_interest e'
## parametro em todas as funcoes abaixo (nao hardcoded), reutilizavel para
## qualquer outra especie mais tarde.
##
## Reutiliza id_richness_dt (track_species_summary()) so' para a taxa
## pura/confusa; a matriz de pares em si recalcula a sequencia diretamente
## de track_dt (id_richness_dt$species e' uma string concatenada, nao um
## list-column -- reprocessar o texto seria mais fragil, ex: "Eagle" e'
## substring de "Eagle-Unknown"/"Eagle-Sp" no vocabulario deste projeto).
##
## "Em geral" vs "para curtailments em particular": a MESMA funcao aceita
## track_ids opcional -- NULL = todos os tracks farm-wide, ou
## unique(curtl_dt$track_id) = so' tracks que dispararam pelo menos 1
## curtailment. Sao 2 chamadas da mesma funcao, nao 2 funcoes separadas.
##
## Depende de: data.table, ggplot2
##
## Uso:
##   source("R/id_transitions.R")
##
##   confusion_summary <- summarise_species_confusion(track_dt, id_richness_dt, curtl_dt, "Kestrel")
##   confusion_summary$rate_compare
##   confusion_summary$confusion_general
##   confusion_summary$confusion_curtailments
##
##   p1 <- plot_species_confusion_involving(confusion_summary$confusion_general, "Kestrel")
##   p2 <- plot_species_confusion_involving(confusion_summary$confusion_curtailments, "Kestrel", title = "Kestrel confusion -- curtailment tracks")
##

## 1. Matriz de pares de especies co-ocorrendo no mesmo track (multi-ID) --
##    track_ids = NULL usa TODOS os tracks; passar unique(curtl_dt$track_id)
##    para restringir a tracks que dispararam curtailment
species_confusion_pairs <- function(track_dt, track_ids = NULL) {

  dt <- track_dt[!is.na(spec), .(track_id, spec)]
  if (!is.null(track_ids)) dt <- dt[as.character(track_id) %in% as.character(track_ids)]

  empty <- data.table::data.table(
    species_a = character(), species_b = character(),
    n_tracks = integer(), pct_of_multi_id_tracks = numeric()
  )
  if (nrow(dt) == 0L) return(empty)

  pairs <- dt[, {
    sp <- unique(spec)
    if (length(sp) >= 2) {
      cmb <- utils::combn(sort(sp), 2)
      .(species_a = cmb[1, ], species_b = cmb[2, ])
    } else {
      .(species_a = character(), species_b = character())
    }
  }, by = track_id]

  if (nrow(pairs) == 0L) return(empty)

  n_multi_id_tracks <- data.table::uniqueN(pairs$track_id)

  out <- pairs[, .(n_tracks = .N), by = .(species_a, species_b)]
  out[, pct_of_multi_id_tracks := round(100 * n_tracks / n_multi_id_tracks, 1)]
  data.table::setorder(out, -n_tracks)
  out[]
}


## 2. Vista de UMA especie -- filtra a matriz de pares para as linhas que
##    envolvem species_of_interest, normalizando para "other_species" (em
##    vez de ter de olhar para species_a/species_b em qualquer ordem)
species_confusion_involving <- function(confusion_dt, species_of_interest) {

  empty <- data.table::data.table(
    species = character(), other_species = character(),
    n_tracks = integer(), pct_of_multi_id_tracks = numeric()
  )
  if (nrow(confusion_dt) == 0L) return(empty)

  dt <- confusion_dt[species_a == species_of_interest | species_b == species_of_interest]
  if (nrow(dt) == 0L) return(empty)

  dt <- data.table::copy(dt)
  dt[, other_species := data.table::fifelse(species_a == species_of_interest, species_b, species_a)]
  out <- dt[, .(species = species_of_interest, other_species, n_tracks, pct_of_multi_id_tracks)]
  data.table::setorder(out, -n_tracks)
  out[]
}


## 3. Taxa de confusao para UMA especie -- de entre os tracks onde
##    species_of_interest apareceu em ALGUM momento da sequencia, que
##    fraccao ficou "pura" (n_species==1, nunca mudou) vs "confusa"
##    (n_species>=2, species_of_interest partilhou o track com outra
##    especie). track_ids = NULL (farm-wide) ou
##    unique(curtl_dt$track_id) (so' tracks com curtailment)
species_confusion_rate <- function(track_dt, richness_dt, species_of_interest, track_ids = NULL) {

  tdt <- track_dt[!is.na(spec)]
  if (!is.null(track_ids)) tdt <- tdt[as.character(track_id) %in% as.character(track_ids)]

  base_ids <- unique(tdt[spec == species_of_interest, as.character(track_id)])
  richness_sub <- richness_dt[as.character(track_id) %in% base_ids]

  n_total    <- nrow(richness_sub)
  n_pure     <- if (n_total > 0L) richness_sub[, sum(n_species == 1)] else 0L
  n_confused <- n_total - n_pure

  data.table::data.table(
    species           = species_of_interest,
    n_tracks_total    = n_total,
    n_tracks_pure     = n_pure,
    n_tracks_confused = n_confused,
    pct_confused      = if (n_total > 0L) round(100 * n_confused / n_total, 1) else NA_real_
  )
}


## 4. Sumario completo para 1 OU VARIAS especies de interesse -- junta as 3
##    funcoes acima, em geral e para curtailments, pronto a exportar/reportar.
##    species_of_interest aceita um vetor (ex: c("Egyptian-Vulture",
##    "Steppe-Eagle")) -- cada especie e' tratada de forma independente (nao
##    como um grupo fundido), e as linhas de todas ficam juntas na mesma
##    tabela, distinguidas pela coluna `species`. Um vetor de tamanho 1
##    funciona exatamente como antes (comportamento antigo preservado).
summarise_species_confusion <- function(track_dt, richness_dt, curtl_dt, species_of_interest) {

  curtl_track_ids <- unique(as.character(curtl_dt$track_id))

  general_pairs <- species_confusion_pairs(track_dt)
  curtl_pairs   <- species_confusion_pairs(track_dt, track_ids = curtl_track_ids)

  confusion_general <- data.table::rbindlist(lapply(
    species_of_interest, function(sp) species_confusion_involving(general_pairs, sp)
  ))
  confusion_curtailments <- data.table::rbindlist(lapply(
    species_of_interest, function(sp) species_confusion_involving(curtl_pairs, sp)
  ))
  if (nrow(confusion_general) > 0L) data.table::setorder(confusion_general, -n_tracks)
  if (nrow(confusion_curtailments) > 0L) data.table::setorder(confusion_curtailments, -n_tracks)

  general_rate <- data.table::rbindlist(lapply(
    species_of_interest, function(sp) species_confusion_rate(track_dt, richness_dt, sp)
  ))
  curtl_rate <- data.table::rbindlist(lapply(
    species_of_interest, function(sp) species_confusion_rate(track_dt, richness_dt, sp, track_ids = curtl_track_ids)
  ))

  # scope adicionado com := (nao data.table(scope=, existing_dt) -- esse
  # construtor nao faz splice das colunas do 2º argumento, ficaria uma
  # coluna aninhada em vez de scope+species+... lado a lado)
  general_rate[, scope := "all_tracks"]
  curtl_rate[, scope := "curtailment_tracks"]

  rate_compare <- data.table::rbindlist(list(general_rate, curtl_rate))
  data.table::setcolorder(rate_compare, c("scope", "species"))

  list(
    rate_compare            = rate_compare[],
    confusion_general        = confusion_general,
    confusion_curtailments   = confusion_curtailments
  )
}


## 5. Barras -- nº de tracks partilhados com cada outra especie, para uma
##    das 2 vistas (confusion_general OU confusion_curtailments) de
##    summarise_species_confusion()
plot_species_confusion_involving <- function(confusion_involving_dt, species_of_interest, title = NULL) {

  if (is.null(title)) title <- sprintf("Species confused with %s", species_of_interest)

  dt <- data.table::copy(confusion_involving_dt)
  data.table::setorder(dt, -n_tracks)
  dt[, other_species := factor(other_species, levels = other_species)]

  ggplot2::ggplot(dt, ggplot2::aes(x = other_species, y = n_tracks)) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::labs(x = "Other species in the same track", y = "Number of tracks", title = title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}


## 6. Resumo em texto para consola -- leitura rapida do essencial sem abrir
##    o xlsx (rate geral vs curtailments + top confusoes das 2 vistas).
##    Nao devolve nada de util (invisible) -- e' so' para o efeito no ecra.
print_species_confusion_summary <- function(confusion_summary, species_of_interest) {

  rc          <- confusion_summary$rate_compare
  general_row <- rc[scope == "all_tracks"]
  curtl_row   <- rc[scope == "curtailment_tracks"]

  cat(sprintf("\n===== Species confusion summary: %s =====\n", species_of_interest))

  cat(sprintf(
    "\nAll tracks:         %d tracks ever classified as %s -- %d pure, %d confused (%s%% confused)\n",
    general_row$n_tracks_total, species_of_interest,
    general_row$n_tracks_pure, general_row$n_tracks_confused,
    format(general_row$pct_confused, nsmall = 1)
  ))
  cat(sprintf(
    "Curtailment tracks: %d tracks ever classified as %s -- %d pure, %d confused (%s%% confused)\n",
    curtl_row$n_tracks_total, species_of_interest,
    curtl_row$n_tracks_pure, curtl_row$n_tracks_confused,
    format(curtl_row$pct_confused, nsmall = 1)
  ))

  print_pairs <- function(dt, label) {
    cat(sprintf("\n%s:\n", label))
    if (nrow(dt) == 0L) {
      cat("  (no confusion recorded)\n")
      return(invisible(NULL))
    }
    for (i in seq_len(nrow(dt))) {
      cat(sprintf(
        "  %-20s %4d tracks (%s%% of multi-ID tracks in this scope)\n",
        dt$other_species[i], dt$n_tracks[i], format(dt$pct_of_multi_id_tracks[i], nsmall = 1)
      ))
    }
    invisible(NULL)
  }

  print_pairs(confusion_summary$confusion_general, "Confused with -- all tracks")
  print_pairs(confusion_summary$confusion_curtailments, "Confused with -- curtailment tracks")

  invisible(NULL)
}
