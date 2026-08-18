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

##
## Especies distintas por track_id (riqueza), sequencia e entropia ----
##

track_species_summary <- function(track_dt) {

  dt <- track_dt[!is.na(spec), .(track_id, spec)]

  out <- dt[, {
    sp <- unique(spec) # ja em ordem cronologica -- track_dt vem ordenado por track_id, timestamp
    n_spec <- .N
    p_i <- as.numeric(table(spec)) / n_spec
    .(
      n_species      = length(sp),
      species        = paste(sp, collapse = ", "),
      first_species  = sp[1],
      last_species   = sp[length(sp)],
      shannon_entropy = -sum(p_i * log(p_i))
    )
  }, by = track_id]

  out[]
}


## Sumario da distribuicao de n_species por track (histograma) + taxa de
## transicao (>=2 especies) + sumario da entropia
summarise_species_richness <- function(richness_dt) {

  if (nrow(richness_dt) == 0L) {
    return(list(
      by_n_species = data.table::data.table(n_species = integer(), n_tracks = integer(), pct = numeric()),
      rate = data.table::data.table(
        total_tracks = integer(), tracks_with_transition = integer(),
        id_transition_rate = numeric(), id_transition_rate_pct = numeric()
      ),
      entropy = data.table::data.table(n_tracks = integer(), mean_entropy = numeric(), median_entropy = numeric())
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
    n_tracks       = .N,
    mean_entropy   = mean(shannon_entropy, na.rm = TRUE),
    median_entropy = median(shannon_entropy, na.rm = TRUE)
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


## Distribuicao do indice de entropia de Shannon por track -- H=0 (so' 1
## especie, estavel), H>0 tanto maior quanto mais alternancia entre especies
plot_entropy_hist <- function(richness_dt) {
  ggplot2::ggplot(richness_dt, ggplot2::aes(shannon_entropy)) +
    ggplot2::geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black") +
    ggplot2::labs(x = "Shannon entropy index", y = "Number of tracks") +
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
