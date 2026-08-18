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
##   - NP -> P (nao-prioritaria -> prioritaria): NAO foi ativado
##     curtailment por o track parecer nao-prioritario, mas a classificacao
##     final e' prioritaria -- individuo de especie sensivel pode ter ficado
##     sem proteção (risco biologico). Esta direcao nao estava coberta no
##     script original (so' via P->NP), foi adicionada por pedido do Paulo
##     (2026-08).
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
##   risk_dt <- classify_id_transition_risk(richness_dt, curtl_dt, prioritysp)
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


##
## Risco de transicao P<->NP para tracks multi-ID (>=2 especies) ----
##

classify_id_transition_risk <- function(richness_dt, curtl_dt, prioritysp) {

  multi_dt <- richness_dt[n_species >= 2]

  if (nrow(multi_dt) == 0L) {
    return(multi_dt[, `:=`(
      triggered_curtailment = logical(), last_is_priority = logical(),
      first_is_priority = logical(), risk_direction = character()
    )][])
  }

  curtailed_ids <- unique(as.character(curtl_dt$track_id))

  out <- data.table::copy(multi_dt)
  out[, triggered_curtailment := as.character(track_id) %in% curtailed_ids]
  out[, last_is_priority  := last_species %in% prioritysp]
  out[, first_is_priority := first_species %in% prioritysp]

  out[, risk_direction := data.table::fcase(
    triggered_curtailment  & !last_is_priority,  "P_to_NP_unnecessary_curtailment",
    !triggered_curtailment &  last_is_priority,  "NP_to_P_protection_gap",
    default = "no_risk"
  )]

  out[]
}


## Sumarios para relatorio: custo de producao (P->NP, com contagem de
## curtailments efetivamente disparados por essas tracks) e risco biologico
## (NP->P, tracks nunca curtailed apesar de terminarem em especie
## prioritaria -- nao ha "curtailments" a contar aqui, por definicao)
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

  list(by_direction = by_direction[], pnp_curtailments = pnp_curtailments[])
}
