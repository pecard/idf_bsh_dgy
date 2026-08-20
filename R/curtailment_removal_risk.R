##
## Risco de remover uma especie da estrategia de curtailment (ex: Kestrel) --
## quantas curtailments disparadas ENQUANTO classificadas como essa especie
## seriam removidas, e para as que ficarem em tracks que mais tarde
## revelaram ser de especie prioritaria, qual seria o custo em tempo/
## distancia de so' proteger a ave a partir dessa reclassificacao.
##
## Contexto (Paulo, 2026-08): discussao com o cliente sobre remover Kestrel
## da lista de especies que disparam curtailment (nao-prioritaria, maior
## volume de curtailments "desnecessarios" em termos de producao -- ver
## R/id_transitions.R risk_direction == "P_to_NP_unnecessary_curtailment" e
## a matriz de confusao em summarise_species_confusion()). O cliente precisa
## de demonstrar aos financiadores (lenders) que isto tem impacto marginal,
## se algum, no risco de OUTRAS especies prioritarias (ex: Saker-Falcon,
## Eagles) que possam ter sido momentaneamente mal classificadas como
## Kestrel no momento do disparo.
##
## Metodologia acordada com o Paulo (2026-08):
##   1. Unidade de analise = CADA EVENTO de curtailment disparado com
##      species == removed_species (nao deduplicado por track -- um track
##      pode ter varios eventos, cada um e' uma decisao real de parar a
##      turbina).
##   2. Para cada evento, procura-se QUALQUER deteccao de especie
##      prioritaria em QUALQUER ponto do track DEPOIS do disparo desse
##      evento (abordagem conservadora -- nao se exige que seja a
##      classificacao final do track, so' que apareca alguma prioritaria a
##      seguir).
##   3. Gap simples de tempo/distancia entre o momento real (disparo
##      enquanto removed_species) e o momento contrafactual (1ª deteccao
##      prioritaria a seguir) -- SEM depender de SCADA/metodologia KNE
##      (R/curtailment_safe_distance.R), para funcionar farm-wide e nao so'
##      nas turbinas com SCADA. Se quisermos a versao KNE mais tarde, e'
##      uma camada adicional sobre este resultado, nao substitui este.
##
## dist_gap_m = x2d_at_curtailment - dist_at_next_priority
##   positivo = a ave estava MAIS PERTO da turbina no momento em que so' a'
##     ter-se-ia protegido sob a nova politica do que estava no momento em
##     que foi de facto protegida sob a politica atual (perda de margem) --
##     e' a direcao relevante para risco.
##   negativo (ou proximo de 0) = sem perda de margem relevante -- a
##     reclassificacao prioritaria aconteceu tao perto (ou mais longe) do
##     momento/posicao do curtailment real, que a protecao efetiva nao
##     mudaria muito removendo o disparo por removed_species.
##
## Se next_priority_ts for NA: este evento e' o UNICO sinal de protecao que
## o track alguma vez teve -- removê-lo deixaria esta ave sem qualquer
## curtailment sob a nova politica. E' o caso mais critico, sinalizado
## separadamente (nao se confunde com "gap pequeno").
##
## Depende de: data.table, ggplot2
##
## Uso:
##   source("R/curtailment_removal_risk.R")
##
##   removal_dt <- evaluate_curtailment_removal_risk(curtl_dt, track_dt, prioritysp, removed_species = "Kestrel")
##   removal_summary <- summarise_curtailment_removal_risk(removal_dt)
##   print_curtailment_removal_risk_summary(removal_summary, "Kestrel")
##
##   p1 <- plot_removal_time_gap(removal_dt)
##   p2 <- plot_removal_dist_gap(removal_dt, threshold_m = track_proximity_threshold_m)
##
##   # linhas completas de curtl_dt para inspecao/defesa de um caso concreto
##   golden_eagle_cases <- curtailment_removal_case_detail(removal_dt, curtl_dt, "Golden-Eagle")
##

## 1. Evento a evento: distancia no momento do disparo (removed_species) e
##    a 1ª deteccao prioritaria a seguir no mesmo track (se houver) ----

evaluate_curtailment_removal_risk <- function(curtl_dt, track_dt, prioritysp,
                                              removed_species = "Kestrel", max_trigger_match_sec = 30) {

  curtl_dt <- data.table::as.data.table(curtl_dt) # curtl_dt_unfilt pode nao ser data.table (ver R/curtailment_cluster_patterns.R)

  removed_curtl <- curtl_dt[species == removed_species, .(track_id, turbine, start)]
  removed_curtl[, event_id := .I]

  empty <- data.table::data.table(
    event_id = integer(), track_id = character(), turbine = character(), curtailment_start = as.POSIXct(character()),
    x2d_at_curtailment = numeric(), next_priority_ts = as.POSIXct(character()),
    next_priority_species = character(), dist_at_next_priority = numeric(),
    time_gap_sec = numeric(), dist_gap_m = numeric(), protected_by_reclassification = logical()
  )
  if (nrow(removed_curtl) == 0L) return(empty)

  track_pts <- track_dt[!is.na(spec), .(track_id, timestamp, spec, dist3d)]
  data.table::setorder(track_pts, track_id, timestamp)

  # distancia no momento do disparo -- ponto de track mais proximo no tempo
  # (antes OU depois, tolerancia max_trigger_match_sec -- mesma logica de
  # match_nearest_rpm() em R/curtailment_response.R), nao so "antes" porque
  # o intervalo entre pontos do track pode ser maior que o gap real
  at_trigger <- track_pts[
    removed_curtl,
    on = .(track_id, timestamp = start),
    roll = "nearest",
    .(
      event_id            = i.event_id,
      track_id            = i.track_id,
      turbine             = i.turbine,
      curtailment_start   = i.start,
      matched_track_time  = x.timestamp,
      x2d_at_curtailment  = x.dist3d
    )
  ]
  at_trigger[, trigger_match_gap_sec := as.numeric(abs(difftime(matched_track_time, curtailment_start, units = "secs")))]
  at_trigger[!is.na(trigger_match_gap_sec) & trigger_match_gap_sec > max_trigger_match_sec, x2d_at_curtailment := NA_real_]
  at_trigger[, c("matched_track_time", "trigger_match_gap_sec") := NULL]

  # 1ª deteccao de especie prioritaria DEPOIS do disparo, no mesmo track --
  # nao exige que seja a classificacao final (pedido do Paulo: abordagem
  # conservadora, conta qualquer prioritaria a seguir, nao so' a ultima)
  priority_pts <- track_dt[!is.na(spec) & spec %in% prioritysp, .(track_id, timestamp, spec, dist3d)]
  data.table::setorder(priority_pts, track_id, timestamp)

  next_priority <- priority_pts[
    removed_curtl,
    on = .(track_id, timestamp > start),
    mult = "first",
    .(
      event_id                = i.event_id,
      next_priority_ts        = x.timestamp,
      next_priority_species   = x.spec,
      dist_at_next_priority   = x.dist3d
    )
  ]

  out <- merge(at_trigger, next_priority, by = "event_id", all.x = TRUE)

  out[, time_gap_sec := as.numeric(difftime(next_priority_ts, curtailment_start, units = "secs"))]
  out[, dist_gap_m := x2d_at_curtailment - dist_at_next_priority]
  out[, protected_by_reclassification := !is.na(next_priority_ts)]

  data.table::setcolorder(out, c("event_id", "track_id", "turbine", "curtailment_start"))
  data.table::setorder(out, event_id)
  out[]
}


## 2. Sumario para relatorio -- panorama geral, quebra por especie
##    prioritaria seguinte, e estatisticas do gap (so' onde ha' proteccao
##    de substituicao -- gap nao esta definido quando NAO ha')
summarise_curtailment_removal_risk <- function(removal_dt) {

  n_total <- nrow(removal_dt)

  # protected_by_reclassification e' TRUE sse existe alguma deteccao
  # prioritaria DEPOIS do disparo -- por construcao, isso e' exatamente
  # "sob a nova politica, uma curtailment de substituicao teria disparado
  # nessa reclassificacao" (a assuncao contrafactual simples acordada com
  # o Paulo). Nao ha' um 3º balde "com prioritaria a seguir mas mesmo assim
  # sem proteccao" -- so' ha 2 grupos: n_with_later_priority (precisa de
  # escrutinio, ver gap_stats) e o complementar (n_total - n_with_later_priority,
  # genuinamente nunca-prioritario, remover sem custo biologico plausivel).
  overview <- data.table::data.table(
    n_events_removed      = n_total,
    n_tracks_affected     = data.table::uniqueN(removal_dt$track_id),
    n_with_later_priority = sum(removal_dt$protected_by_reclassification, na.rm = TRUE)
  )
  overview[, n_never_priority      := n_events_removed - n_with_later_priority]
  overview[, pct_with_later_priority := round(100 * n_with_later_priority / n_total, 1)]
  overview[, pct_never_priority      := round(100 * n_never_priority / n_total, 1)]

  protected <- removal_dt[protected_by_reclassification == TRUE]

  by_next_priority_species <- if (nrow(protected) > 0L) {
    tmp <- protected[, .(
      n_events         = .N,
      mean_time_gap_sec   = round(mean(time_gap_sec, na.rm = TRUE), 1),
      median_time_gap_sec = round(median(time_gap_sec, na.rm = TRUE), 1),
      mean_dist_gap_m     = round(mean(dist_gap_m, na.rm = TRUE), 1),
      median_dist_gap_m   = round(median(dist_gap_m, na.rm = TRUE), 1)
    ), by = next_priority_species]
    data.table::setorder(tmp, -n_events)
    tmp[]
  } else {
    data.table::data.table(
      next_priority_species = character(), n_events = integer(),
      mean_time_gap_sec = numeric(), median_time_gap_sec = numeric(),
      mean_dist_gap_m = numeric(), median_dist_gap_m = numeric()
    )
  }

  gap_stats <- if (nrow(protected) > 0L) {
    protected[, .(
      n_events            = .N,
      mean_time_gap_sec   = round(mean(time_gap_sec, na.rm = TRUE), 1),
      median_time_gap_sec = round(median(time_gap_sec, na.rm = TRUE), 1),
      max_time_gap_sec    = round(max(time_gap_sec, na.rm = TRUE), 1),
      mean_dist_gap_m     = round(mean(dist_gap_m, na.rm = TRUE), 1),
      median_dist_gap_m   = round(median(dist_gap_m, na.rm = TRUE), 1),
      max_dist_gap_m      = round(max(dist_gap_m, na.rm = TRUE), 1),
      pct_dist_gap_gt_0   = round(100 * sum(dist_gap_m > 0, na.rm = TRUE) / .N, 1)
    )]
  } else {
    data.table::data.table(
      n_events = integer(), mean_time_gap_sec = numeric(), median_time_gap_sec = numeric(),
      max_time_gap_sec = numeric(), mean_dist_gap_m = numeric(), median_dist_gap_m = numeric(),
      max_dist_gap_m = numeric(), pct_dist_gap_gt_0 = numeric()
    )
  }

  list(overview = overview[], by_next_priority_species = by_next_priority_species, gap_stats = gap_stats)
}


## 3. Resumo em texto para consola ----

print_curtailment_removal_risk_summary <- function(removal_summary, removed_species = "Kestrel") {

  ov <- removal_summary$overview

  cat(sprintf("\n===== Curtailment removal risk: %s =====\n", removed_species))
  cat(sprintf(
    "\n%d curtailment events triggered while classified as %s, on %d distinct tracks.\n",
    ov$n_events_removed, removed_species, ov$n_tracks_affected
  ))
  cat(sprintf(
    "  %d (%s%%) later showed a priority species on the same track -- these are the events needing scrutiny.\n",
    ov$n_with_later_priority, format(ov$pct_with_later_priority, nsmall = 1)
  ))
  cat(sprintf(
    "  %d (%s%%) NEVER showed a priority species after -- genuinely non-priority, safe to remove, no biological cost.\n",
    ov$n_never_priority, format(ov$pct_never_priority, nsmall = 1)
  ))

  gs <- removal_summary$gap_stats
  if (nrow(gs) > 0L) {
    cat(sprintf(
      "\nAmong events still protected by a later reclassification (n=%d):\n  time gap: median %ss, mean %ss, max %ss\n  distance gap: median %sm, mean %sm, max %sm (%s%% show a positive gap -- bird closer under the new policy)\n",
      gs$n_events,
      format(gs$median_time_gap_sec, nsmall = 1), format(gs$mean_time_gap_sec, nsmall = 1), format(gs$max_time_gap_sec, nsmall = 1),
      format(gs$median_dist_gap_m, nsmall = 1), format(gs$mean_dist_gap_m, nsmall = 1), format(gs$max_dist_gap_m, nsmall = 1),
      format(gs$pct_dist_gap_gt_0, nsmall = 1)
    ))
  }

  bs <- removal_summary$by_next_priority_species
  if (nrow(bs) > 0L) {
    cat("\nBy the species that showed up afterwards:\n")
    for (i in seq_len(nrow(bs))) {
      cat(sprintf(
        "  %-25s %4d events -- median time gap %ss, median dist gap %sm\n",
        bs$next_priority_species[i], bs$n_events[i],
        format(bs$median_time_gap_sec[i], nsmall = 1), format(bs$median_dist_gap_m[i], nsmall = 1)
      ))
    }
  }

  invisible(NULL)
}


## 4. Plots -- distribuicao dos gaps, so' entre eventos com proteccao de
##    substituicao (gap indefinido nos outros) ----

plot_removal_time_gap <- function(removal_dt, xlim_max = 300) {

  dt <- removal_dt[protected_by_reclassification == TRUE & !is.na(time_gap_sec)]

  ggplot2::ggplot(dt, ggplot2::aes(time_gap_sec)) +
    ggplot2::geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
    ggplot2::coord_cartesian(xlim = c(NA, xlim_max)) +
    ggplot2::labs(
      x = "Time from removed-species curtailment to next priority-species detection (s)",
      y = "Number of events",
      title = "Time gap if curtailment moved to the priority reclassification"
    ) +
    ggplot2::theme_minimal()
}


plot_removal_dist_gap <- function(removal_dt, threshold_m = 100, xlim_max = 500) {

  dt <- removal_dt[protected_by_reclassification == TRUE & !is.na(dist_gap_m)]

  ggplot2::ggplot(dt, ggplot2::aes(dist_gap_m)) +
    ggplot2::geom_histogram(binwidth = 20, fill = "steelblue", color = "black") +
    ggplot2::geom_vline(xintercept = 0, color = "grey40", linetype = "solid", linewidth = 0.5) +
    ggplot2::geom_vline(xintercept = threshold_m, color = "firebrick", linetype = "dashed", linewidth = 0.7) +
    ggplot2::coord_cartesian(xlim = c(NA, xlim_max)) +
    ggplot2::labs(
      x = sprintf("Distance margin lost (m, positive = closer under new policy) -- proximity risk threshold = %dm", threshold_m),
      y = "Number of events",
      title = "Distance gap if curtailment moved to the priority reclassification"
    ) +
    ggplot2::theme_minimal()
}


## 5. Detalhe caso a caso -- linhas COMPLETAS de curtl_dt (todas as colunas
##    originais, nao so' o subconjunto usado em evaluate_curtailment_removal_risk())
##    para os eventos filtrados, tipicamente por next_priority_species --
##    mesmo raciocinio de id_transition_late_cases() em R/id_transitions.R:
##    ir da contagem agregada para os casos concretos, para inspecao manual
##    ou para defender um caso especifico perante o cliente/financiadores.
##    next_priority_species_sel = NULL devolve TODOS os eventos com
##    protected_by_reclassification == TRUE (nao so' um subconjunto)
curtailment_removal_case_detail <- function(removal_dt, curtl_dt, next_priority_species_sel = NULL) {

  curtl_dt <- data.table::as.data.table(curtl_dt)

  cases <- removal_dt[protected_by_reclassification == TRUE]
  if (!is.null(next_priority_species_sel)) {
    cases <- cases[next_priority_species %in% next_priority_species_sel]
  }
  if (nrow(cases) == 0L) return(curtl_dt[0])

  out <- merge(
    curtl_dt, cases[, .(track_id, curtailment_start, next_priority_ts, next_priority_species,
                         x2d_at_curtailment, dist_at_next_priority, time_gap_sec, dist_gap_m)],
    by.x = c("track_id", "start"), by.y = c("track_id", "curtailment_start")
  )
  data.table::setorder(out, -time_gap_sec)
  out[]
}
