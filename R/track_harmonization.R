##
## Harmonizacao/reconciliacao de tracks fragmentados (EXPLORATORIO -- nao faz
## parte do pipeline de producao, nao e' chamado por IDF_analysis.R nem por
## IDF_monthly_report.R)
##
## Motivacao (Paulo, 2026-08): o IdentiFlight por vezes regista o MESMO
## individuo como varios track_id distintos. Identificaram-se 3 padroes
## visuais concretos (dados BSH, 2026-08-07):
##
##   1. "handoff" -- um track termina e outro comeca muito perto no tempo E
##      no espaco (a ave sai da deteccao de uma unidade/faixa e entra
##      noutra, ou o proprio algoritmo troca de ID a meio do voo -- o mesmo
##      fenomeno ja documentado em R/track_min_individuals.R linhas
##      293-310). Os 2 tracks NAO se sobrepoem no tempo.
##
##   2. "duplicate" -- 2 unidades IDF diferentes registam o MESMO individuo
##      em SIMULTANEO (tracks sobrepostos no tempo), mas por erro de
##      calibracao entre as 2 unidades ficam desviados espacialmente um do
##      outro (tipicamente poucos metros, nao os ~50m do handoff). Testar
##      isto por "distancia minima em algum instante" (como
##      count_min_individuals_per_bin(), R/track_min_individuals.R) daria
##      demasiados falsos positivos -- 2 aves DIFERENTES podem passar a
##      <50m uma da outra por um instante sem serem duplicados. O sinal real
##      e' os 2 tracks manterem-se proximos ao longo de TODA a janela em que
##      coexistem, nao so' num ponto.
##
##   3. Combinacao das 2 -- 2 cadeias de handoff (uma por unidade IDF),
##      ligadas entre si por um par "duplicate" -- ver
##      build_reconciliation_groups() abaixo, que junta os 2 tipos de aresta
##      no MESMO grafo de componentes conexas, exatamente para capturar isto.
##
## Parametros acordados com o Paulo (2026-08): handoff testado a 15-30s /
## 50m; duplicado testado por FRACCAO ALTA (nao 100%) dos instantes
## sobrepostos dentro de uma distancia fixa (max_duplicate_dist_m), para
## tolerar um ponto solto de calibracao mais ruidoso sem invalidar o par
## inteiro. Os valores por omissao abaixo sao um ponto de partida, nao
## valores validados -- a rever depois de ver casos reais (ver
## explore_track_harmonization.R).
##
## Depende de: data.table, R/track_min_individuals.R (.uf_components -- usa
## a MESMA implementacao de componentes conexas, nao redefinida aqui)
##
## Uso:
##   source("R/track_min_individuals.R")  # .uf_components()
##   source("R/track_harmonization.R")
##
##   handoff_edges_dt   <- find_handoff_edges(track_dt, species = "Steppe-Eagle")
##   duplicate_edges_dt <- find_duplicate_edges(track_dt, species = "Steppe-Eagle")
##
##   rec <- build_reconciliation_groups(track_dt, "Steppe-Eagle", handoff_edges_dt, duplicate_edges_dt)
##   rec$groups  # track_id -> synth_track_id
##   rec$edges   # arestas usadas (com edge_type)
##
##   summarise_reconciliation(rec$groups)
##
##   # inspecao visual de UM grupo especifico, antes de confiar na fusao
##   inspect_reconciliation_group(track_dt, rec$groups, rec$edges, "SYN_Steppe-Eagle_003")
##
##   synth_dt <- stitch_synthetic_tracks(track_dt, "Steppe-Eagle", rec$groups, duplicate_edges_dt)
##


##
## 1. Arestas "handoff" -- fim de um track perto (tempo+espaco) do inicio de
##    outro, tracks SEM sobreposicao temporal ----
##
## Usa foverlaps() (interval tree) em vez de um cross-join O(n^2) -- esta
## funcao pode ter de correr sobre uma epoca inteira, com milhares de tracks
## por especie.
##

find_handoff_edges <- function(track_dt, species, time_window_sec = 30, max_dist_m = 50) {

  dt <- track_dt[spec == species, .(track_id, timestamp, utm_x, utm_y)]
  data.table::setorder(dt, track_id, timestamp)

  empty <- data.table::data.table(
    track_id_a = dt$track_id[0], track_id_b = dt$track_id[0],
    gap_sec = numeric(), dist_m = numeric()
  )
  if (data.table::uniqueN(dt$track_id) < 2L) return(empty)

  ends <- dt[, .(
    first_ts = data.table::first(timestamp), first_x = data.table::first(utm_x), first_y = data.table::first(utm_y),
    last_ts  = data.table::last(timestamp),  last_x  = data.table::last(utm_x),  last_y  = data.table::last(utm_y)
  ), by = track_id]

  # colunas de intervalo para foverlaps() em segundos-desde-epoch (double
  # puro) -- os timestamps reais (first_ts/last_ts, nomes unicos em cada
  # tabela, sem colisao) ficam guardados a parte para reconstruir gap_sec
  closing <- ends[, .(
    closing_track_id = track_id,
    wstart = as.numeric(last_ts), wend = as.numeric(last_ts) + time_window_sec,
    last_ts, cx = last_x, cy = last_y
  )]
  opening <- ends[, .(
    opening_track_id = track_id,
    wstart = as.numeric(first_ts), wend = as.numeric(first_ts),
    first_ts, ox = first_x, oy = first_y
  )]
  data.table::setkey(closing, wstart, wend)

  cand <- data.table::foverlaps(opening, closing, type = "any", nomatch = NULL)
  cand <- cand[closing_track_id != opening_track_id]
  if (nrow(cand) == 0L) return(empty)

  cand[, `:=`(
    dist_m  = sqrt((cx - ox)^2 + (cy - oy)^2),
    gap_sec = as.numeric(difftime(first_ts, last_ts, units = "secs"))
  )]

  out <- cand[dist_m <= max_dist_m, .(track_id_a = closing_track_id, track_id_b = opening_track_id, gap_sec, dist_m)]
  data.table::setorder(out, track_id_a, gap_sec)
  out[]
}


##
## 2. Arestas "duplicate" -- 2 unidades IDF diferentes, tracks sobrepostos no
##    tempo, proximos ao longo de uma FRACCAO ALTA (nao 100%) da janela de
##    sobreposicao ----
##
## 1º passo (foverlaps, barato): so' intervalos [start,end] sobrepostos.
## 2º passo (so' nos candidatos sobreviventes, mais caro): reamostra os 2
## tracks para uma grelha temporal comum (uniao dos timestamps de ambos
## dentro da sobreposicao, interpolacao linear -- stats::approx()) e mede a
## fraccao de instantes com distancia <= max_duplicate_dist_m.
##

find_duplicate_edges <- function(track_dt, species, max_duplicate_dist_m = 20,
                                  min_overlap_frac = 0.8, min_overlap_sec = 10) {

  dt <- track_dt[spec == species, .(track_id, timestamp, utm_x, utm_y, idf)]
  data.table::setorder(dt, track_id, timestamp)
  data.table::setkey(dt, track_id)

  empty <- data.table::data.table(
    track_id_a = dt$track_id[0], track_id_b = dt$track_id[0],
    overlap_sec = numeric(), frac_within_dist = numeric()
  )
  if (data.table::uniqueN(dt$track_id) < 2L) return(empty)

  spans <- dt[, .(
    start = min(timestamp), end = max(timestamp),
    idf_units = paste(sort(unique(idf)), collapse = ",")
  ), by = track_id]

  # assuncao: cada track_id e' registado por UMA so' unidade IDF (a troca de
  # unidade produz um NOVO track_id, nao um idf misto dentro do mesmo
  # track) -- o teste de duplicado so' faz sentido comparando unidades
  # DIFERENTES; alerta em vez de dar silenciosamente resultados errados se a
  # assuncao nao se verificar nos dados reais
  if (any(grepl(",", spans$idf_units, fixed = TRUE))) {
    warning("find_duplicate_edges(): ha track_id com mais de 1 unidade IDF -- assuncao de unidade unica por track violada, resultados podem nao ser fiaveis.")
  }

  a <- data.table::copy(spans)
  data.table::setnames(a, c("track_id", "start", "end", "idf_units"), c("track_id_a", "astart", "aend", "idf_a"))
  a[, `:=`(wstart = as.numeric(astart), wend = as.numeric(aend))]

  b <- data.table::copy(spans)
  data.table::setnames(b, c("track_id", "start", "end", "idf_units"), c("track_id_b", "bstart", "bend", "idf_b"))
  b[, `:=`(wstart = as.numeric(bstart), wend = as.numeric(bend))]

  data.table::setkey(a, wstart, wend)
  cand <- data.table::foverlaps(b, a, type = "any", nomatch = NULL)

  # cada par sobreposto aparece 2x (A-B e B-A), e cada track sobrepoe-se
  # trivialmente a si propria -- fica so com 1 linha por par nao-trivial
  cand <- cand[track_id_a < track_id_b & idf_a != idf_b]
  if (nrow(cand) == 0L) return(empty)

  edges <- cand[, {
    pa <- dt[.(track_id_a)]
    pb <- dt[.(track_id_b)]
    t0 <- max(min(pa$timestamp), min(pb$timestamp))
    t1 <- min(max(pa$timestamp), max(pb$timestamp))
    overlap_sec <- as.numeric(difftime(t1, t0, units = "secs"))

    if (overlap_sec < min_overlap_sec) {
      list(overlap_sec = overlap_sec, frac_within_dist = NA_real_)
    } else {
      grid <- sort(unique(c(
        as.numeric(pa[timestamp >= t0 & timestamp <= t1, timestamp]),
        as.numeric(pb[timestamp >= t0 & timestamp <= t1, timestamp])
      )))
      ax <- stats::approx(as.numeric(pa$timestamp), pa$utm_x, xout = grid, rule = 2)$y
      ay <- stats::approx(as.numeric(pa$timestamp), pa$utm_y, xout = grid, rule = 2)$y
      bx <- stats::approx(as.numeric(pb$timestamp), pb$utm_x, xout = grid, rule = 2)$y
      by_ <- stats::approx(as.numeric(pb$timestamp), pb$utm_y, xout = grid, rule = 2)$y
      d <- sqrt((ax - bx)^2 + (ay - by_)^2)
      list(overlap_sec = overlap_sec, frac_within_dist = mean(d <= max_duplicate_dist_m))
    }
  }, by = .(track_id_a, track_id_b)]

  out <- edges[overlap_sec >= min_overlap_sec & frac_within_dist >= min_overlap_frac]
  data.table::setorder(out, track_id_a, track_id_b)
  out[]
}


##
## 3. Componentes conexas -- junta os 2 tipos de aresta no MESMO grafo (caso
##    3 do topo do ficheiro: cadeias de handoff ligadas por um par
##    duplicate) e atribui um synth_track_id por componente, incluindo
##    tracks isolados (grupo de 1) ----
##

build_reconciliation_groups <- function(track_dt, species, handoff_edges_dt, duplicate_edges_dt) {

  track_ids <- track_dt[spec == species, sort(unique(track_id))]
  n <- length(track_ids)
  idx_of <- stats::setNames(seq_len(n), as.character(track_ids))

  edge_pieces <- list()
  if (!is.null(handoff_edges_dt) && nrow(handoff_edges_dt) > 0L) {
    edge_pieces[[length(edge_pieces) + 1L]] <-
      handoff_edges_dt[, .(track_id_a, track_id_b, edge_type = "handoff")]
  }
  if (!is.null(duplicate_edges_dt) && nrow(duplicate_edges_dt) > 0L) {
    edge_pieces[[length(edge_pieces) + 1L]] <-
      duplicate_edges_dt[, .(track_id_a, track_id_b, edge_type = "duplicate")]
  }
  edges_dt <- if (length(edge_pieces) > 0L) {
    data.table::rbindlist(edge_pieces)
  } else {
    data.table::data.table(track_id_a = character(), track_id_b = character(), edge_type = character())
  }

  roots <- if (nrow(edges_dt) == 0L) {
    seq_len(n)
  } else {
    edges_idx <- data.table::data.table(
      i = idx_of[as.character(edges_dt$track_id_a)],
      j = idx_of[as.character(edges_dt$track_id_b)]
    )
    .uf_components(n, edges_idx)
  }

  groups <- data.table::data.table(track_id = track_ids, group_id = roots)
  groups[, synth_track_id := sprintf("SYN_%s_%03d", species, .GRP), by = group_id]
  groups[, group_id := NULL]

  list(groups = groups[], edges = edges_dt[])
}


## Sumario rapido -- quantos tracks originais foram fundidos em quantos
## sinteticos, e o maior grupo encontrado (para despistar rapidamente se os
## limiares estao a fundir tracks a mais)
summarise_reconciliation <- function(groups_dt) {

  by_group <- groups_dt[, .(n_orig_tracks = .N), by = synth_track_id]

  data.table::data.table(
    n_synth_tracks         = nrow(by_group),
    n_orig_tracks_total    = sum(by_group$n_orig_tracks),
    n_merged_synth_tracks  = sum(by_group$n_orig_tracks > 1),
    max_tracks_in_1_group  = max(by_group$n_orig_tracks)
  )
}


##
## 4. Inspecao caso a caso de UM grupo -- validar manualmente antes de
##    confiar na fusao automatica (mesmo espirito de
##    inspect_min_individuals_bin(), R/track_min_individuals.R) ----
##

inspect_reconciliation_group <- function(track_dt, groups_dt, edges_dt, target_synth_id) {

  ids <- groups_dt[synth_track_id == target_synth_id, track_id]

  pts <- track_dt[track_id %in% ids, .(track_id, timestamp, idf, utm_x, utm_y)]

  track_summary <- pts[, .(
    n_points    = .N,
    idf_units   = paste(sort(unique(idf)), collapse = ", "),
    first_time  = min(timestamp),
    last_time   = max(timestamp),
    utm_x_range = sprintf("%.0f - %.0f", min(utm_x), max(utm_x)),
    utm_y_range = sprintf("%.0f - %.0f", min(utm_y), max(utm_y))
  ), by = track_id]
  data.table::setorder(track_summary, first_time)

  own_edges <- edges_dt[track_id_a %in% ids & track_id_b %in% ids]

  list(tracks = track_summary[], edges = own_edges[])
}


##
## 5. Reconstrucao geometrica -- concatena por ordem temporal os pontos de
##    cada grupo; segmentos ligados por uma aresta "duplicate" (que POR
##    DEFINICAO se sobrepoem no tempo) sao fundidos num so ponto por
##    instante (media das posicoes interpoladas), nao concatenados (isso
##    daria um zig-zag entre as 2 unidades). Segmentos "handoff" nao se
##    sobrepoem por construcao, por isso so' precisam de concatenacao ----
##
## Simplificacao aceite nesta 1ª versao: sobreposicoes de 3+ unidades no
## mesmo grupo sao fundidas par a par, sequencialmente (nao como uma fusao
## conjunta das 3+) -- ok para uma primeira exploracao, a rever se se
## mostrar relevante nos dados reais.
##

stitch_synthetic_tracks <- function(track_dt, species, groups_dt, duplicate_edges_dt) {

  dt <- track_dt[spec == species, .(track_id, timestamp, utm_x, utm_y)]
  dt <- merge(dt, groups_dt[, .(track_id, synth_track_id)], by = "track_id")

  dup_pairs <- if (!is.null(duplicate_edges_dt) && nrow(duplicate_edges_dt) > 0L) {
    duplicate_edges_dt[, .(track_id_a, track_id_b)]
  } else {
    data.table::data.table(track_id_a = dt$track_id[0], track_id_b = dt$track_id[0])
  }

  out <- dt[, .merge_group_points(.SD, dup_pairs), by = synth_track_id]
  data.table::setorder(out, synth_track_id, timestamp)
  out[]
}


.merge_group_points <- function(pts, dup_pairs) {

  ids_here <- unique(pts$track_id)
  relevant <- dup_pairs[track_id_a %in% ids_here & track_id_b %in% ids_here]

  excluded <- rep(FALSE, nrow(pts))
  merged_list <- list()
  tzone <- attr(pts$timestamp, "tzone")

  for (k in seq_len(nrow(relevant))) {
    a_id <- relevant$track_id_a[k]; b_id <- relevant$track_id_b[k]
    pa <- pts[track_id == a_id]; pb <- pts[track_id == b_id]
    if (nrow(pa) == 0L || nrow(pb) == 0L) next  # ja excluido por outro par (grupo com 3+ unidades)

    t0 <- max(min(pa$timestamp), min(pb$timestamp))
    t1 <- min(max(pa$timestamp), max(pb$timestamp))
    if (t1 <= t0) next

    grid <- sort(unique(c(
      as.numeric(pa[timestamp >= t0 & timestamp <= t1, timestamp]),
      as.numeric(pb[timestamp >= t0 & timestamp <= t1, timestamp])
    )))
    if (length(grid) == 0L) next

    ax <- stats::approx(as.numeric(pa$timestamp), pa$utm_x, xout = grid, rule = 2)$y
    ay <- stats::approx(as.numeric(pa$timestamp), pa$utm_y, xout = grid, rule = 2)$y
    bx <- stats::approx(as.numeric(pb$timestamp), pb$utm_x, xout = grid, rule = 2)$y
    by_ <- stats::approx(as.numeric(pb$timestamp), pb$utm_y, xout = grid, rule = 2)$y

    merged_list[[length(merged_list) + 1L]] <- data.table::data.table(
      timestamp     = as.POSIXct(grid, origin = "1970-01-01", tz = tzone),
      utm_x         = (ax + bx) / 2,
      utm_y         = (ay + by_) / 2,
      orig_track_id = paste(a_id, b_id, sep = "+"),
      source        = "merged_duplicate"
    )

    excluded <- excluded | (pts$track_id %in% c(a_id, b_id) & pts$timestamp >= t0 & pts$timestamp <= t1)
  }

  kept <- pts[!excluded, .(timestamp, utm_x, utm_y, orig_track_id = as.character(track_id), source = "single")]
  data.table::rbindlist(c(list(kept), merged_list))
}
