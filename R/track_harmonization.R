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
##      em SIMULTANEO (tracks sobrepostos no tempo), desviados espacialmente
##      um do outro por um erro de calibracao/geometria ENTRE as 2 unidades.
##      Testar isto por "distancia minima em algum instante" (como
##      count_min_individuals_per_bin(), R/track_min_individuals.R) daria
##      demasiados falsos positivos -- 2 aves DIFERENTES podem passar perto
##      uma da outra por um instante sem serem duplicados. O sinal real e'
##      os 2 tracks manterem uma distancia relativa ESTAVEL ao longo de TODA
##      a janela em que coexistem, nao so' num ponto.
##
##      IMPORTANTE (revisto 2026-08 com dados reais BSH, unidades 31/32,
##      Steppe-Eagle, 2026-08-07): o desvio de calibracao NAO e' "poucos
##      metros" como se assumiu inicialmente a partir da inspecao visual --
##      em 5 pares reais confirmados como o MESMO individuo, a distancia
##      mediana andou entre 130-173m, MUITO acima de qualquer limiar
##      absoluto pequeno (0% dentro de 20m OU 50m). O que distinguiu estes
##      pares de 2 aves diferentes nao foi a MAGNITUDE da distancia, foi a
##      sua ESTABILIDADE -- dist_max-dist_min ficou entre 6-49m dentro de
##      cada par, i.e. um desvio sistematico quase constante entre as 2
##      unidades para aquele objeto, nao um "por acaso perto". Por isso o
##      teste usa max_median_dist_m (um teto de sanidade generoso sobre a
##      magnitude, para nao fundir 2 aves em formacao) + max_spread_m (o
##      criterio que realmente decide -- quao estavel e' o desvio em volta
##      da SUA PROPRIA mediana, nao em volta de 0).
##
##   3. Combinacao das 2 -- 2 cadeias de handoff (uma por unidade IDF),
##      ligadas entre si por um ou mais pares "duplicate" -- ver
##      build_reconciliation_groups() abaixo, que junta os 2 tipos de aresta
##      no MESMO grafo de componentes conexas, exatamente para capturar isto.
##
## Parametros acordados com o Paulo (2026-08): handoff testado a 15-30s /
## 50m. Duplicado: FRACCAO ALTA (nao 100%) dos instantes sobrepostos dentro
## de max_spread_m da MEDIANA do par (nao de um valor fixo), mais um teto de
## sanidade max_median_dist_m sobre a propria mediana -- ver nota acima. Os
## valores por omissao abaixo sao um ponto de partida, nao valores
## validados -- a rever depois de ver mais casos reais (ver
## explore_track_harmonization.R).
##
## Depende de: data.table, plotly, R/track_min_individuals.R (.uf_components
## -- usa a MESMA implementacao de componentes conexas, nao redefinida aqui)
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

  cand <- data.table::foverlaps(
    opening, closing, by.x = c("wstart", "wend"), by.y = c("wstart", "wend"),
    type = "any", nomatch = NULL
  )
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
##    tempo, com um desvio ESTAVEL entre si ao longo de uma FRACCAO ALTA
##    (nao 100%) da janela de sobreposicao ----
##
## 1º passo (foverlaps, barato): so' intervalos [start,end] sobrepostos.
## 2º passo (so' nos candidatos sobreviventes, mais caro): reamostra os 2
## tracks para uma grelha temporal comum (uniao dos timestamps de ambos
## dentro da sobreposicao, interpolacao linear -- stats::approx()) e mede,
## para cada instante, o desvio da distancia A' PROPRIA MEDIANA do par (nao
## a zero) -- ver nota no topo do ficheiro sobre porque a magnitude da
## distancia (max_median_dist_m, so um teto de sanidade) NAO e' o criterio
## decisivo, so' a sua estabilidade (max_spread_m) o e'.
##

find_duplicate_edges <- function(track_dt, species, max_median_dist_m = 300,
                                  max_spread_m = 50, min_overlap_frac = 0.8, min_overlap_sec = 10) {

  dt <- track_dt[spec == species, .(track_id, timestamp, utm_x, utm_y, idf)]
  data.table::setorder(dt, track_id, timestamp)
  data.table::setkey(dt, track_id)

  empty <- data.table::data.table(
    track_id_a = dt$track_id[0], track_id_b = dt$track_id[0],
    overlap_sec = numeric(), median_dist_m = numeric(), frac_within_spread = numeric()
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
  cand <- data.table::foverlaps(
    b, a, by.x = c("wstart", "wend"), by.y = c("wstart", "wend"),
    type = "any", nomatch = NULL
  )

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
      list(overlap_sec = overlap_sec, median_dist_m = NA_real_, frac_within_spread = NA_real_)
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
      med <- stats::median(d)
      list(overlap_sec = overlap_sec, median_dist_m = med, frac_within_spread = mean(abs(d - med) <= max_spread_m))
    }
  }, by = .(track_id_a, track_id_b)]

  out <- edges[
    overlap_sec >= min_overlap_sec &
      median_dist_m <= max_median_dist_m &
      frac_within_spread >= min_overlap_frac
  ]
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
## Sobreposicoes de 3+ unidades (ex: uma track ligada por "duplicate" a 2
## OUTRAS que nao se sobrepoem completamente entre si) sao fundidas em
## CLUSTER -- media conjunta de todas as tracks ativas em cada instante, nao
## par a par sequencialmente. A 1ª versao fundia par a par e produzia um
## zig-zag visivel (caso real Steppe-Eagle, 2026-08: ABE4713A sobreposta a
## FC67642E E a E4E2BB64 em simultaneo) -- ver .merge_group_points() abaixo.
##

stitch_synthetic_tracks <- function(track_dt, species, groups_dt, duplicate_edges_dt, grid_step_sec = 1) {

  dt <- track_dt[spec == species, .(track_id, timestamp, utm_x, utm_y)]
  dt <- merge(dt, groups_dt[, .(track_id, synth_track_id)], by = "track_id")

  dup_pairs <- if (!is.null(duplicate_edges_dt) && nrow(duplicate_edges_dt) > 0L) {
    duplicate_edges_dt[, .(track_id_a, track_id_b)]
  } else {
    data.table::data.table(track_id_a = dt$track_id[0], track_id_b = dt$track_id[0])
  }

  out <- dt[, .merge_group_points(.SD, dup_pairs, grid_step_sec = grid_step_sec), by = synth_track_id]
  data.table::setorder(out, synth_track_id, timestamp)
  out[]
}


## Funde os overlaps de um grupo em CLUSTERS de duplicado, nao par a par
## sequencialmente, numa grelha REGULAR (nao a uniao dos timestamps
## brutos) ----
##
## 2 bugs corrigidos em sucessao (2026-08, caso real Steppe-Eagle), o 2º so'
## visivel depois de corrigir o 1º:
##
##   1. Par a par sequencial (1ª versao): quando uma track (ex: ABE4713A)
##      tem arestas "duplicate" validadas com 2 OUTRAS tracks (ex: FC67642E
##      e E4E2BB64), processar cada par isoladamente dava 2 medias
##      ligeiramente diferentes para o MESMO instante (troço em que os 2
##      pares se sobrepunham) -- corrigido agrupando por CLUSTER (ver
##      abaixo), 1 unico calculo conjunto por cluster.
##
##   2. Grelha = uniao dos timestamps BRUTOS (2ª versao): mesmo so' com 1
##      cluster, cada instante da grelha "pertence" a' unidade que la'
##      calhou de amostrar -- 2 unidades IDF amostram em relogios proprios,
##      tipicamente intercalados, NUNCA exatamente nos mesmos instantes. Com
##      um desvio de dezenas a centenas de metros entre unidades (ver nota
##      no topo do ficheiro), a media num instante "da unidade A" fica
##      puxada principalmente para A (a B so' entra interpolada), e no
##      instante seguinte "da unidade B" fica puxada para B -- um zig-zag
##      visivel de amplitude ~metade do desvio entre unidades, NAO um erro
##      de agrupamento. Corrigido usando uma grelha REGULAR (passo fixo
##      grid_step_sec, por omissao 1s -- a cadencia tipica de deteccao
##      observada nestes dados), igual para todos os membros do cluster, em
##      vez da uniao dos timestamps de cada um.
##
## As arestas "duplicate" validadas do grupo formam os seus proprios
## clusters (componentes conexas SO' dessas arestas, nao de todo o grupo --
## uma track ligada so' por handoff a outras fica de fora). Dentro de cada
## cluster, para cada instante da grelha regular, faz-se a media de TODAS
## as tracks do cluster que estiverem ativas NESSE INSTANTE (1, 2, 3+, o
## que for). Pontos fora de qualquer janela multi-ativa (incluindo troços
## solo de um membro do cluster antes/depois de coexistir com outro) passam
## tal e qual, sem resampling.

.merge_group_points <- function(pts, dup_pairs, grid_step_sec = 1) {

  ids_here <- unique(pts$track_id)
  relevant <- dup_pairs[track_id_a %in% ids_here & track_id_b %in% ids_here]
  tzone <- attr(pts$timestamp, "tzone")

  if (nrow(relevant) == 0L) {
    return(pts[, .(
      timestamp, utm_x = as.double(utm_x), utm_y = as.double(utm_y),
      orig_track_id = as.character(track_id), source = "single"
    )])
  }

  dup_ids   <- sort(unique(c(relevant$track_id_a, relevant$track_id_b)))
  idx_of    <- stats::setNames(seq_along(dup_ids), dup_ids)
  edges_idx <- data.table::data.table(i = idx_of[relevant$track_id_a], j = idx_of[relevant$track_id_b])
  cluster_of <- .uf_components(length(dup_ids), edges_idx)

  excluded <- rep(FALSE, nrow(pts))
  merged_list <- list()

  for (cl in unique(cluster_of)) {
    members <- dup_ids[cluster_of == cl]
    if (length(members) < 2L) next

    cpts  <- pts[track_id %in% members]
    spans <- cpts[, .(t0 = as.numeric(min(timestamp)), t1 = as.numeric(max(timestamp))), by = track_id]
    data.table::setkey(spans, track_id)

    # grelha REGULAR (passo fixo), NAO a uniao dos timestamps brutos --
    # ver nota acima do ficheiro (bug 2): com a uniao bruta, cada instante
    # "pertence" a' unidade que la' amostrou, puxando a media
    # alternadamente para cada lado e criando um zig-zag
    t0_all <- min(spans$t0); t1_all <- max(spans$t1)
    if (t1_all <= t0_all) next
    grid <- seq(t0_all, t1_all, by = grid_step_sec)

    # posicao interpolada (rule=2) de CADA membro em TODOS os instantes da
    # grelha do cluster em que estiver ativo (dentro do seu proprio
    # [t0,t1]) -- fora disso fica NA e nao entra na media desse instante
    pos <- lapply(members, function(tid) {
      p  <- cpts[track_id == tid]
      sp <- spans[.(tid)]
      active <- grid >= sp$t0 & grid <= sp$t1
      x <- rep(NA_real_, length(grid)); y <- rep(NA_real_, length(grid))
      if (any(active)) {
        x[active] <- stats::approx(as.numeric(p$timestamp), p$utm_x, xout = grid[active], rule = 2)$y
        y[active] <- stats::approx(as.numeric(p$timestamp), p$utm_y, xout = grid[active], rule = 2)$y
      }
      list(x = x, y = y, active = active)
    })

    x_mat   <- do.call(cbind, lapply(pos, `[[`, "x"))
    y_mat   <- do.call(cbind, lapply(pos, `[[`, "y"))
    act_mat <- do.call(cbind, lapply(pos, `[[`, "active"))
    n_active <- rowSums(act_mat)

    multi <- n_active >= 2L
    if (!any(multi)) next  # cluster formado mas sem instante realmente simultaneo (defensivo -- nao devia acontecer)

    x_final <- rowMeans(x_mat[multi, , drop = FALSE], na.rm = TRUE)
    y_final <- rowMeans(y_mat[multi, , drop = FALSE], na.rm = TRUE)
    act_sub <- act_mat[multi, , drop = FALSE]
    orig_id_m <- vapply(seq_len(nrow(act_sub)), function(i) paste(members[act_sub[i, ]], collapse = "+"), character(1))

    merged_list[[length(merged_list) + 1L]] <- data.table::data.table(
      timestamp     = as.POSIXct(grid[multi], origin = "1970-01-01", tz = tzone),
      utm_x         = x_final, utm_y = y_final,
      orig_track_id = orig_id_m, source = "merged_duplicate"
    )

    # intervalos CONTIGUOS onde n_active>=2 (rle sobre a grelha regular) --
    # exclui, de cada membro, so' os pontos BRUTOS cujo timestamp caia
    # dentro de algum desses intervalos (precisao = 1 passo de grelha,
    # suficiente aqui), nao o [t0,t1] inteiro do cluster -- para nao apagar
    # um troço solo de um membro antes/depois de coexistir com outro
    runs <- rle(multi)
    run_ends   <- cumsum(runs$lengths)
    run_starts <- run_ends - runs$lengths + 1L
    for (r in which(runs$values)) {
      t_start <- grid[run_starts[r]]; t_end <- grid[run_ends[r]]
      excluded <- excluded | (
        pts$track_id %in% members &
          as.numeric(pts$timestamp) >= t_start & as.numeric(pts$timestamp) <= t_end
      )
    }
  }

  # utm_x/utm_y forcados a double -- track_dt guarda-os como integer, mas o
  # ramo "merged_duplicate" acima produz sempre double (media/interpolacao);
  # sem isto, um grupo sem nenhum cluster mantem integer e um grupo com
  # pelo menos 1 mistura os 2 tipos na mesma coluna, o que a agregacao por
  # grupo (by=synth_track_id) em stitch_synthetic_tracks() rejeita
  # ("Column ... is type 'double' but expecting type 'integer'")
  kept <- pts[!excluded, .(
    timestamp, utm_x = as.double(utm_x), utm_y = as.double(utm_y),
    orig_track_id = as.character(track_id), source = "single"
  )]
  data.table::rbindlist(c(list(kept), merged_list))
}


##
## 6. Diagnostico bruto -- TODOS os pares candidatos, SEM aplicar
##    time_window_sec/max_dist_m (handoff) ou max_median_dist_m/max_spread_m/
##    min_overlap_frac (duplicate) ----
##
## Uso (2026-08, depois de Griffon-Vulture/Cinereous-Vulture/Steppe-Eagle
## nao aparecerem em find_handoff_edges()/find_duplicate_edges() num dia
## onde o Paulo tinha casos identificados visualmente): antes de mudar
## limiares as cegas outra vez, ver os numeros reais A' VOLTA dos pares que
## ele ja sabe que deviam ligar-se, e so' depois decidir o que ajustar (ou
## descobrir que o problema nem e' de limiar -- ex: nome de especie
## diferente do esperado, dia/instante errado, etc.)
##
## Ambas ordenam pelo campo mais relevante para "scroll ate' encontrares o
## par que reconheces", nao pelo track_id -- diagnose_handoff_candidates()
## por |gap_sec| crescente (pares mais proximos no tempo primeiro,
## independente de estarem dentro do limiar atual), diagnose_overlap_candidates()
## por dist_median crescente.
##

## Todos os pares ordenados (A fecha, B abre a seguir) -- gap_sec pode ser
## NEGATIVO (B comecou ANTES de A terminar, i.e. sobrepostos no tempo -- info
## util mesmo aqui, pode ser um caso "duplicate" disfarcado). dist_m e' so'
## a distancia entre o ultimo ponto de A e o 1º de B, sem limiar nenhum.
diagnose_handoff_candidates <- function(track_dt, species) {

  dt <- track_dt[spec == species, .(track_id, timestamp, utm_x, utm_y, idf)]
  data.table::setorder(dt, track_id, timestamp)

  ends <- dt[, .(
    first_ts  = data.table::first(timestamp), first_x = data.table::first(utm_x), first_y = data.table::first(utm_y),
    last_ts   = data.table::last(timestamp),  last_x  = data.table::last(utm_x),  last_y  = data.table::last(utm_y),
    idf_units = paste(sort(unique(idf)), collapse = ",")
  ), by = track_id]

  n <- nrow(ends)
  if (n < 2L) return(data.table::data.table())

  out <- data.table::rbindlist(lapply(seq_len(n), function(i) {
    j <- setdiff(seq_len(n), i)
    data.table::data.table(
      track_id_a = ends$track_id[i], track_id_b = ends$track_id[j],
      idf_a = ends$idf_units[i], idf_b = ends$idf_units[j],
      gap_sec = as.numeric(difftime(ends$first_ts[j], ends$last_ts[i], units = "secs")),
      dist_m  = sqrt((ends$last_x[i] - ends$first_x[j])^2 + (ends$last_y[i] - ends$first_y[j])^2)
    )
  }))

  # setorder() so aceita nomes de coluna, nao expressoes (abs(gap_sec) direto
  # falha com "some columns are not in the data.table: [abs]") -- ordena por
  # uma coluna auxiliar e remove-a a seguir
  out[, abs_gap_sec := abs(gap_sec)]
  data.table::setorder(out, abs_gap_sec)
  out[, abs_gap_sec := NULL]
  out[]
}


## Todos os pares com QUALQUER sobreposicao temporal (>0s), com a
## distancia interpolada ao longo dessa sobreposicao resumida (min/mediana/
## max/spread + fraccao dentro de +-25m e +-50m DA PROPRIA MEDIANA do par,
## nao de zero) -- para calibrar max_median_dist_m/max_spread_m/
## min_overlap_frac contra casos reais em vez de adivinhar. NAO filtra por
## idf_a != idf_b (ao contrario de find_duplicate_edges()) -- serve tambem
## para despistar se a assuncao de "1 unidade por track" aguenta nos dados
## reais.
diagnose_overlap_candidates <- function(track_dt, species) {

  dt <- track_dt[spec == species, .(track_id, timestamp, utm_x, utm_y, idf)]
  data.table::setorder(dt, track_id, timestamp)
  data.table::setkey(dt, track_id)

  spans <- dt[, .(
    start = min(timestamp), end = max(timestamp),
    idf_units = paste(sort(unique(idf)), collapse = ",")
  ), by = track_id]

  n <- nrow(spans)
  if (n < 2L) return(data.table::data.table())

  pairs <- utils::combn(n, 2)

  out <- data.table::rbindlist(lapply(seq_len(ncol(pairs)), function(k) {
    i <- pairs[1, k]; j <- pairs[2, k]

    t0 <- max(spans$start[i], spans$start[j])
    t1 <- min(spans$end[i], spans$end[j])
    overlap_sec <- as.numeric(difftime(t1, t0, units = "secs"))
    if (overlap_sec <= 0) return(NULL)

    pa <- dt[.(spans$track_id[i])]
    pb <- dt[.(spans$track_id[j])]
    grid <- sort(unique(c(
      as.numeric(pa[timestamp >= t0 & timestamp <= t1, timestamp]),
      as.numeric(pb[timestamp >= t0 & timestamp <= t1, timestamp])
    )))
    if (length(grid) == 0L) return(NULL)

    ax <- stats::approx(as.numeric(pa$timestamp), pa$utm_x, xout = grid, rule = 2)$y
    ay <- stats::approx(as.numeric(pa$timestamp), pa$utm_y, xout = grid, rule = 2)$y
    bx <- stats::approx(as.numeric(pb$timestamp), pb$utm_x, xout = grid, rule = 2)$y
    by_ <- stats::approx(as.numeric(pb$timestamp), pb$utm_y, xout = grid, rule = 2)$y
    d <- sqrt((ax - bx)^2 + (ay - by_)^2)

    med <- stats::median(d)

    data.table::data.table(
      track_id_a = spans$track_id[i], track_id_b = spans$track_id[j],
      idf_a = spans$idf_units[i], idf_b = spans$idf_units[j],
      overlap_sec              = overlap_sec,
      dist_min                 = min(d), dist_median = med, dist_max = max(d),
      dist_spread              = max(d) - min(d),
      frac_within_median_pm25m = mean(abs(d - med) <= 25),
      frac_within_median_pm50m = mean(abs(d - med) <= 50)
    )
  }))

  if (is.null(out) || nrow(out) == 0L) return(data.table::data.table())
  # ordena por ESTABILIDADE (dist_spread), nao pela magnitude da distancia --
  # e' o spread pequeno que indica "mesmo individuo, desviado por
  # calibracao", nao a distancia em si (ver nota no topo do ficheiro:
  # confirmado nos dados reais que a mediana pode andar nos 130-180m e ainda
  # assim ser o MESMO individuo)
  data.table::setorder(out, dist_spread)
  out[]
}


##
## 7. Plot interativo (plotly, estilo plot_forensic_rpm() em
##    R/curtailment_forensic_trace.R) -- as trajetorias espaciais (utm_x,
##    utm_y) dos track_ids indicados, uma cor por track_id, com hover
##    ponto-a-ponto (timestamp/unidade IDF) para julgar a plausibilidade de
##    uma fusao ao olho, ANTES de confiar em qualquer limiar ----
##
## Aceita qualquer tabela de arestas com colunas track_id_a/track_id_b
## (find_handoff_edges(), find_duplicate_edges(), ou as versoes SEM filtro
## diagnose_handoff_candidates()/diagnose_overlap_candidates()) -- desenha
## uma linha tracejada entre o ULTIMO ponto de A e o PRIMEIRO ponto de B,
## com hover mostrando TODAS as colunas da aresta (gap_sec/dist_m,
## overlap_sec/frac_within_dist, o que la' estiver). Para arestas
## "duplicate"/overlap, essa linha e' so' ilustrativa (liga os extremos, nao
## o segmento de sobreposicao real) -- serve para dizer "estes 2 tracks
## foram considerados o mesmo par", nao para ler distancias exatas dela
## (essas ja vem na tabela/hover).
##
## track_ids = NULL desenha TODOS os tracks presentes em track_dt (cuidado
## com dias/especies com muitos tracks -- fica ilegivel); normalmente e'
## chamado com um subconjunto pequeno, escolhido a partir de
## reconciliation_by_species/diagnose_*_candidates.
##
## Uso:
##   plot_candidate_tracks(track_dt_day, c("659C681F-...", "397C369F-..."))
##
##   # com as arestas candidatas sobrepostas (linha tracejada + hover):
##   plot_candidate_tracks(track_dt_day, merged_ids_do_grupo, edges_dt = edges1)
##
##   # direto de uma tabela de diagnostico, so' as 3 linhas mais promissoras:
##   cand <- diagnose_handoff_candidates(track_dt_day, "Steppe-Eagle")[1:3]
##   plot_candidate_tracks(track_dt_day, unique(c(cand$track_id_a, cand$track_id_b)), edges_dt = cand)
##

plot_candidate_tracks <- function(track_dt, track_ids = NULL, edges_dt = NULL, title = NULL) {

  dt <- if (is.null(track_ids)) {
    data.table::copy(track_dt)[, .(track_id, timestamp, utm_x, utm_y, idf, spec)]
  } else {
    track_dt[track_id %in% track_ids, .(track_id, timestamp, utm_x, utm_y, idf, spec)]
  }
  data.table::setorder(dt, track_id, timestamp)
  if (nrow(dt) == 0L) stop("plot_candidate_tracks(): nenhum ponto encontrado para os track_ids indicados.")

  p <- plotly::plot_ly()

  for (tid in unique(dt$track_id)) {
    pts <- dt[track_id == tid]
    p <- p %>% plotly::add_trace(
      data = pts, x = ~utm_x, y = ~utm_y, type = "scatter", mode = "lines+markers",
      name = as.character(tid),
      marker = list(size = 6), line = list(width = 2),
      text = ~sprintf("track_id: %s<br>spec: %s<br>idf: %s<br>%s", tid, spec, idf, format(timestamp, "%Y-%m-%d %H:%M:%S")),
      hoverinfo = "text"
    )
  }

  if (!is.null(edges_dt) && nrow(edges_dt) > 0L) {

    ends <- dt[, .(
      x_first = data.table::first(utm_x), y_first = data.table::first(utm_y),
      x_last  = data.table::last(utm_x),  y_last  = data.table::last(utm_y)
    ), by = track_id]

    for (i in seq_len(nrow(edges_dt))) {
      e  <- edges_dt[i]
      ea <- e$track_id_a; eb <- e$track_id_b
      if (!(ea %in% ends$track_id) || !(eb %in% ends$track_id)) next

      pa <- ends[track_id == ea]; pb <- ends[track_id == eb]
      hover_txt <- paste(
        sprintf("%s: %s", names(e), vapply(e, function(v) format(v, digits = 3, trim = TRUE), character(1))),
        collapse = "<br>"
      )

      p <- p %>% plotly::add_trace(
        x = c(pa$x_last, pb$x_first), y = c(pa$y_last, pb$y_first),
        type = "scatter", mode = "lines",
        line = list(color = "grey30", width = 1.5, dash = "dot"),
        hoverinfo = "text", text = hover_txt, showlegend = FALSE
      )
    }
  }

  p %>% plotly::layout(
    title = if (is.null(title)) sprintf("Candidate tracks -- %s", paste(sort(unique(dt$spec)), collapse = ", ")) else title,
    xaxis = list(title = "UTM X", scaleanchor = "y"),  # scaleanchor -- aspeto 1:1, distancias em metros nao ficam distorcidas
    yaxis = list(title = "UTM Y"),
    showlegend = TRUE
  )
}


##
## 8. Plot interativo (plotly) do resultado FINAL -- os fragmentos originais
##    (finos, cinzentos, so' para contexto/comparacao) por baixo da
##    trajetoria sintetica reconciliada (grossa, marcadores coloridos por
##    "source": azul = "single" (segmento vindo de 1 so' track, concatenado
##    por handoff), vermelho = "merged_duplicate" (segmento fundido de 2
##    unidades IDF em simultaneo)) por cima ----
##
## synth_dt: resultado de stitch_synthetic_tracks() (tem synth_track_id,
## timestamp, utm_x, utm_y, orig_track_id, source). Os track_ids originais a
## desenhar em fundo sao derivados do proprio orig_track_id (separa "idA+idB"
## nas linhas "merged_duplicate"), nao precisam de ser passados a parte.
##
## Uso:
##   synth_dt <- stitch_synthetic_tracks(track_dt_day, "Steppe-Eagle", groups_se, duplicate_se)
##   plot_synthetic_track(track_dt_day, synth_dt, "SYN_Steppe-Eagle_004")
##

plot_synthetic_track <- function(track_dt, synth_dt, target_synth_id, title = NULL, show_raw = TRUE) {

  synth_pts <- synth_dt[synth_track_id == target_synth_id]
  if (nrow(synth_pts) == 0L) stop("plot_synthetic_track(): synth_track_id nao encontrado em synth_dt.")
  data.table::setorder(synth_pts, timestamp)

  p <- plotly::plot_ly()

  # fragmentos originais -- finos/cinzentos, so' para comparar com o
  # resultado. show_raw = FALSE isola so' a trajetoria sintetica (util para
  # confirmar se um padrao em zig-zag vem mesmo do caminho final ou e' so'
  # o aspeto visual de varios fragmentos brutos sobrepostos no mesmo
  # corredor espacial -- caso real Steppe-Eagle, 2026-08: o zig-zag
  # aparente era dos fragmentos cinzentos, nao da trajetoria sintetica,
  # que ja tinha sido confirmada suave via velocidade implicita
  # ponto-a-ponto)
  if (show_raw) {
    orig_ids <- unique(unlist(strsplit(synth_pts$orig_track_id, "+", fixed = TRUE)))
    raw_pts <- track_dt[track_id %in% orig_ids, .(track_id, timestamp, utm_x, utm_y, idf)]
    data.table::setorder(raw_pts, track_id, timestamp)

    for (tid in unique(raw_pts$track_id)) {
      pts <- raw_pts[track_id == tid]
      p <- p %>% plotly::add_trace(
        data = pts, x = ~utm_x, y = ~utm_y, type = "scatter", mode = "lines+markers",
        name = paste0("orig: ", tid), legendgroup = "orig",
        line = list(width = 1, color = "grey70"), marker = list(size = 3, color = "grey70"),
        opacity = 0.6,
        text = ~sprintf("orig track_id: %s<br>idf: %s<br>%s", tid, idf, format(timestamp, "%Y-%m-%d %H:%M:%S")),
        hoverinfo = "text"
      )
    }
  }

  # trajetoria final -- 1 linha continua ligando TODOS os pontos sinteticos
  # por ordem temporal, por baixo dos marcadores coloridos por source
  p <- p %>% plotly::add_trace(
    data = synth_pts, x = ~utm_x, y = ~utm_y, type = "scatter", mode = "lines",
    name = "synthetic path", line = list(width = 2.5, color = "black"),
    hoverinfo = "skip", showlegend = TRUE
  )

  color_map <- c(single = "steelblue", merged_duplicate = "firebrick")
  for (src in unique(synth_pts$source)) {
    seg <- synth_pts[source == src]
    p <- p %>% plotly::add_trace(
      data = seg, x = ~utm_x, y = ~utm_y, type = "scatter", mode = "markers",
      name = sprintf("synthetic (%s)", src),
      marker = list(size = 7, color = color_map[[src]]),
      text = ~sprintf("synth: %s<br>orig: %s<br>source: %s<br>%s", target_synth_id, orig_track_id, source, format(timestamp, "%Y-%m-%d %H:%M:%S")),
      hoverinfo = "text"
    )
  }

  default_title <- if (show_raw) {
    sprintf("Synthetic track %s (raw fragments in grey)", target_synth_id)
  } else {
    sprintf("Synthetic track %s (synthetic path only)", target_synth_id)
  }

  p %>% plotly::layout(
    title = if (is.null(title)) default_title else title,
    xaxis = list(title = "UTM X", scaleanchor = "y"),
    yaxis = list(title = "UTM Y"),
    showlegend = TRUE
  )
}
