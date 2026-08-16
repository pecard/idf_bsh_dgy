##
## Numero minimo de individuos, por bin de tempo e especie (modulo geral)
##
## Poe os pontos de track_dt de uma (ou mais) especie(s) em bins de duracao
## fixa (por omissao 2min) e conta, por bin, o numero MINIMO de individuos
## distintos que estiveram presentes: dois tracks (mesmo de unidades IDF
## diferentes) no mesmo bin contam como UM individuo se a distancia minima
## entre QUALQUER par de pontos dos dois tracks, dentro desse bin, for
## inferior a merge_dist_m (por omissao 200m) -- nao usa um
## centroide/posicao media por track, usa a distancia minima ponto-a-ponto
## ao longo de toda a janela do bin, para captar proximidade a meio do bin
## e handoffs entre unidades IDF.
##
## Metodo: por bin (e por especie), constroi um grafo de tracks (aresta =
## par de tracks a menos de merge_dist_m em algum instante do bin) e conta
## as componentes conexas -- ex: track A perto de B, B perto de C, mas A
## longe de C, ainda assim colapsam para 1 individuo (cadeia de
## proximidade), nao apenas pares.
##
## Modulo generico e reutilizavel -- qualquer especie, qualquer periodo,
## farm-wide. Nao depende de fatality_incidents nem de nenhuma turbina
## especifica; uma janela de dias antes de uma fatalidade e' apenas UM uso
## possivel (via date_from/date_to), entre outros (ex: parque inteiro, todo
## o periodo do projeto).
##
## Depende de: data.table, lubridate
##
## Uso:
##   source("R/track_min_individuals.R")
##
##   bins_dt <- count_min_individuals_per_bin(
##     track_dt, species = "Egyptian-Vulture",
##     bin_min = min_individuals_bin_min, merge_dist_m = min_individuals_merge_dist_m
##   )
##
##   # varias especies de uma vez -- calculado em separado por especie (um
##   # individuo nunca funde tracks de especies diferentes)
##   bins_dt <- count_min_individuals_per_bin(track_dt, species = prioritysp)
##
##   # limitado a uma janela (ex: os 8 dias antes de uma fatalidade)
##   bins_dt <- count_min_individuals_per_bin(
##     track_dt, species = "Steppe-Eagle", date_from = date_from, date_to = date_to
##   )
##
##   summarise_min_individuals(bins_dt) # pico de individuos simultaneos, por especie/periodo
##   plot_min_individuals_per_bin(bins_dt) # evolucao da contagem, eixo x = sequencia ordinal de bins
##
##   # auditoria -- validar manualmente um bin especifico (ex: um pico suspeito)
##   inspect_min_individuals_bin(track_dt, species = "Steppe-Eagle", bin_start = "2025-03-04 13:14:00")
##


## 1. Componentes conexas (union-find) -- auxiliar interno ----
##    edges: data.table com colunas i, j (indices 1-based em 1:n)

.uf_components <- function(n, edges) {

  parent <- seq_len(n)

  find <- function(x) {
    root <- x
    while (parent[root] != root) root <- parent[root]
    while (parent[x] != root) { nxt <- parent[x]; parent[x] <<- root; x <- nxt }
    root
  }

  if (nrow(edges) > 0L) {
    for (k in seq_len(nrow(edges))) {
      ra <- find(edges$i[k]); rb <- find(edges$j[k])
      if (ra != rb) parent[ra] <- rb
    }
  }

  vapply(seq_len(n), find, integer(1))
}


## 2. Um bin, uma especie -- pares de tracks a fundir + numero de individuos ----

.min_individuals_one_bin <- function(bin_pts, merge_dist_m) {

  track_ids <- unique(bin_pts$track_id)
  n <- length(track_ids)

  if (n <= 1L) {
    return(list(n_individuals_min = n, groups = data.table::data.table(track_id = track_ids, group_id = seq_len(n))))
  }

  pts_by_track <- split(bin_pts[, .(utm_x, utm_y)], bin_pts$track_id)[track_ids]

  edges_i <- integer(0); edges_j <- integer(0)
  for (a in seq_len(n - 1L)) {
    pa <- pts_by_track[[a]]
    for (b in (a + 1L):n) {
      pb <- pts_by_track[[b]]
      d <- sqrt(outer(pa$utm_x, pb$utm_x, "-")^2 + outer(pa$utm_y, pb$utm_y, "-")^2)
      if (min(d) < merge_dist_m) { edges_i <- c(edges_i, a); edges_j <- c(edges_j, b) }
    }
  }

  roots <- .uf_components(n, data.table::data.table(i = edges_i, j = edges_j))

  list(
    n_individuals_min = length(unique(roots)),
    groups = data.table::data.table(track_id = track_ids, group_id = match(roots, unique(roots)))
  )
}


## 3. Farm-wide, todos os bins de uma (ou mais) especie(s) ----

count_min_individuals_per_bin <- function(track_dt, species, bin_min = 2, merge_dist_m = 200,
                                          date_from = NULL, date_to = NULL) {

  pts <- track_dt[spec %in% species, .(track_id, timestamp, utm_x, utm_y, spec)]
  if (!is.null(date_from)) pts <- pts[timestamp >= date_from]
  if (!is.null(date_to))   pts <- pts[timestamp <= date_to]

  empty <- data.table::data.table(
    spec = character(), bin_start = as.POSIXct(character()),
    n_tracks = integer(), n_individuals_min = integer()
  )
  if (nrow(pts) == 0L) return(empty)

  pts[, bin_start := lubridate::floor_date(timestamp, unit = paste(bin_min, "minutes"))]

  res <- pts[, {
    r <- .min_individuals_one_bin(.SD, merge_dist_m = merge_dist_m)
    .(n_tracks = length(unique(track_id)), n_individuals_min = r$n_individuals_min)
  }, by = .(spec, bin_start)]

  data.table::setorder(res, spec, bin_start)
  res[]
}


## 4. Sumario -- pico de individuos simultaneos, por especie ----
##    n_individuals_min do bin de pico e' uma estimativa por defeito (lower
##    bound) do numero de individuos distintos presentes nesse periodo: se
##    naquele bin de 2min havia, no minimo, X individuos claramente distintos
##    (>=200m entre si), entao a populacao presente no periodo e' >= X.
##
##    mean_individuals_nonzero -- media de n_individuals_min sobre os n_bins
##    bins ONDE A ESPECIE FOI DETETADA (bins_dt so tem linhas para bins com
##    >=1 deteccao -- nao ha linhas com n_individuals_min=0 para filtrar; o
##    "nonzero" no nome refere-se a esse desenho dos dados de entrada, nao a
##    um filtro ativo aqui). Ou seja: em media, quantos individuos distintos
##    estao simultaneamente presentes num bin em que a especie aparece.

summarise_min_individuals <- function(bins_dt) {

  if (nrow(bins_dt) == 0L) {
    return(data.table::data.table(
      spec = character(), n_bins = integer(), peak_individuals = integer(),
      peak_bin_start = as.POSIXct(character()), mean_individuals_nonzero = numeric()
    ))
  }

  out <- bins_dt[, {
    i_max <- which.max(n_individuals_min)
    .(
      n_bins = .N,
      peak_individuals = n_individuals_min[i_max],
      peak_bin_start = bin_start[i_max],
      mean_individuals_nonzero = round(mean(n_individuals_min[n_individuals_min > 0]), 2)
    )
  }, by = spec]

  data.table::setorder(out, -peak_individuals)
  out[]
}


## 5. Plot -- evolucao da contagem ao longo dos bins, por especie ----
##
## x = indice ordinal do bin (1, 2, 3, ...), NAO o timestamp -- os bins de
## bins_dt sao apenas os que tem pelo menos 1 registo (ver
## count_min_individuals_per_bin(), agrupado por spec+bin_start), por isso a
## sequencia ordinal salta bins vazios/sem deteções em vez de os mostrar como
## um gap na escala temporal real.

plot_min_individuals_per_bin <- function(bins_dt, species_sel = NULL) {

  dt <- data.table::copy(bins_dt)
  if (!is.null(species_sel)) dt <- dt[spec %in% species_sel]

  data.table::setorder(dt, spec, bin_start)
  dt[, bin_seq := seq_len(.N), by = spec]

  ggplot(dt, aes(x = bin_seq, y = n_individuals_min)) +
    geom_line(colour = "steelblue") +
    geom_point(colour = "steelblue", size = 1.2) +
    facet_wrap(~ spec, ncol = 1, scales = "free_y") +
    labs(
      x = "Time bin (ordinal sequence)",
      y = "Minimum number of individuals",
      title = "Minimum individuals per time bin"
    ) +
    theme_minimal()
}


## 6. Auditoria -- detalhe de um bin especifico (validar manualmente um pico) ----
##
## Devolve, para uma especie e um bin (identificado pelo seu bin_start), um
## resumo por track: nº de pontos, unidade(s) IDF que o registaram, janela
## temporal dentro do bin, extensao espacial (min-max UTM), e o group_id
## atribuido pelo algoritmo de componentes conexas (mesmo group_id = mesmo
## individuo, por count_min_individuals_per_bin()).
##
## Serve para distinguir um pico genuino (ex: kettle de migracao, varios
## tracks bem separados no espaco e/ou vistos por unidades IDF diferentes)
## de um possivel artefacto de fragmentacao de track_id -- o IdentiFlight
## por vezes atribui um novo track_id a meio do voo da mesma ave (troca de
## ID, ver seccao 1.2 "ID transitions" do IDF_analysis.R); tracks muito
## curtos (poucos pontos), da mesma unidade IDF, que comecam mesmo a seguir
## a outro terminar perto do mesmo sitio, sao um sinal desse artefacto --
## nesse caso o algoritmo pode estar a contar como 2+ individuos o que e'
## na realidade a mesma ave, se os dois fragmentos ficarem a mais de
## merge_dist_m um do outro no momento da troca.

inspect_min_individuals_bin <- function(track_dt, species, bin_start, bin_min = 2, merge_dist_m = 200) {

  tz <- attr(track_dt$timestamp, "tzone")
  bin_start <- as.POSIXct(bin_start, tz = tz)
  bin_end   <- bin_start + bin_min * 60

  pts <- track_dt[
    spec == species & timestamp >= bin_start & timestamp < bin_end,
    .(track_id, timestamp, idf, utm_x, utm_y)
  ]

  empty <- data.table::data.table(
    track_id = character(), group_id = integer(), n_points = integer(),
    idf_units = character(), first_time = as.POSIXct(character(), tz = tz),
    last_time = as.POSIXct(character(), tz = tz), utm_x_range = character(), utm_y_range = character()
  )
  if (nrow(pts) == 0L) return(empty)

  r <- .min_individuals_one_bin(pts, merge_dist_m = merge_dist_m)

  track_summary <- pts[, .(
    n_points    = .N,
    idf_units   = paste(sort(unique(idf)), collapse = ", "),
    first_time  = min(timestamp),
    last_time   = max(timestamp),
    utm_x_range = sprintf("%.0f - %.0f", min(utm_x), max(utm_x)),
    utm_y_range = sprintf("%.0f - %.0f", min(utm_y), max(utm_y))
  ), by = track_id]

  out <- merge(track_summary, r$groups, by = "track_id")
  data.table::setorder(out, group_id, first_time)
  out[]
}
