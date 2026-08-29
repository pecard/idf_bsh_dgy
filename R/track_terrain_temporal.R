##
## Padrao temporal de uso do espaco por classe de terreno (bins de 7 dias)
##
## Pergunta do Paulo (2026-08): alem de ver se a distancia/velocidade dos
## curtailments varia por classe de terreno (R/curtailment_bearing_sectors.R,
## R/turbine_terrain_classification.R), tambem interessa ver se o PROPRIO
## padrao de uso do espaco (TODOS os tracks, nao so' os que dispararam
## curtailment) muda ao longo do tempo consoante a classe de terreno -- ex:
## mais tracks perto de cristas nalguma epoca do ano (migracao?).
##
## Cada track e' associado a' turbina geometricamente mais proxima (via UTM,
## KD-tree RANN::nn2 -- NAO o campo NearestTurbine3d/turbine do proprio
## track_dt, que pode atribuir mal pontos de fronteira -- mesma razao de
## R/fatality_track_investigation.R e R/coverage_3d_topography.R) no seu 1o
## registo -- mesma aproximacao "1o ponto = momento de referencia" ja usada
## em R/curtailment_bearing_sectors.R. Um track conta 1 vez so', no bin de 7
## dias onde caiu esse 1o registo -- NAO conta 1 vez por semana em que
## esteve presente (evitaria duplicar tracks longos, mas nao e' isso que
## foi pedido -- "total de tracks por bin", nao "presenca-semanas").
##
## Bins de 7 dias ancorados ao 1o dia com dados (nao ao calendario) -- ver
## week_start em summarise_tracks_by_week_terrain() -- da' janelas exatas de
## 7 dias desde o inicio do periodo analisado, nao semanas ISO.
##
## As classes de terreno tipicamente NAO tem o mesmo numero de turbinas (ex:
## caso real do Bash, 2026-08: table(terrain_dt$terrain_class) deu flat=52,
## complex=19, ridge=8) -- comparar n_tracks absoluto (soma de todos os
## tracks perto de QUALQUER turbina da classe) sobrestima sempre a classe
## com mais turbinas, mesmo que a atividade POR TURBINA seja identica.
##
## Por isso summarise_tracks_by_week_terrain() agrega primeiro por TURBINA x
## semana (nivel mais fino, incluindo turbinas com 0 tracks nessa semana),
## e so' depois por classe x semana, dando 3 estatisticas ENTRE TURBINAS da
## mesma classe, nao so' uma razao soma/contagem:
##   mean_tracks_per_turbine   -- media de tracks por turbina na classe (o
##                                que o Paulo pediu, 2026-08, ao ver a
##                                distribuicao desigual de turbinas por
##                                classe) -- numericamente equivalente a
##                                n_tracks/n_turbines, mas calculada aqui a
##                                partir da distribuicao real por turbina,
##                                nao so' 1 razao agregada
##   median_tracks_per_turbine -- mediana entre turbinas -- menos sensivel a
##                                1 ou 2 turbinas muito ativas dominarem a
##                                media da classe
##   sd_tracks_per_turbine     -- desvio-padrao entre turbinas -- mostra se
##                                a atividade esta espalhada por todas as
##                                turbinas da classe ou concentrada nalgumas
##                                poucas (mesma logica de "a media sozinha
##                                nao chega" ja aplicada noutras seccoes
##                                deste projeto -- ver R/curtailment_bearing_sectors.R)
##   pct_turbines_active       -- % de turbinas da classe com >= 1 track
##                                nessa semana. Explica um efeito real
##                                confirmado no Bash (2026-08): com atividade
##                                concentrada nalgumas turbinas (ex: 457
##                                tracks numa semana, mas so' 6 das 52
##                                turbinas "flat" tiveram algum), a MEDIANA
##                                colapsa para 0 (< metade das turbinas
##                                ativas), apesar de haver atividade real e a
##                                media continuar positiva -- nao e' bug, e'
##                                so' o que a mediana faz com dados esparsos.
##                                pct_turbines_active mostra essa esparsidade
##                                diretamente, em vez de deixar a mediana
##                                "desaparecer" em silencio.
##
## Depende de: data.table, sf, RANN, ggplot2
##
## Uso:
##   source("R/track_terrain_temporal.R")
##   track_terrain_dt <- assign_track_terrain_class(track_dt, wtg, terrain_dt)
##   weekly_dt <- summarise_tracks_by_week_terrain(track_terrain_dt, terrain_dt) # terrain_dt AQUI e' o mesmo objeto classificado (classify_terrain()), precisa de TODAS as turbinas, nao so' as com tracks
##   plot_tracks_by_week_terrain(weekly_dt) # mean_tracks_per_turbine, com banda +/-1 SD (omissao)
##   plot_tracks_by_week_terrain(weekly_dt, metric = "n_tracks") # total bruto, sem ajustar ao nº de turbinas
##   plot_tracks_by_week_terrain(weekly_dt, metric = "pct_turbines_active") # % turbinas com >=1 track -- ve a esparsidade diretamente
##   plot_tracks_by_week_terrain(weekly_dt, facet = TRUE) # 1 painel por classe, escala Y livre
##


## 1. Turbina mais proxima (geometrica) + classe de terreno, por track ----

assign_track_terrain_class <- function(track_dt, wtg_sf, turbine_terrain_dt, wtg_id_col = "InternalNa") {

  wtg_coords <- sf::st_coordinates(wtg_sf)
  wtg_ids    <- wtg_sf[[wtg_id_col]]

  ## 1o registo de cada track (ordenado por tempo) -- mesma aproximacao "1o
  ## ponto = momento de referencia" de compute_curtailment_bearing()
  pts <- track_dt[, .(track_id, timestamp, utm_x, utm_y)]
  data.table::setorder(pts, track_id, timestamp)
  first_pt_dt <- pts[, .(
    first_timestamp = data.table::first(timestamp),
    utm_x = data.table::first(utm_x), utm_y = data.table::first(utm_y)
  ), by = track_id]

  ## turbina geometricamente mais proxima (KD-tree) -- NAO usa o campo
  ## turbine/NearestTurbine3d do proprio track_dt
  nn <- RANN::nn2(
    data  = as.matrix(data.table::data.table(x = wtg_coords[, "X"], y = wtg_coords[, "Y"])),
    query = as.matrix(first_pt_dt[, .(utm_x, utm_y)]),
    k = 1
  )
  first_pt_dt[, nearest_turbine        := wtg_ids[as.integer(nn$nn.idx[, 1])]]
  first_pt_dt[, nearest_turbine_dist_m := round(as.numeric(nn$nn.dists[, 1]), 1)]

  out <- merge(
    first_pt_dt, turbine_terrain_dt[, .(nearest_turbine = wtg_id, terrain_class)],
    by = "nearest_turbine", all.x = TRUE
  )

  out[, .(track_id, first_timestamp, nearest_turbine, nearest_turbine_dist_m, terrain_class)]
}


## 2. Contagem de tracks por bin de 7 dias x classe de terreno ----
##
## turbine_terrain_dt -- tabela COMPLETA de turbinas (saida de
## classify_terrain(), todas as turbinas do parque, nao so' as que tiveram
## tracks) -- obrigatoria, precisa dela para saber quantas turbinas cada
## classe tem e incluir as turbinas com 0 tracks numa dada semana no calculo
## da media/mediana/desvio-padrao entre turbinas (ver nota no topo do
## ficheiro) -- sem isto a media ficava sobrestimada (so' contaria turbinas
## que TIVERAM pelo menos 1 track nessa semana).

summarise_tracks_by_week_terrain <- function(track_terrain_dt, turbine_terrain_dt) {

  dt <- track_terrain_dt[!is.na(terrain_class)]
  if (nrow(dt) == 0L) {
    message("summarise_tracks_by_week_terrain(): sem tracks com terrain_class conhecida -- tabela vazia devolvida.")
    return(dt[, .(
      week_start = as.Date(character()), terrain_class = character(), n_turbines = integer(),
      n_tracks = integer(), mean_tracks_per_turbine = numeric(),
      median_tracks_per_turbine = numeric(), sd_tracks_per_turbine = numeric(),
      pct_turbines_active = numeric()
    )])
  }

  min_date <- min(as.Date(dt$first_timestamp))
  dt[, week_start := min_date + (as.integer(as.Date(first_timestamp) - min_date) %/% 7L) * 7L]

  ## contagem por TURBINA x semana -- nivel mais fino, agregado a classe so'
  ## depois de completar as turbinas com 0 tracks nessa semana (ver abaixo)
  per_turbine_week <- dt[, .(n_tracks = .N), by = .(nearest_turbine, week_start)]

  all_weeks <- seq(min(per_turbine_week$week_start), max(per_turbine_week$week_start), by = 7)
  turbine_class_dt <- turbine_terrain_dt[, .(nearest_turbine = wtg_id, terrain_class = as.character(terrain_class))]

  ## grelha completa: TODAS as turbinas x TODAS as semanas -- e' aqui que as
  ## turbinas sem NENHUM track numa semana entram com n_tracks=0, em vez de
  ## ficarem simplesmente ausentes (o que inflacionaria a media so' com as
  ## turbinas ativas nessa semana)
  full_turbine_week <- data.table::CJ(nearest_turbine = turbine_class_dt$nearest_turbine, week_start = all_weeks)
  full_turbine_week <- merge(full_turbine_week, turbine_class_dt, by = "nearest_turbine")
  full_turbine_week <- merge(full_turbine_week, per_turbine_week, by = c("nearest_turbine", "week_start"), all.x = TRUE)
  full_turbine_week[is.na(n_tracks), n_tracks := 0L]

  out <- full_turbine_week[, .(
    n_turbines                = .N,
    n_tracks                  = sum(n_tracks),
    mean_tracks_per_turbine   = round(mean(n_tracks), 2),
    median_tracks_per_turbine = round(stats::median(n_tracks), 2),
    sd_tracks_per_turbine     = round(stats::sd(n_tracks), 2),
    pct_turbines_active       = round(100 * sum(n_tracks > 0) / .N, 1)
  ), by = .(week_start, terrain_class)]

  out[, terrain_class := factor(terrain_class, levels = c("flat", "complex", "ridge"))]
  data.table::setorder(out, week_start, terrain_class)
  out[]
}


## 3. Grafico de linhas -- 1 linha por classe de terreno, ao longo do tempo --
##
## metric = "mean_tracks_per_turbine" (omissao) -- desenha tambem uma banda
## +/-1 desvio-padrao ENTRE TURBINAS (sd_tracks_per_turbine), truncada em 0
## (nao ha tracks negativos) -- mostra se a media de cada semana representa
## bem todas as turbinas da classe ou se esconde muita variacao entre elas.
## "n_tracks" (total bruto, sem ajustar ao numero de turbinas -- ver nota no
## topo do ficheiro sobre porque isto pode enganar), "median_tracks_per_turbine"
## (mais robusta a 1-2 turbinas muito ativas, mas colapsa para 0 quando menos
## de metade das turbinas da classe tem alguma atividade -- ver
## pct_turbines_active) e "pct_turbines_active" (% de turbinas da classe com
## >= 1 track -- mostra essa esparsidade diretamente) tambem disponiveis, sem
## banda (SD nao se aplica a mediana/percentagem).

plot_tracks_by_week_terrain <- function(weekly_dt,
                                        metric = c("mean_tracks_per_turbine", "n_tracks",
                                                  "median_tracks_per_turbine", "pct_turbines_active"),
                                        facet = FALSE) {

  metric <- match.arg(metric)

  if (!metric %in% names(weekly_dt)) {
    message(sprintf(
      "plot_tracks_by_week_terrain(): weekly_dt nao tem a coluna '%s' -- confirma que veio de summarise_tracks_by_week_terrain(). NULL devolvido.",
      metric
    ))
    return(NULL)
  }

  if (nrow(weekly_dt) == 0L) {
    message("plot_tracks_by_week_terrain(): sem dados -- NULL devolvido.")
    return(NULL)
  }

  y_lab <- switch(metric,
    n_tracks                  = "Number of tracks",
    mean_tracks_per_turbine   = "Mean tracks per turbine",
    median_tracks_per_turbine = "Median tracks per turbine",
    pct_turbines_active       = "% turbines active"
  )

  p <- ggplot(weekly_dt, aes(x = week_start, y = .data[[metric]], colour = terrain_class))

  if (metric == "mean_tracks_per_turbine" && "sd_tracks_per_turbine" %in% names(weekly_dt)) {
    p <- p + geom_ribbon(
      aes(
        ymin = pmax(0, .data[[metric]] - sd_tracks_per_turbine),
        ymax = .data[[metric]] + sd_tracks_per_turbine,
        fill = terrain_class
      ),
      colour = NA, alpha = 0.15
    ) +
      scale_fill_manual(values = c(flat = "#4daf4a", complex = "#ff7f00", ridge = "#e41a1c"), guide = "none")
  }

  p <- p +
    geom_line() +
    geom_point(size = 1) +
    scale_colour_manual(values = c(flat = "#4daf4a", complex = "#ff7f00", ridge = "#e41a1c")) +
    labs(
      x = "Week starting", y = y_lab, colour = "Terrain class",
      title = sprintf("Weekly %s by terrain class", tolower(y_lab))
    ) +
    theme_bw()

  # facet = TRUE -- separa em 3 paineis com escala Y livre -- util quando o
  # numero de turbinas por classe e' muito desigual (ex: caso real do Bash,
  # 2026-08: flat=52, complex=19, ridge=8), que pode fazer uma classe ficar
  # quase invisivel numa escala Y partilhada
  if (facet) p <- p + facet_wrap(~terrain_class, ncol = 1, scales = "free_y")

  p
}
