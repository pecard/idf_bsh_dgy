##
## Metricas biologicas de voo por especie: velocidade (6.1) e altura (6.2) ----
##
## Migrado de scripts_IDF/bio_flight_speed.R, bio_flight_height.R e
## bio_distrib_flight_height_speed_per_species.R.
##
## Os 3 scripts originais aplicavam filtros de qualidade DIFERENTES entre si
## para o que deveria ser a mesma base de registos fiaveis de especies
## prioritarias:
##   - bio_flight_speed.R: count>2, speed_ms em (1,100), SEM filtro de altura
##   - bio_flight_height.R: count>3, speed_ms em (1,100), SEM filtro de
##     altura (incluia alturas negativas -- ao contrario da metodologia ja
##     adotada no relatorio de performance dos incidentes, que exclui
##     height<0 e documenta isso no rodape da tabela: "*values below 0 m AGL
##     excluded; **minimum values are raw data bounds, reported to highlight
##     potential data quality issues already under investigation")
##   - bio_distrib_flight_height_speed_per_species.R (grafico): height>0,
##     speed_ms<100, SEM filtro de nº de pontos nem cota inferior de speed_ms
## Aqui os 3 usam a MESMA base filtrada (flight_metrics_base()), para as
## tabelas e o grafico serem diretamente comparaveis -- nao ha razao para a
## base do grafico ser diferente da base das tabelas do mesmo conceito.
## min_track_points por omissao fica no valor mais exigente dos 2 scripts
## originais (>3 pontos, aqui expresso como >=4).
##
## O nº de registos com altura negativa (antes do filtro height>=0) fica
## visivel separadamente via flight_height_qa() -- nao se descarta run
## silenciosamente, mesma logica do rodape acima.
##
## risk_height_lower/upper (zona de risco desenhada no grafico) reutilizam
## os parametros ja existentes no userSettings para a secção 6.3 (Risk per
## species) -- o mesmo conceito de zona de risco, sem duplicar limiares.
##
## Depende de: data.table, ggplot2
##
## Uso:
##   source("R/bio_flight_metrics.R")
##
##   flight_base_dt <- flight_metrics_base(track_dt, prioritysp)
##   speed_summary_dt  <- summarise_flight_speed(flight_base_dt)
##   height_summary_dt <- summarise_flight_height(flight_base_dt)
##   height_qa_dt      <- flight_height_qa(track_dt, prioritysp)
##   p_flight <- plot_flight_metrics_distribution(flight_base_dt, riskHeight_lower, riskHeight_upper)
##

## Base de registos fiaveis (track com pontos suficientes, velocidade e
## altura dentro de limites plausiveis), so' para especies prioritarias
flight_metrics_base <- function(track_dt, prioritysp, min_track_points = 4,
                                speed_ms_min = 1, speed_ms_max = 100) {

  dt <- track_dt[!is.na(spec) & spec %in% prioritysp, .(track_id, spec, speed_ms, height)]
  dt[, n_points := .N, by = track_id]

  dt[
    n_points >= min_track_points &
      !is.na(speed_ms) & speed_ms > speed_ms_min & speed_ms < speed_ms_max &
      !is.na(height) & height >= 0
  ]
}


## Velocidade de voo por especie (m/s) -- media, mediana, desvio-padrao,
## min/max, sobre a base fiavel (flight_metrics_base())
summarise_flight_speed <- function(flight_base_dt) {

  if (nrow(flight_base_dt) == 0L) {
    return(data.table::data.table(
      spec = character(), n = integer(), mean_speed_ms = numeric(), median_speed_ms = numeric(),
      sd_speed_ms = numeric(), min_speed_ms = numeric(), max_speed_ms = numeric()
    ))
  }

  out <- flight_base_dt[, .(
    n               = .N,
    mean_speed_ms   = round(mean(speed_ms), 2),
    median_speed_ms = round(median(speed_ms), 2),
    sd_speed_ms     = round(sd(speed_ms), 2),
    min_speed_ms    = round(min(speed_ms), 2),
    max_speed_ms    = round(max(speed_ms), 2)
  ), by = spec]
  data.table::setorder(out, -mean_speed_ms)
  out[]
}


## Altura de voo por especie (m AGL) -- media, mediana, desvio-padrao,
## min/max, sobre a base fiavel (flight_metrics_base(), ja com height>=0).
## risk_height_lower/risk_height_upper opcionais (mesma zona de risco
## desenhada em plot_flight_metrics_distribution()) -- quando indicados,
## acrescenta pct_in_risk_zone (% dos registos dessa especie com altura
## dentro da rotor-swept zone, [risk_height_lower, risk_height_upper]).
## Omitir para manter o comportamento antigo (sem esta coluna).
summarise_flight_height <- function(flight_base_dt, risk_height_lower = NULL, risk_height_upper = NULL) {

  if (nrow(flight_base_dt) == 0L) {
    out <- data.table::data.table(
      spec = character(), n = integer(), mean_height_m = numeric(), median_height_m = numeric(),
      sd_height_m = numeric(), min_height_m = numeric(), max_height_m = numeric()
    )
    if (!is.null(risk_height_lower) && !is.null(risk_height_upper)) out[, pct_in_risk_zone := numeric()]
    return(out)
  }

  out <- flight_base_dt[, .(
    n              = .N,
    mean_height_m  = round(mean(height), 1),
    median_height_m = round(median(height), 1),
    sd_height_m    = round(sd(height), 1),
    min_height_m   = round(min(height), 1),
    max_height_m   = round(max(height), 1)
  ), by = spec]

  if (!is.null(risk_height_lower) && !is.null(risk_height_upper)) {
    risk_pct <- flight_base_dt[, .(
      pct_in_risk_zone = round(100 * mean(height >= risk_height_lower & height <= risk_height_upper), 1)
    ), by = spec]
    out <- merge(out, risk_pct, by = "spec")
  }

  data.table::setorder(out, -mean_height_m)
  out[]
}


## Diagnostico de qualidade: quantos registos de especies prioritarias tem
## altura negativa (abaixo do solo -- fisicamente impossivel, indicador de
## erro de sensor/geometria), por especie -- ANTES do filtro height>=0 usado
## nas tabelas/grafico acima. Nao tenta corrigir nem explicar a causa, so'
## documenta a extensao do problema (mesma base de filtros que
## flight_metrics_base(), exceto o corte de altura)
flight_height_qa <- function(track_dt, prioritysp, min_track_points = 4,
                             speed_ms_min = 1, speed_ms_max = 100) {

  dt <- track_dt[!is.na(spec) & spec %in% prioritysp, .(track_id, spec, speed_ms, height)]
  dt[, n_points := .N, by = track_id]
  dt <- dt[
    n_points >= min_track_points &
      !is.na(speed_ms) & speed_ms > speed_ms_min & speed_ms < speed_ms_max &
      !is.na(height)
  ]

  if (nrow(dt) == 0L) {
    return(data.table::data.table(
      spec = character(), n_total = integer(), n_negative_height = integer(),
      pct_negative = numeric(), min_height_m = numeric()
    ))
  }

  out <- dt[, .(
    n_total           = .N,
    n_negative_height = sum(height < 0),
    min_height_m      = round(min(height), 1)
  ), by = spec]
  out[, pct_negative := round(100 * n_negative_height / n_total, 1)]
  data.table::setorder(out, -n_negative_height)
  out[]
}


## Histogramas de velocidade e altura, 1 linha por especie (abreviatura das
## iniciais), com a zona de risco (risk_height_lower/upper) destacada no
## painel de altura
plot_flight_metrics_distribution <- function(flight_base_dt, risk_height_lower = 50, risk_height_upper = 250) {

  make_abbr <- function(x) {
    vapply(strsplit(x, "[- ]"), function(words) paste0(toupper(substr(words, 1, 1)), collapse = ""), character(1))
  }

  dt <- data.table::copy(flight_base_dt)
  dt[, spec_abbr := make_abbr(spec)]

  # height pode ser lido como integer por fread() quando todos os valores do
  # ficheiro sao inteiros (speed_ms e' sempre double, calculado por divisao)
  # -- sem isto, melt() avisa (nao erro, so' aviso) que vai coagir a double
  dt[, `:=`(speed_ms = as.double(speed_ms), height = as.double(height))]

  long_dt <- data.table::melt(
    dt, id.vars = c("spec_abbr", "track_id"), measure.vars = c("speed_ms", "height"),
    variable.name = "metric", value.name = "value"
  )

  risk_zone <- data.table::data.table(
    metric = "height", xmin = risk_height_lower, xmax = risk_height_upper, ymin = -Inf, ymax = Inf
  )

  ggplot2::ggplot(long_dt, ggplot2::aes(value)) +
    ggplot2::geom_histogram(bins = 30, fill = "steelblue", color = "black") +
    ggplot2::facet_grid(spec_abbr ~ metric, scales = "free_x") +
    ggplot2::geom_rect(
      data = risk_zone,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE, fill = "red", alpha = 0.15
    ) +
    ggplot2::labs(
      x = NULL, y = "Frequency", title = "Distribution of flight speed and height for priority species",
      caption = sprintf(
        "Shaded band (height panel) marks the rotor-swept risk height zone, %g-%gm AGL.",
        risk_height_lower, risk_height_upper
      )
    ) +
    ggplot2::theme_minimal()
}
