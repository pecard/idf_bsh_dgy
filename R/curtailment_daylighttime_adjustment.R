##
## Ajuste do horario diario de curtailment -- os limites (primeiro/ultimo
## curtailment do dia) estao a acompanhar o amanhecer/anoitecer, ou ficam
## sempre dentro de uma janela fixa mais estreita?
##
## Pedido do Paulo (2026-09): com os dias a encurtar (outono/inverno --
## amanhecer mais tarde, anoitecer mais cedo), sera' que a atividade das
## aves (e por isso os curtailments que ela despoleta) tambem se concentra
## cada vez mais perto do "nucleo" do dia, deixando as margens do dia (logo
## ao amanhecer, mesmo antes do anoitecer) sem curtailments reais? Se sim,
## isso suporta reajustar o horario diario de curtailment das turbinas
## envolvidas em incidentes com abutres/aguias (fatality_incidents,
## userSettings_BSH.R/userSettings_DGY.R -- SAO todos abutres/aguias nos 2
## parques, nao precisa de filtrar por especie) para uma janela fixa mais
## favoravel ao cliente (ex: 7h-18h), sem comprometer a seguranca -- desde
## que NENHUM dia do periodo revisto tenha tido curtailment fora dessa
## janela (ver summarise_curtailment_window_coverage(), a verificacao
## decisiva -- uma media/mediana favoravel NAO chega, 1 dia fora da janela
## proposta e' um argumento contra).
##
## So' usa curtailments (curtl_dt) -- NAO cruza com heartbeats/SCADA (ao
## contrario de R/offline_curtailment_check.R) nem com tracks -- pedido
## explicito do Paulo ("apenas a evolucao temporal dos primeiros e dos
## ultimos curtailments de cada dia").
##
## Depende de: data.table, ggplot2
## (build_daylight_calendar(), R/availability_daylight.R, ja' fornece
## sunrise/sunset por dia -- sourced separadamente, nao redefinido aqui)
##
## Uso (depois de uma corrida normal de IDF_analysis.R/IDF_monthly_report.R
## -- reutiliza curtl_dt, daylight_cal, fatality_incidents ja' em memoria;
## ver tambem explore_curtailment_daylighttime_adjustment.R):
##
##   source("R/curtailment_daylighttime_adjustment.R")
##   window_start <- max(curtl_dt$start) %m-% months(6) # %m-%, NAO "-" (ver nota em R/monthly_report_utils.R -- "-" com Period de meses pode devolver NA por overflow de dia-do-mes)
##
##   window_end <- max(curtl_dt$start)
##
##   # Farm-wide (todas as turbinas) -- padrao geral
##   daily_dt <- daily_curtailment_bounds(curtl_dt, window_start, window_end, tz = proj_timezone)
##   daily_dt <- join_curtailment_bounds_daylight(daily_dt, daylight_cal)
##   p <- plot_daily_curtailment_bounds(daily_dt, proposed_start_clock = "07:00", proposed_end_clock = "18:00")
##   coverage_dt <- summarise_curtailment_window_coverage(daily_dt, "07:00", "18:00", window_start, window_end, tz = proj_timezone)
##   violations_dt <- list_curtailment_window_violations(daily_dt, "07:00", "18:00")
##   trend_dt <- test_curtailment_gap_trend(daily_dt)
##
##   # So' as turbinas de incidentes com abutres/aguias -- o alvo real da recomendacao
##   critical_turbines <- unique(fatality_incidents$turbine)
##   daily_dt_critical <- daily_curtailment_bounds(curtl_dt, window_start, window_end, tz = proj_timezone, turbines = critical_turbines)
##   daily_dt_critical <- join_curtailment_bounds_daylight(daily_dt_critical, daylight_cal)
##   p_critical <- plot_daily_curtailment_bounds(daily_dt_critical, proposed_start_clock = "07:00", proposed_end_clock = "18:00")
##   coverage_dt_critical <- summarise_curtailment_window_coverage(daily_dt_critical, "07:00", "18:00", window_start, window_end, tz = proj_timezone)
##


## 1. Primeiro/ultimo curtailment de cada dia (opcionalmente so' de um
## subconjunto de turbinas) ----
##
## turbines = NULL (omissao) -- todas as turbinas do parque (padrao
## farm-wide, para caracterizar o padrao geral de atividade das aves).
## turbines = um vector de IDs (ex: unique(fatality_incidents$turbine)) --
## restringe as turbinas de interesse concreto para a recomendacao (as
## envolvidas em incidentes).
##
## Um dia SEM nenhum curtailment fica de fora da tabela (nao ha' "primeiro/
## ultimo" para calcular) -- NAO e' o mesmo que 0 curtailments ser um sinal
## de seguranca; e' so' um dia sem dados para este calculo especifico. Dias
## sem curtailment nenhum sao contabilizados a parte (n_days_total vs
## n_days_with_curtailment) em summarise_curtailment_window_coverage().
##
## first_curtailment_start -- inicio do curtailment mais cedo do dia (a
## protecao "liga"). last_curtailment_end -- fim do curtailment mais tardio
## do dia (a protecao "desliga", olhando ao fim do sinal, nao so' ao seu
## inicio -- 2 curtailments podem sobrepor-se, o que importa e' quando a
## ultima paragem realmente terminou).

daily_curtailment_bounds <- function(curtl_dt, window_start, window_end, tz, turbines = NULL) {

  dt <- curtl_dt[start >= window_start & start <= window_end]
  if (!is.null(turbines)) dt <- dt[turbine %in% turbines]

  if (nrow(dt) == 0L) {
    message("daily_curtailment_bounds(): sem curtailments no periodo/turbinas pedidas -- tabela vazia devolvida.")
    return(dt[, .(
      date = as.Date(character()), first_curtailment_start = as.POSIXct(character()),
      last_curtailment_end = as.POSIXct(character()), n_curtailments = integer()
    )])
  }

  # as.Date(POSIXct) sem tz= assume UTC -- desloca a data 1 dia para tras
  # em fusos positivos (Asia/Samarkand, UTC+5); mesmo cuidado documentado em
  # build_daylight_calendar(), R/availability_daylight.R.
  dt[, date := as.Date(start, tz = tz)]

  out <- dt[, .(
    first_curtailment_start = min(start),
    last_curtailment_end    = max(end),
    n_curtailments          = .N
  ), by = date]

  data.table::setorder(out, date)
  out[]
}


## 2. Junta com o calendario de luz do dia (sunrise/sunset por dia) ----
##
## daylight_cal -- saida de build_daylight_calendar() (R/availability_daylight.R),
## colunas date/sunrise/sunset -- tem de cobrir (pelo menos) o mesmo
## intervalo de datas de daily_bounds_dt.
##
## twilight_cal = NULL (omissao) -- opcional, saida de build_twilight_calendar()
## (funcao 2b, abaixo), colunas date/dawn/dusk -- so' usado para desenhar a
## linha azul de crepusculo no grafico (funcao 3); nao entra em nenhum
## calculo de gap/tendencia, so' referencia visual.
##
## gap_sunrise_min = first_curtailment_start - sunrise, em minutos --
## POSITIVO significa que o 1o curtailment do dia aconteceu DEPOIS do
## amanhecer (a margem inicial do dia ficou sem curtailment real); um valor
## a CRESCER ao longo do periodo (dias a encurtar) e' o sinal que suporta a
## hipotese do Paulo. gap_sunset_min = sunset - last_curtailment_end, mesma
## logica para o fim do dia (POSITIVO = ultimo curtailment terminou ANTES
## do anoitecer).

join_curtailment_bounds_daylight <- function(daily_bounds_dt, daylight_cal, twilight_cal = NULL) {

  out <- merge(daily_bounds_dt, daylight_cal[, .(date, sunrise, sunset)], by = "date", all.x = TRUE)
  if (!is.null(twilight_cal)) out <- merge(out, twilight_cal[, .(date, dawn, dusk)], by = "date", all.x = TRUE)

  out[, gap_sunrise_min := round(as.numeric(difftime(first_curtailment_start, sunrise, units = "mins")), 1)]
  out[, gap_sunset_min  := round(as.numeric(difftime(sunset, last_curtailment_end, units = "mins")), 1)]
  out[, daylight_mins   := round(as.numeric(difftime(sunset, sunrise, units = "mins")), 1)]

  data.table::setorder(out, date)
  out[]
}


## 2b. Calendario de crepusculo civil (dawn/dusk), por dia -- so' para a
## linha azul de referencia no grafico (funcao 3) ----
##
## Modulo AUTONOMO -- NAO le nem altera build_daylight_calendar()
## (R/availability_daylight.R, usada em todo o resto do pipeline para
## classificar slots dia/noite) -- crepusculo civil e' so' uma referencia
## visual extra pedida aqui (inspirada num grafico semelhante de outro
## projeto -- ver comentario na funcao 3), nao um novo criterio de
## classificacao dia/noite. Mesma logica de tz de build_daylight_calendar()
## (as.Date(POSIXct) sem tz= desloca a data em fusos positivos).

build_twilight_calendar <- function(start_date, end_date, lat, lon, tz) {

  dates <- seq(as.Date(start_date, tz = tz), as.Date(end_date, tz = tz), by = "day")

  twi <- as.data.table(
    suncalc::getSunlightTimes(date = dates, lat = lat, lon = lon, keep = c("dawn", "dusk"), tz = tz)
  )

  twi[, .(date, dawn, dusk)]
}


## 3. Grafico -- faixa de luz do dia + barra vertical diaria (1o ao ultimo
## curtailment), estilo "sun/twilight band" ----
##
## Desenho pedido pelo Paulo (2026-09), a partir de um grafico semelhante
## usado noutro projeto (Nota Tecnica Brasil, PACAAL): faixa amarela =
## intervalo de luz solar (sunrise-sunset), linha azul = limites do
## crepusculo civil (dawn/dusk, build_twilight_calendar() acima), fundo
## escuro = noite, barra vertical escura + circulos brancos nas pontas = o
## intervalo do 1o ao ultimo curtailment desse dia (equivalente ao
## "intervalo diario de observacoes" do grafico original, aqui aplicado a
## curtailments em vez de deteções). Substitui a 1a versao deste grafico (4
## series de linha/ponto) -- o mesmo conteudo, mas a leitura de "o
## curtailment ficou dentro do nucleo do dia de luz, ou chegou perto das
## bordas" fica imediata visualmente, sem precisar de comparar 4 legendas.
##
## twilight_cal (dawn/dusk) e' OPCIONAL -- se daily_bounds_daylight_dt nao
## tiver essas colunas (join_curtailment_bounds_daylight() chamado sem
## twilight_cal), a linha azul e' simplesmente omitida, resto do grafico
## igual.
##
## proposed_start_clock/proposed_end_clock -- "HH:MM", a janela fixa a
## testar (ex: "07:00"/"18:00", pedido do Paulo) -- linhas verdes
## horizontais; window_marker_date -- opcional, 1 linha vertical branca
## (ex: inicio da janela dos "ultimos 6 meses" revistos, se
## daily_bounds_daylight_dt cobrir um periodo mais longo para dar contexto
## -- mesmo papel da linha branca "inicio do estudo" no grafico original).
##
## Nota de fidelidade ao grafico original: a textura de grelha tracejada
## fina, visivel tanto sobre a faixa amarela como sobre o fundo escuro no
## grafico de referencia, NAO foi replicada aqui (precisaria de desenhar a
## grelha por cima do preenchimento em vez de atras, via geom_tile em vez
## do tema automatico do ggplot2) -- so' a paleta/logica de cores (dia/
## noite/crepusculo/barra) foi reproduzida, por simplicidade.

plot_daily_curtailment_bounds <- function(daily_bounds_daylight_dt, proposed_start_clock = NULL, proposed_end_clock = NULL,
                                          window_marker_date = NULL, date_breaks = "1 week") {

  if (nrow(daily_bounds_daylight_dt) == 0L) {
    message("plot_daily_curtailment_bounds(): sem dados -- NULL devolvido.")
    return(NULL)
  }

  to_decimal_hour <- function(x) lubridate::hour(x) + lubridate::minute(x) / 60 + lubridate::second(x) / 3600
  clock_to_decimal <- function(hhmm) {
    if (is.null(hhmm)) return(NULL)
    parts <- as.numeric(strsplit(hhmm, ":")[[1]])
    parts[1] + parts[2] / 60
  }

  dt <- data.table::copy(daily_bounds_daylight_dt)
  dt[, `:=`(
    sunrise_h = to_decimal_hour(sunrise), sunset_h = to_decimal_hour(sunset),
    first_h   = to_decimal_hour(first_curtailment_start), last_h = to_decimal_hour(last_curtailment_end)
  )]
  has_twilight <- all(c("dawn", "dusk") %in% names(dt))
  if (has_twilight) dt[, `:=`(dawn_h = to_decimal_hour(dawn), dusk_h = to_decimal_hour(dusk))]

  proposed_start_h <- clock_to_decimal(proposed_start_clock)
  proposed_end_h   <- clock_to_decimal(proposed_end_clock)

  p <- ggplot(dt, aes(x = date)) +
    # faixa de luz do dia (sunrise-sunset), desenhada 1a -- fundo do painel
    # (theme, abaixo) fica escuro fora desta faixa, simulando a noite
    geom_ribbon(aes(ymin = sunrise_h, ymax = sunset_h), fill = "#d9c94a", colour = NA)

  if (has_twilight) {
    p <- p +
      geom_line(aes(y = dawn_h), colour = "#3a5bbf", linewidth = 0.5) +
      geom_line(aes(y = dusk_h), colour = "#3a5bbf", linewidth = 0.5)
  }

  # barra vertical diaria (1o ao ultimo curtailment) + circulos brancos nas
  # pontas -- so' para dias com pelo menos 1 curtailment (first_h/last_h
  # nao-NA, ja' garantido por daily_curtailment_bounds() so' incluir esses
  # dias)
  p <- p +
    geom_segment(aes(xend = date, y = first_h, yend = last_h), colour = "#2b2b2b", linewidth = 0.7) +
    geom_point(aes(y = first_h), shape = 21, fill = "white", colour = "#2b2b2b", size = 1.6, stroke = 0.4) +
    geom_point(aes(y = last_h),  shape = 21, fill = "white", colour = "#2b2b2b", size = 1.6, stroke = 0.4)

  if (!is.null(proposed_start_h)) p <- p + geom_hline(yintercept = proposed_start_h, colour = "#2f9e56", linetype = "dashed", linewidth = 0.6)
  if (!is.null(proposed_end_h))   p <- p + geom_hline(yintercept = proposed_end_h,   colour = "#2f9e56", linetype = "dashed", linewidth = 0.6)
  if (!is.null(window_marker_date)) p <- p + geom_vline(xintercept = as.numeric(as.Date(window_marker_date)), colour = "white", linewidth = 0.8)

  p +
    scale_x_date(date_breaks = date_breaks, date_labels = "%d %b %Y", expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 24), breaks = seq(0, 24, 4), labels = function(x) sprintf("%02d:00", x), expand = c(0, 0)) +
    labs(
      x = "Date", y = "Time of day",
      title = "Daily first/last curtailment vs. daylight",
      subtitle = paste(
        "Yellow: daylight (sunrise-sunset).",
        if (has_twilight) "Blue: civil twilight (dawn/dusk)." else NULL,
        if (!is.null(proposed_start_h)) sprintf("Dashed green: proposed fixed window (%s-%s).", proposed_start_clock, proposed_end_clock) else NULL
      )
    ) +
    theme_minimal(base_size = 9) +
    theme(
      panel.background = element_rect(fill = "#16324a", colour = NA),
      panel.grid = element_line(colour = "white", linewidth = 0.15, linetype = "dotted"),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6.5),
      plot.subtitle = element_text(size = 7)
    )
}


## 4. A verificacao decisiva -- alguma vez, nalgum dia do periodo, o 1o
## curtailment foi ANTES do inicio da janela proposta, ou o ultimo DEPOIS
## do fim? ----
##
## n_days_total = todos os dias do periodo (window_start..window_end),
## n_days_with_curtailment = quantos tiveram pelo menos 1 curtailment (so'
## esses entram em daily_bounds_dt/tem 1o-ultimo definido) -- um dia SEM
## curtailment nenhum nao e' uma violacao da janela proposta (nao houve
## nada fora dela), mas tambem nao e' evidencia a favor -- fica reportado a
## parte para nao inflacionar silenciosamente a "% de dias dentro da
## janela" so' com dias sem atividade nenhuma.
##
## n_days_violation_start/end -- contagem e lista das datas onde a janela
## proposta TERIA deixado bird activity real sem curtailment (a decisao
## "e' seguro" ou "nao e' seguro" assenta nestas contagens, nao nas medias
## de gap_sunrise_min/gap_sunset_min).

summarise_curtailment_window_coverage <- function(daily_bounds_daylight_dt, proposed_start_clock, proposed_end_clock,
                                                   window_start, window_end, tz) {

  n_days_total <- length(seq(as.Date(window_start, tz = tz), as.Date(window_end, tz = tz), by = "day"))
  n_days_with_curtailment <- nrow(daily_bounds_daylight_dt)

  if (n_days_with_curtailment == 0L) {
    return(data.table::data.table(
      n_days_total = n_days_total, n_days_with_curtailment = 0L,
      n_days_violation_start = NA_integer_, n_days_violation_end = NA_integer_,
      pct_days_within_window = NA_real_,
      mean_gap_sunrise_min = NA_real_, mean_gap_sunset_min = NA_real_
    ))
  }

  to_decimal_hour <- function(x) lubridate::hour(x) + lubridate::minute(x) / 60 + lubridate::second(x) / 3600
  clock_to_decimal <- function(hhmm) {
    parts <- as.numeric(strsplit(hhmm, ":")[[1]])
    parts[1] + parts[2] / 60
  }
  proposed_start_h <- clock_to_decimal(proposed_start_clock)
  proposed_end_h   <- clock_to_decimal(proposed_end_clock)

  dt <- data.table::copy(daily_bounds_daylight_dt)
  dt[, `:=`(first_h = to_decimal_hour(first_curtailment_start), last_h = to_decimal_hour(last_curtailment_end))]
  dt[, violation_start := first_h < proposed_start_h]
  dt[, violation_end    := last_h  > proposed_end_h]

  data.table::data.table(
    n_days_total               = n_days_total,
    n_days_with_curtailment    = n_days_with_curtailment,
    n_days_violation_start     = sum(dt$violation_start),
    n_days_violation_end       = sum(dt$violation_end),
    pct_days_within_window     = round(100 * mean(!dt$violation_start & !dt$violation_end), 1),
    mean_gap_sunrise_min       = round(mean(dt$gap_sunrise_min, na.rm = TRUE), 1),
    mean_gap_sunset_min        = round(mean(dt$gap_sunset_min, na.rm = TRUE), 1)
  )
}


## 4b. Datas concretas de violacao -- para inspecionar caso a caso (que
## dia, que curtailment, que turbina/especie estava a disparar tao cedo/
## tarde) antes de assumir a janela proposta como segura ----

list_curtailment_window_violations <- function(daily_bounds_daylight_dt, proposed_start_clock, proposed_end_clock) {

  to_decimal_hour <- function(x) lubridate::hour(x) + lubridate::minute(x) / 60 + lubridate::second(x) / 3600
  clock_to_decimal <- function(hhmm) {
    parts <- as.numeric(strsplit(hhmm, ":")[[1]])
    parts[1] + parts[2] / 60
  }
  proposed_start_h <- clock_to_decimal(proposed_start_clock)
  proposed_end_h   <- clock_to_decimal(proposed_end_clock)

  dt <- data.table::copy(daily_bounds_daylight_dt)
  dt[, `:=`(first_h = to_decimal_hour(first_curtailment_start), last_h = to_decimal_hour(last_curtailment_end))]
  dt[, violation := data.table::fcase(
    first_h < proposed_start_h & last_h > proposed_end_h, "Both ends",
    first_h < proposed_start_h, "Before window start",
    last_h  > proposed_end_h,   "After window end",
    default = NA_character_
  )]

  out <- dt[!is.na(violation), .(date, first_curtailment_start, last_curtailment_end, n_curtailments, violation)]
  data.table::setorder(out, date)
  out[]
}


## 5. Tendencia -- os gaps (sunrise/sunset) estao a crescer a medida que os
## dias encurtam? ----
##
## Regressao linear simples de gap_sunrise_min/gap_sunset_min contra
## daylight_mins (duracao do dia, decrescente ao longo do outono/inverno)
## -- um coeficiente NEGATIVO (gap cresce quando daylight_mins desce, i.e.
## quando os dias encurtam) e estatisticamente significativo (p < 0.05)
## suporta a hipotese do Paulo. Exploratorio/simples de proposito (1
## regressao linear, sem controlar por outros fatores) -- serve para
## quantificar a tendencia visivel no grafico, nao para uma alegacao causal
## forte.

test_curtailment_gap_trend <- function(daily_bounds_daylight_dt) {

  fit_one <- function(y) {
    dt <- daily_bounds_daylight_dt[!is.na(get(y)) & !is.na(daylight_mins)]
    if (nrow(dt) < 3L) return(list(slope_min_per_daylight_min = NA_real_, r = NA_real_, p_value = NA_real_, n = nrow(dt)))
    fit <- stats::lm(dt[[y]] ~ dt$daylight_mins)
    s <- summary(fit)
    list(
      slope_min_per_daylight_min = round(unname(stats::coef(fit)[2]), 3),
      r       = round(sqrt(s$r.squared) * sign(stats::coef(fit)[2]), 3),
      p_value = round(unname(s$coefficients[2, 4]), 4),
      n       = nrow(dt)
    )
  }

  rbind(
    data.table::data.table(metric = "gap_sunrise_min", as.data.table(fit_one("gap_sunrise_min"))),
    data.table::data.table(metric = "gap_sunset_min",  as.data.table(fit_one("gap_sunset_min")))
  )
}
