##
## 10.1 -- Padroes espaciais e temporais de curtailments por cluster de
## turbinas, e contributo marginal das turbinas em investigacao de
## fatalidade dentro do seu cluster
##
## Usa os clusters de R/turbine_spatial_clusters.R (estatisticos, por
## distancia, OU manuais, por setor -- mesma estrutura turbine/cluster_id
## para as 2 vias, por isso as funcoes abaixo servem para qualquer uma sem
## alteracao).
##
## Objetivo (ver conversa com o Paulo): perceber se uma turbina de
## incidente (fatality_incidents$turbine) esta numa "zona critica". Sao 2
## perguntas DIFERENTES, por isso reportam-se as 2:
##   (a) e' ela propria uma fatia grande do seu cluster (pct_of_cluster)?
##   (b) o proprio cluster e' dos mais ativos da quinta (cluster_rank)?
## Uma turbina de incidente pode ser "a maior fatia de um cluster pouco
## ativo" ou "uma fatia pequena de um cluster muito ativo" -- conclusoes
## diferentes para efeitos de "zona critica".
##
## Periodo: por omissao usa TODO o periodo coberto por curtl_dt_unfilt (nao
## so' a janela do relatorio) -- ajustavel via curtl_cluster_date_from/to em
## userSettings_BSH.R se for preciso restringir.
##
## Depende de: data.table, lubridate, ggplot2
##
## Uso:
##   source("R/curtailment_cluster_patterns.R")
##
##   curtl_cl_dt        <- join_curtailments_to_clusters(curtl_dt_unfilt, cluster_dt)
##   cluster_summary     <- summarise_cluster_curtailments(curtl_cl_dt)
##   cluster_weekly_dt   <- summarise_cluster_curtailments_weekly(curtl_cl_dt)
##   marginal_dt         <- summarise_turbine_marginal_contribution(curtl_cl_dt, fatality_incidents$turbine)
##   perm_dt             <- permutation_test_marginal_contribution_all(curtl_cl_dt, fatality_incidents$turbine)
##
##   p1 <- plot_cluster_curtailments_total(cluster_summary, highlight_clusters = marginal_dt$cluster_id)
##   p2 <- plot_cluster_curtailments_weekly(cluster_weekly_dt, highlight_clusters = marginal_dt$cluster_id)
##

## 1. Juntar cluster_id aos curtailments, por turbina ----

join_curtailments_to_clusters <- function(curtl_dt, cluster_dt, date_from = NULL, date_to = NULL) {

  # curtl_dt_unfilt (a versao tipica passada aqui) pode nao ser data.table
  # -- read_curtailments_data() devolve um tibble (cadeia dplyr), so' fica
  # data.table se vier da cache .fst (fst::read_fst(as.data.table=TRUE)).
  # as.data.table() e' no-op se ja for data.table -- mesma cautela ja usada
  # em "curtl_dt <- as.data.table(curtl_dt_unfilt)[...]" na secção 0.
  out <- data.table::as.data.table(curtl_dt)
  if (!is.null(date_from)) out <- out[start >= date_from]
  if (!is.null(date_to))   out <- out[start <= date_to]

  out[cluster_dt, on = "turbine", cluster_id := i.cluster_id]
  out[]
}


## 2. Totais por cluster e por turbina dentro do cluster (periodo completo)
##    -- turbinas/curtailments sem cluster atribuido (turbina fora de todos
##    os clusters, ex: nao coberta pelos setores manuais) ficam de fora
summarise_cluster_curtailments <- function(curtl_cl_dt) {

  dt <- curtl_cl_dt[!is.na(cluster_id)]

  by_turbine <- dt[, .(n = .N), by = .(cluster_id, turbine)]
  if (nrow(by_turbine) > 0L) by_turbine[, pct_of_cluster := round(100 * n / sum(n), 1), by = cluster_id]
  data.table::setorder(by_turbine, cluster_id, -n)

  by_cluster <- dt[, .(
    n_turbines = data.table::uniqueN(turbine),
    n_total    = .N
  ), by = cluster_id]
  if (nrow(by_cluster) > 0L) by_cluster[, mean_per_turbine := round(n_total / n_turbines, 1)]
  data.table::setorder(by_cluster, -n_total)
  by_cluster[, cluster_rank := seq_len(.N)]

  list(by_turbine = by_turbine[], by_cluster = by_cluster[])
}


## 3. Serie semanal por cluster e por turbina ----

summarise_cluster_curtailments_weekly <- function(curtl_cl_dt, unit = "week") {

  dt <- curtl_cl_dt[!is.na(cluster_id)]
  if (nrow(dt) == 0L) {
    return(data.table::data.table(cluster_id = character(), turbine = character(),
                                   period = as.Date(character()), n = integer()))
  }
  dt <- data.table::copy(dt)
  # as.Date() sem tz= usa UTC por omissao e desloca o limite do periodo --
  # mesma correcao ja aplicada em R/curtailment_response_timeline.R
  dt[, period := lubridate::floor_date(as.Date(start, tz = attr(start, "tzone")), unit = unit)]

  out <- dt[, .(n = .N), by = .(cluster_id, turbine, period)]
  data.table::setorder(out, cluster_id, turbine, period)
  out[]
}


## 4. Contributo marginal das turbinas de interesse (ex: em investigacao de
##    fatalidade) dentro do seu cluster -- quota % periodo completo + quota
##    semanal (mediana, para ver se e' consistente ou varia muito semana a
##    semana) + ranking do proprio cluster entre todos os clusters
summarise_turbine_marginal_contribution <- function(curtl_cl_dt, turbines_of_interest, unit = "week") {

  cluster_summary <- summarise_cluster_curtailments(curtl_cl_dt)
  weekly_dt        <- summarise_cluster_curtailments_weekly(curtl_cl_dt, unit = unit)

  res <- lapply(turbines_of_interest, function(tb) {
    row <- cluster_summary$by_turbine[turbine == tb]
    if (nrow(row) == 0L) {
      return(data.table::data.table(
        turbine = tb, cluster_id = NA_character_, n_turbines_in_cluster = NA_integer_,
        n_total = NA_integer_, pct_of_cluster = NA_real_,
        cluster_rank = NA_integer_, n_clusters = NA_integer_,
        median_weekly_pct_of_cluster = NA_real_
      ))
    }
    cl_id  <- row$cluster_id
    cl_row <- cluster_summary$by_cluster[cluster_id == cl_id]

    weekly_cl       <- weekly_dt[cluster_id == cl_id]
    weekly_cl_total <- weekly_cl[, .(cluster_n = sum(n)), by = period]
    weekly_tb       <- weekly_cl[turbine == tb, .(period, tb_n = n)]
    weekly_join     <- merge(weekly_cl_total, weekly_tb, by = "period", all.x = TRUE)
    weekly_join[is.na(tb_n), tb_n := 0L]
    weekly_join[, pct := round(100 * tb_n / cluster_n, 1)]

    data.table::data.table(
      turbine = tb, cluster_id = cl_id,
      n_turbines_in_cluster = data.table::uniqueN(cluster_summary$by_turbine[cluster_id == cl_id, turbine]),
      n_total = row$n, pct_of_cluster = row$pct_of_cluster,
      cluster_rank = cl_row$cluster_rank, n_clusters = nrow(cluster_summary$by_cluster),
      median_weekly_pct_of_cluster = round(stats::median(weekly_join$pct, na.rm = TRUE), 1)
    )
  })

  data.table::rbindlist(res)
}


## 5. Validacao estatistica: teste de permutacao para o contributo marginal
##    ----
##    H0: os curtailments do cluster distribuem-se UNIFORMEMENTE pelas
##    turbinas do cluster (cada curtailment tem a mesma probabilidade de
##    ocorrer em qualquer turbina do cluster, independentemente de quantas
##    turbinas o cluster tem). Reembaralha n_perm vezes (multinomial) e ve
##    onde cai a contagem OBSERVADA da turbina de interesse nessa
##    distribuicao ao acaso -- p_value pequeno = a turbina concentra MAIS
##    curtailments do que seria de esperar so' pela sua posicao no cluster
##    (nao e' so' "o cluster e' ativo", a propria turbina destaca-se dentro
##    dele)
permutation_test_marginal_contribution <- function(curtl_cl_dt, turbine_id, n_perm = 999, seed = 1) {

  cl_id <- unique(curtl_cl_dt[turbine == turbine_id & !is.na(cluster_id), cluster_id])
  if (length(cl_id) == 0L) {
    return(data.table::data.table(
      turbine = turbine_id, cluster_id = NA_character_, n_cluster_turbines = NA_integer_,
      n_cluster_total = NA_integer_, observed_n = NA_integer_, observed_pct = NA_real_,
      expected_pct_uniform = NA_real_, p_value_gt_uniform = NA_real_
    ))
  }
  cl_id <- cl_id[1]

  cluster_dt         <- curtl_cl_dt[cluster_id == cl_id]
  cluster_turbines    <- sort(unique(cluster_dt$turbine))
  n_cluster_turbines <- length(cluster_turbines)
  n_cluster_total    <- nrow(cluster_dt)
  observed_n          <- cluster_dt[turbine == turbine_id, .N]

  set.seed(seed)
  perm          <- stats::rmultinom(n_perm, size = n_cluster_total, prob = rep(1, n_cluster_turbines))
  turbine_idx   <- match(turbine_id, cluster_turbines)
  perm_turbine  <- perm[turbine_idx, ]

  data.table::data.table(
    turbine              = turbine_id,
    cluster_id           = cl_id,
    n_cluster_turbines   = n_cluster_turbines,
    n_cluster_total      = n_cluster_total,
    observed_n           = observed_n,
    observed_pct         = round(100 * observed_n / n_cluster_total, 1),
    expected_pct_uniform = round(100 / n_cluster_turbines, 1),
    p_value_gt_uniform   = round(mean(perm_turbine >= observed_n), 4)
  )
}


permutation_test_marginal_contribution_all <- function(curtl_cl_dt, turbine_ids, n_perm = 999, seed = 1) {
  data.table::rbindlist(lapply(turbine_ids, function(tb) {
    permutation_test_marginal_contribution(curtl_cl_dt, tb, n_perm = n_perm, seed = seed)
  }))
}


## 6. Plots -- totais por cluster (periodo completo) e evolucao semanal por
##    cluster, com os clusters das turbinas de interesse destacados a
##    vermelho (highlight_clusters) para se ver logo a olho onde ficam
##    dentro da distribuicao geral

plot_cluster_curtailments_total <- function(cluster_summary, highlight_clusters = NULL, title = "Curtailments by cluster (full period)") {

  dt <- data.table::copy(cluster_summary$by_cluster)
  data.table::setorder(dt, -n_total)
  dt[, cluster_id := factor(cluster_id, levels = cluster_id)]
  dt[, highlight := cluster_id %in% highlight_clusters]

  ggplot2::ggplot(dt, ggplot2::aes(x = cluster_id, y = n_total, fill = highlight)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = c(`TRUE` = "firebrick", `FALSE` = "steelblue"), guide = "none") +
    ggplot2::labs(x = "Cluster", y = "Total curtailments", title = title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}


plot_cluster_curtailments_weekly <- function(cluster_weekly_dt, highlight_clusters = NULL, title = "Weekly curtailments by cluster") {

  dt <- cluster_weekly_dt[, .(n = sum(n)), by = .(cluster_id, period)]
  dt[, is_highlight := cluster_id %in% highlight_clusters]

  ggplot2::ggplot(dt, ggplot2::aes(x = period, y = n, color = cluster_id, linewidth = is_highlight, group = cluster_id)) +
    ggplot2::geom_line() +
    ggplot2::scale_linewidth_manual(values = c(`TRUE` = 1.3, `FALSE` = 0.5), guide = "none") +
    ggplot2::labs(x = "Week", y = "Curtailments", color = "Cluster", title = title) +
    ggplot2::theme_minimal()
}
