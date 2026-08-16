##
## Cobertura geometrica turbina <-> unidade IDF (sobreposicao de buffers)
##
## Deriva, a partir das posicoes das turbinas (wtg) e das unidades IDF (idf),
## que unidade(s) cobrem cada turbina e em que grau, por sobreposicao de
## buffers circulares -- sem depender de nenhuma matriz manual. Serve de
## validacao cruzada face a ACWA_IDF_Coverage_Matrix.xlsx (matriz mantida
## manualmente, com a atribuicao operacional Primary/Secondary IDF por
## turbina).
##
## Metodo:
##   1. buffer de buffer_m a volta de cada turbina (por omissao,
##      idf_op_detection_range -- o mesmo raio de deteção operacional do IDF
##      ja usado noutras seccoes, nao um parametro novo)
##   2. buffer de buffer_m a volta de cada unidade IDF
##   3. para cada par turbina+unidade cujos buffers se sobrepoem, calcula a %
##      da AREA DO BUFFER DA TURBINA coberta pelo buffer dessa unidade
##   4. por turbina, a unidade com maior % fica "Primary" (geometrico), as
##      restantes "Secondary_1", "Secondary_2", ... por ordem decrescente de %
##
## A matriz manual e' a fonte operacional a usar a jusante (ex: para
## resolver que unidades checar na investigacao de fatalidades, ver
## R/fatality_track_investigation.R) -- esta analise geometrica e' um
## instrumento de validacao/QA, nao substitui a matriz.
##
## Depende de: sf, data.table
##
## Uso:
##   source("R/turbine_idf_coverage.R")
##
##   coverage_dt <- compute_turbine_idf_coverage(
##     wtg, idf, buffer_m = idf_op_detection_range,
##     wtg_id_col = "InternalNa", idf_id_col = "imaging_he"
##   )
##   coverage_wide_dt <- pivot_turbine_idf_coverage_wide(coverage_dt)
##
##   manual_dt <- readxl::read_xlsx(file.path(folder_input, turbine_idf_matrix_filename))
##   comparison_dt <- compare_turbine_idf_matrix(manual_dt, coverage_dt)
##


## 1. Overlap geometrico, formato longo (1 linha por par turbina+unidade que se sobrepoe) ----

compute_turbine_idf_coverage <- function(wtg_sf, idf_sf, buffer_m,
                                         wtg_id_col = "InternalNa", idf_id_col = "imaging_he") {

  wtg_buf <- sf::st_buffer(wtg_sf, buffer_m)
  idf_buf <- sf::st_buffer(idf_sf, buffer_m)

  wtg_buf_area <- as.numeric(sf::st_area(wtg_buf))

  hits <- sf::st_intersects(wtg_buf, idf_buf)

  res <- lapply(seq_len(nrow(wtg_buf)), function(i) {
    idf_idx <- hits[[i]]
    if (length(idf_idx) == 0L) return(NULL)

    pct <- vapply(idf_idx, function(j) {
      inter_geom <- suppressWarnings(sf::st_intersection(wtg_buf[i, ], idf_buf[j, ]))
      if (nrow(inter_geom) == 0L) return(0)
      100 * as.numeric(sf::st_area(inter_geom)) / wtg_buf_area[i]
    }, numeric(1))

    data.table::data.table(
      turbine      = wtg_buf[[wtg_id_col]][i],
      idf          = idf_buf[[idf_id_col]][idf_idx],
      pct_coverage = round(pct, 1)
    )
  })

  out <- data.table::rbindlist(res)
  data.table::setorder(out, turbine, -pct_coverage)
  out[]
}


## 2. Formato largo -- Primary/Secondary_1/Secondary_2/... por turbina ----

pivot_turbine_idf_coverage_wide <- function(coverage_dt) {

  if (nrow(coverage_dt) == 0L) return(data.table::data.table(turbine = character()))

  dt <- data.table::copy(coverage_dt)
  data.table::setorder(dt, turbine, -pct_coverage)
  dt[, rank := seq_len(.N), by = turbine]
  dt[, role := data.table::fifelse(rank == 1L, "Primary", paste0("Secondary_", rank - 1L))]

  idf_wide <- data.table::dcast(dt, turbine ~ role, value.var = "idf")
  pct_wide <- data.table::dcast(dt, turbine ~ role, value.var = "pct_coverage")
  pct_cols <- setdiff(names(pct_wide), "turbine")
  data.table::setnames(pct_wide, pct_cols, paste0(pct_cols, "_pct"))

  out <- merge(idf_wide, pct_wide, by = "turbine")

  role_levels <- unique(dt$role)
  rank_of <- function(r) if (r == "Primary") 0 else as.numeric(sub("Secondary_", "", r))
  role_order <- role_levels[order(vapply(role_levels, rank_of, numeric(1)))]
  col_order <- c("turbine", unlist(lapply(role_order, function(r) c(r, paste0(r, "_pct")))))
  data.table::setcolorder(out, intersect(col_order, names(out)))
  out[]
}


## 3. Comparacao com a matriz manual (ACWA_IDF_Coverage_Matrix.xlsx) ----
##
## manual_dt: lido diretamente do xlsx (colunas Site, `Turbine ID`, `Primary
## IDF`, `Secondary IDF(s)`) -- a coluna "Secondary IDF(s)" pode conter mais
## do que uma unidade separada por virgula, e' desdobrada numa linha por
## unidade antes de comparar.
##
## coverage_dt: formato LONGO de compute_turbine_idf_coverage() (nao o wide).
##
## match_status, por par turbina+unidade:
##   match           -- na matriz manual E na analise geometrica, com o
##                       mesmo papel (Primary/Secondary)
##   role_mismatch   -- presente nas duas, mas com papel diferente (ex:
##                       manual diz Primary, geometria da mais % a outra unidade)
##   manual_only     -- na matriz manual, sem sobreposicao geometrica de buffers
##   geometric_only  -- sobreposicao geometrica sem correspondencia na matriz manual

compare_turbine_idf_matrix <- function(manual_dt, coverage_dt) {

  manual <- data.table::as.data.table(manual_dt)
  data.table::setnames(
    manual,
    old = c("Turbine ID", "Primary IDF", "Secondary IDF(s)"),
    new = c("turbine", "manual_primary", "manual_secondary_raw"),
    skip_absent = TRUE
  )

  manual_primary <- manual[!is.na(manual_primary) & manual_primary != "",
    .(turbine, idf = manual_primary, manual_role = "Primary")]

  manual_secondary <- manual[
    !is.na(manual_secondary_raw) & manual_secondary_raw != "",
    .(idf = trimws(unlist(strsplit(manual_secondary_raw, ",")))), by = turbine
  ]
  if (nrow(manual_secondary) > 0L) manual_secondary[, manual_role := "Secondary"]
  else manual_secondary[, manual_role := character()]

  manual_long <- data.table::rbindlist(list(manual_primary, manual_secondary), use.names = TRUE)
  manual_long <- manual_long[!is.na(idf) & idf != ""]

  geo <- data.table::copy(coverage_dt)
  geo[, geo_role := data.table::fifelse(pct_coverage == max(pct_coverage), "Primary", "Secondary"), by = turbine]

  cmp <- merge(
    manual_long, geo[, .(turbine, idf, geo_role, pct_coverage)],
    by = c("turbine", "idf"), all = TRUE
  )
  cmp[, match_status := data.table::fcase(
    is.na(manual_role) & !is.na(geo_role), "geometric_only",
    !is.na(manual_role) & is.na(geo_role), "manual_only",
    manual_role == geo_role,               "match",
    default = "role_mismatch"
  )]

  data.table::setorder(cmp, turbine, -pct_coverage)
  cmp[]
}
