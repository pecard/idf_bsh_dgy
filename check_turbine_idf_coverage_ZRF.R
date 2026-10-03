##
## Confirma, por geometria (sobreposicao de buffers), que unidade(s) IDF
## cobrem uma turbina de interesse do parque de Zarafshan (ZRF) -- usado
## para preencher heartbeat_idf_units num novo relatorio de incidente
## (ver inputs/userSettings_ZRF_<incidente>.R), SEM precisar de correr o
## run_incident_zrshan.R inteiro (so' le os 2 shapefiles -- NAO tracks,
## curtailments, SCADA nem heartbeats).
##
## Replica EXATAMENTE a normalizacao de wtg$InternalNa/idf$imaging_he de
## run_incident_zrshan.R (secção "2. Turbine/IDF coverage"), para os
## rotulos "IDF<NN>" saírem identicos aos que o relatorio real vai usar.
##
## Pedido do Paulo (2026-09/10): limiar minimo de 20% de cobertura (nao so'
## a unidade com maior %, ver turbines_by_idf_threshold()/nota em
## R/turbine_idf_coverage.R -- uma unidade pode legitimamente cobrir mais
## do que 1 turbina, e uma turbina pode estar coberta por mais do que 1
## unidade) e os 2 buffers (a volta da turbina E a volta de cada unidade
## IDF) com o MESMO raio, 1000m -- compute_turbine_idf_coverage() ja usa um
## unico buffer_m para os 2 lados (confirmado abaixo, nao e' um parametro
## separado por engano).
##
## Uso: ajustar turbine_id abaixo (omissao: "T94", o incidente atual) e
## dar Source a este ficheiro.
##

packages <- c('sf', 'data.table')
for (p in packages) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

turbine_id       <- "T94"                             # turbina a verificar
min_pct_coverage <- 20                                 # % minima do buffer da turbina coberto pelo buffer da unidade IDF
buffer_m         <- 1000                               # raio (m) dos 2 buffers -- turbina E unidade IDF, o MESMO valor para ambos
## "IDF8" (1 digito) -> "IDF08": a normalizacao real (run_incident_zrshan.R
## e abaixo) usa sempre sprintf("IDF%02d", ...), 2 digitos -- sem este
## ajuste a comparacao mais abaixo nunca bateria certo so' por formatacao,
## mesmo com o numero de unidade correto.
expected_idf     <- c("IDF60", "IDF53", "IDF08", "IDF66") # a confirmar/contradizer pelo calculo abaixo

folder_input <- "inputs"
source(file.path(folder_input, "userSettings_ZRF.R")) # so' para wtg_filename/idf_filename/*_source_id_col/crs_projection_plannar -- partilhados por TODOS os incidentes deste parque (geometria do parque, nao do incidente)
source("R/turbine_idf_coverage.R")

## wtg -- mesma normalizacao de run_incident_zrshan.R ("T35" -> "T35",
## generico para letras+digitos, zero-pad a 2 digitos)
wtg <- sf::read_sf(file.path(folder_input, wtg_filename)) |> sf::st_transform(crs_projection_plannar)
wtg$InternalNa <- {
  raw_id <- wtg[[wtg_source_id_col]]
  m <- regmatches(raw_id, regexec("^([A-Za-z]+)([0-9]+)$", raw_id))
  vapply(seq_along(raw_id), function(i) {
    g <- m[[i]]
    if (length(g) < 3) return(raw_id[i])
    paste0(g[2], sprintf("%02d", as.integer(g[3])))
  }, character(1))
}

## idf -- mesma normalizacao de run_incident_zrshan.R (sequencia de digitos
## no FIM da string -> "IDF<NN>", 2 digitos)
idf <- sf::read_sf(file.path(folder_input, idf_filename))
idf$imaging_he <- sprintf("IDF%02d", as.integer(sub(".*([0-9]+)$", "\\1", idf[[idf_source_id_col]])))
idf <- sf::st_transform(idf, crs_projection_plannar)

if (!turbine_id %in% wtg$InternalNa) {
  stop(sprintf(
    "'%s' nao encontrada em wtg$InternalNa apos normalizacao (%s) -- confirma o ID exato no shapefile (%s).",
    turbine_id, paste(sort(unique(wtg$InternalNa)), collapse = ", "), wtg_filename
  ))
}

coverage_dt <- compute_turbine_idf_coverage(
  wtg, idf, buffer_m = buffer_m, wtg_id_col = "InternalNa", idf_id_col = "imaging_he"
)

cat(sprintf("\n===== Cobertura geometrica de %s (buffers de %dm, limiar >= %d%%) =====\n", turbine_id, buffer_m, min_pct_coverage))
result_dt <- coverage_dt[turbine == turbine_id & pct_coverage >= min_pct_coverage]
data.table::setorder(result_dt, -pct_coverage)
print(result_dt)

## Todas as unidades que tocam o buffer da turbina, mesmo abaixo do
## limiar -- para perceberes o que ficou de fora e porque (ex: uma unidade
## a 19% fica mesmo a' justa fora do corte de 20%)
cat(sprintf("\n===== Todas as unidades com alguma sobreposicao geometrica (sem limiar) =====\n"))
print(coverage_dt[turbine == turbine_id])

found_idf <- sort(unique(result_dt$idf))
expected_sorted <- sort(expected_idf)
only_found    <- setdiff(found_idf, expected_sorted)
only_expected <- setdiff(expected_sorted, found_idf)
fmt_set <- function(x) if (length(x) == 0L) "(nenhuma)" else paste(x, collapse = ", ")

cat(sprintf("\n===== Confirmacao face as unidades esperadas (%s) =====\n", paste(expected_sorted, collapse = ", ")))
if (identical(found_idf, expected_sorted)) {
  cat("CONFIRMADO -- o calculo geometrico bate exatamente com as unidades esperadas.\n")
} else {
  cat(sprintf("DIFERENTE do esperado:\n  Calculado : %s\n  Esperado  : %s\n  So' no calculado: %s\n  So' no esperado : %s\n",
    paste(found_idf, collapse = ", "), paste(expected_sorted, collapse = ", "),
    fmt_set(only_found), fmt_set(only_expected)
  ))
}
