##
## Carrega SO' o que a analise de ajuste do horario diario de curtailment
## precisa (R/curtailment_daylighttime_adjustment.R) -- curtailments +
## calendario de luz do dia + a lista de turbinas de incidentes -- sem
## correr o IDF_analysis.R inteiro (que le tambem tracks/SCADA/heartbeats,
## calcula disponibilidade/resposta/terreno/clustering/etc., muito mais
## lento e sem necessidade aqui).
##
## NAO altera nenhum dos settings/scripts existentes (userSettings_BSH.R,
## userSettings_DGY.R, monthlyReportSettings_*.R, IDF_analysis.R,
## IDF_monthly_report.R) -- so' os LE (source(), read-only). Reutiliza a
## MESMA cache (cache/<farm_code>/curtl_dt_unfilt.fst) que esses scripts ja'
## escrevem/leem -- se ja' correste um deles recentemente para este parque,
## este script e' quase instantaneo (nao relê os ficheiros brutos); senao,
## lê os brutos 1a vez e grava essa cache, tal como IDF_analysis.R faria.
##
## Uso -- escolher o parque ANTES de dar source a este ficheiro (mesmo
## padrao de run_annual_analysis_BSH.R/run_annual_analysis_DGY.R):
##
##   project_settings_file <- "userSettings_BSH.R"  # ou "userSettings_DGY.R"
##   source("load_curtailments.R")
##
##   # so' depois:
##   source("R/curtailment_daylighttime_adjustment.R")
##   source("explore_curtailment_daylighttime_adjustment.R")
##
## Objetos deixados em memoria por este script:
##   curtl_dt_unfilt -- TODOS os curtailments em cache (nao filtrados por
##     periodo nenhum) -- a analise de ajuste do horario escolhe a sua
##     PROPRIA janela (ex: ultimos 6 meses) a partir daqui, nao a ini/end
##     do relatorio anual/mensal (podem ser periodos bem mais curtos/velhos)
##   daylight_cal -- sunrise/sunset por dia, ja' cobrindo toda a gama de
##     datas de curtl_dt_unfilt
##   fatality_incidents, proj_timezone, proj_lat, proj_lon, farm_code --
##     copiados do settings file, para a analise/graficos usarem
##

## Packages minimos para ISTO -- so' o necessario para ler+cachear
## curtailments e calcular sunrise/sunset; ggplot2/flextable/etc. (para os
## graficos/tabelas) sao carregados por quem usar este loader a seguir
## (explore_curtailment_daylighttime_adjustment.R sourcea
## R/curtailment_daylighttime_adjustment.R, que precisa de ggplot2 -- ver
## nota nesse ficheiro).
packages <- c('data.table', 'lubridate', 'janitor', 'readxl', 'dplyr', 'suncalc', 'fst')
for (p in packages) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

if (!exists("project_settings_file")) {
  stop("Define project_settings_file (\"userSettings_BSH.R\" ou \"userSettings_DGY.R\") ANTES de source(\"load_curtailments.R\").")
}

folder_input <- "inputs"
source(file.path(folder_input, project_settings_file)) # so' LE -- nao escreve nada neste ficheiro

source("R/read_utils.R")
source("R/read_curtailments.R")
source("R/data_cache.R")
source("R/availability_daylight.R") # so' para build_daylight_calendar() (funcao 1) -- as restantes funcoes desse ficheiro (availability/offline) nao sao chamadas aqui

databases_dirs <- unique(c(databases_dir, if (exists("databases_dir_alt")) databases_dir_alt))
folder_cache <- file.path("cache", farm_code)

if (!exists("force_reread_cache")) force_reread_cache <- FALSE

## current = NULL sempre (nao "if (exists(...))...") -- mesmo cuidado de
## explore_bsh_dgy_comparison.R: garante que ve' sempre a cache em disco
## deste farm_code, nunca um curtl_dt_unfilt de OUTRO parque deixado em
## memoria de uma corrida anterior na mesma sessao R.
curtl_dt_unfilt <- reuse_or_load_cache(
  NULL,
  "curtl_dt_unfilt", file.path(folder_cache, "curtl_dt_unfilt.fst"),
  function() read_curtailments_data(databases_dirs, curtailments_pattern, tz = proj_timezone, farm_pattern = if (exists("farm_pattern")) farm_pattern else NULL),
  force_reread = force_reread_cache, tz = proj_timezone
)
curtl_dt_unfilt <- as.data.table(curtl_dt_unfilt)

daylight_cal <- build_daylight_calendar(
  min(curtl_dt_unfilt$start), max(curtl_dt_unfilt$start),
  proj_lat, proj_lon, proj_timezone
)

cat(sprintf(
  "\n===== %s: curtl_dt_unfilt carregado (%d linhas, %s a %s) =====\n",
  farm_code, nrow(curtl_dt_unfilt),
  format(min(curtl_dt_unfilt$start), "%Y-%m-%d"), format(max(curtl_dt_unfilt$start), "%Y-%m-%d")
))
cat(sprintf("===== fatality_incidents (%s): %s =====\n", farm_code, paste(unique(fatality_incidents$turbine), collapse = ", ")))
