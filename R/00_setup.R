## -----------------------------------------------------------
## 00_setup.R
## Setup: pacotes, configuracoes do projeto e leitura de dados
## -----------------------------------------------------------
##
## Assume working directory = raiz do projeto (garantido
## automaticamente ao abrir idf_bsh_dgy.Rproj no RStudio).
##


## Pacotes ----

packages <- c('data.table', 'dplyr', 'readxl', 'janitor', 'lubridate', 'sf', 'suncalc', 'ggplot2')

for (p in packages) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}


## Pastas ----

folder_input  <- "inputs"
folder_output <- file.path("outputs", format(Sys.time(), "%Y%m%d"))

dir.create(folder_output, showWarnings = FALSE, recursive = TRUE)


## Configuracoes do projeto ----

source(file.path(folder_input, "userSettings.R"))


## Funcoes de leitura ----

source("R/read_tracks.R")
source("R/read_curtailments.R")
source("R/read_scada.R")
source("R/read_heartbeats.R")


##
## 0. Import data ----
##

# WTG e IDF (shapefiles)
wtg <- read_sf(file.path(folder_input, wtg_filename))
idf <- read_sf(file.path(folder_input, idf_filename))

# Tier scheme
tier  <- read_xlsx(file.path(folder_input, tier_start_scheme_filename))
tier3 <- read_xlsx(file.path(folder_input, tier3_start_scheme_filename), sheet = 'tier3')

# Bases de dados externas (tracks, curtailments, SCADA, heartbeats)
# --> ficam na pasta acima do projeto (databases_dir, definido em userSettings.R)
track_dt_unfilt  <- read_tracks_data(databases_dir, trackreport_pattern)
curtl_dt_unfilt  <- read_curtailments_data(databases_dir, curtailments_pattern)
scada_dt_unfilt  <- read_scada_data(databases_dir, scada_pattern)
heartb_dt_unfilt <- read_heartbeats_data(databases_dir, heartbeats_pattern, tz = proj_timezone)


## Resumo rapido (confirmar que a leitura correu bem) ----

cat("WTG:", nrow(wtg), "turbinas\n")
cat("IDF:", nrow(idf), "unidades\n")
cat("Tier scheme:", nrow(tier), "linhas | Tier3 scheme:", nrow(tier3), "linhas\n")
cat("Tracks:", if (is.null(track_dt_unfilt)) 0L else nrow(track_dt_unfilt), "registos\n")
cat("Curtailments:", if (is.null(curtl_dt_unfilt)) 0L else nrow(curtl_dt_unfilt), "registos\n")
cat("SCADA:", if (is.null(scada_dt_unfilt)) 0L else nrow(scada_dt_unfilt), "registos\n")
cat("Heartbeats:", if (is.null(heartb_dt_unfilt)) 0L else nrow(heartb_dt_unfilt), "registos\n")
