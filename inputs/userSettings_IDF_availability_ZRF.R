##
## User settings -- relatorio de DISPONIBILIDADE DAS UNIDADES IDF (varias
## turbinas/unidades em simultaneo) -- run_idf_availability.R
##
## So' a parametrizacao relevante para esta analise (heartbeats x
## curtailments x SCADA, relacao turbina <-> unidade primaria/secundaria).
## Para outro parque: copiar este ficheiro para
## inputs/userSettings_IDF_availability_<FARM>.R e ajustar -- nada mais
## no codigo e' especifico de um parque.
##

project_ref <- "Zarafshan WPP"

##
## 1. Parque e datasets (chave do projeto) ----
##

farm_code    <- "ZRF"   # nome da cache (cache/<farm_code>/) e das pastas/ficheiros de output
farm_pattern <- "ZRF"   # 2a camada de filtro por substring nos nomes dos ficheiros brutos

## pasta (ou pastas) dos 4 datasets do portal IDF
databases_dir <- "G:/O meu disco/Programacao/r/Bsh_Dgy_WPP/data-raw"

heartbeats_pattern   <- "Heartbeats_ZRF.+csv"
curtailments_pattern <- "^Curtailments_ZRF"
scada_pattern        <- "SCADA_ZRF.+csv"
## TrackReport: so' para atribuir cada curtailment a unidade IDF que o
## disparou (TowerNumber do track) -- um curtailment so' conta como
## evidencia para a PROPRIA unidade
trackreport_pattern  <- "TrackReport_Default_ZRF"

## Localizacao do projeto (calendario de luz do dia: nascer/por do sol) e fuso
proj_lat      <- 41.58115
proj_lon      <- 64.39011
proj_timezone <- "Asia/Samarkand"
crs_projection_plannar <- 32641   # UTM da zona do parque (metros)

##
## 2. Posicoes das turbinas e das unidades IDF (inputs/) ----
##

wtg_filename      <- "Masdar_wtg_positions_utm41N.txt.shp"
wtg_source_id_col <- "ID"     # coluna de ID das turbinas no shapefile
idf_filename      <- "identiflight.shp"
idf_source_id_col <- "Name"   # coluna de ID das unidades IDF no shapefile ("IDF-22", ...)

##
## 3. Periodo analisado ----
##

ini <- as.POSIXct('2026-09-01 00:00:00', tz = proj_timezone)
end <- as.POSIXct('2026-09-30 23:59:59', tz = proj_timezone)

## Turbinas a analisar (NULL = todas as turbinas do shapefile com pelo menos
## uma unidade IDF a cobri-las)
turbines_of_interest <- NULL   # ex: c("T23", "T35", "T94")

##
## 4. Relacao turbina <-> unidade IDF primaria/secundaria ----
##
## Se existir o ficheiro MANUAL em inputs/ (colunas `Turbine ID`, `Primary IDF`,
## `Secondary IDF(s)`, varias secundarias separadas por virgula), e' esse que
## manda; turbinas que nao estejam nele, e todas as turbinas se o ficheiro
## nao existir, usam a relacao GEOMETRICA: unidades cujo buffer cobre >=
## relation_min_pct_coverage % do buffer da turbina; a de maior % e' a
## primaria, as restantes (ate relation_max_secondary) as secundarias.
##

turbine_idf_matrix_filename <- "IDF_Coverage_Matrix_ZRF.xlsx"   # pode nao existir
idf_op_detection_range      <- 1000   # m; raio de deteccao do IDF (buffer da turbina e da unidade)
relation_min_pct_coverage   <- 20     # %
relation_max_secondary      <- 3      # n.o maximo de unidades secundarias por turbina

##
## 5. Disponibilidade e alerta ----
##

heartbeat_interval_min    <- 30   # min; intervalo normal entre heartbeats de uma unidade online
heartbeat_offline_gap_min <- 60   # min; sem heartbeat durante este tempo = offline

## Alerta de falha persistente: unidades com mais de X% do tempo diurno do
## periodo offline. basis: "raw" (so' heartbeats) ou "net" (retirando os gaps
## com curtailment disparado pela propria unidade)
persistent_failure_threshold_pct <- 50
persistent_failure_basis         <- "raw"

## Calendarios: n.o de unidades por figura e espacamento das datas no eixo
calendar_units_per_figure <- 6
calendar_date_breaks      <- "2 days"
