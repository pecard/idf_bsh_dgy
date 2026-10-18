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
## disparou (TowerNumber do track) -- lidas so' 3 colunas e so' os tracks dos
## curtailments do periodo. Ignorado se attribute_curtailments_by_unit = FALSE.
trackreport_pattern  <- "TrackReport_Default_ZRF"

## TRUE: um curtailment so' conta como evidencia para a unidade que o
## disparou (le os TrackReport, ver acima). FALSE: nao le tracks; um
## curtailment da turbina conta para TODAS as unidades que a protegem
## (sobrestima "IDF unit communication failure" quando ha' varias unidades).
attribute_curtailments_by_unit <- TRUE

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

ini <- as.POSIXct('2026-08-15 00:00:00', tz = proj_timezone)
end <- as.POSIXct('2026-10-15 23:59:59', tz = proj_timezone)

## Turbinas a analisar (NULL = todas as turbinas do shapefile com pelo menos
## uma unidade IDF a cobri-las)
turbines_of_interest <- c("T94")

##
## 4. Relacao turbina <-> unidade IDF primaria/secundaria ----
##
## Se existir o ficheiro MANUAL em inputs/ (colunas `Turbine ID`, `Primary IDF`,
## `Secondary IDF(s)`, varias secundarias separadas por virgula), e' esse que
## manda; turbinas que nao estejam nele, e todas as turbinas se o ficheiro
## nao existir, usam a relacao GEOMETRICA: unidades cujo buffer cobre >=
## relation_min_pct_coverage % do buffer da turbina (0 = qualquer sobreposicao);
## a de maior % e' a primaria, as restantes (ate relation_max_secondary) as
## secundarias, terciarias, ... Um limite baixo/sem limite mostra TODAS as unidades
## que podem detetar aves junto da turbina (e disparar curtailments).
##

turbine_idf_matrix_filename <- "IDF_Coverage_Matrix_ZRF.xlsx"   # pode nao existir
idf_op_detection_range      <- 1000   # m; raio de deteccao do IDF (buffer da turbina e da unidade)
relation_min_pct_coverage   <- 5      # %; 0 = todas as unidades cujo buffer se sobrepoe ao da turbina
relation_max_secondary      <- Inf    # n.o maximo de unidades secundarias por turbina (Inf = sem limite)

## Area em redor da turbina (raio, m) usada na "protecao remanescente": %
## dessa area coberta pelas unidades que NAO sao a primaria
turbine_area_radius_m <- 1000

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

## Especie de interesse para a tabela de curtailments disparados por cada
## unidade IDF (NULL = todas as especies). Precisa da atribuicao por unidade
## (attribute_curtailments_by_unit = TRUE).
species_of_interest <- "Egyptian-Vulture"

## Calendarios: n.o de unidades por figura e espacamento das datas no eixo
calendar_units_per_figure <- 6
calendar_date_breaks      <- "2 days"

##
## 6. Falhas reportadas pela equipa IDF (opcional) ----
##
## Falhas que nao se obtem dos dados do portal. Para as do tipo WFOV compara-se,
## na unidade avariada, a % de tracks capturados por cada camara WFOV
## (coluna WFOV_camera do TrackReport) nas janelas ANTES e DEPOIS da data da
## falha, e mostra-se junto com heartbeats, curtailments e SCADA do mesmo
## periodo. Os TrackReport tem de cobrir as janelas (mesmo fora do periodo
## `ini`-`end`). Outros tipos (PTU, estereo, ...) ficam listados, sem avaliacao.
## Colunas: unit (codigo completo), datetime (hora local; se so' se conhece o
## dia, assumir uma hora), type (texto da equipa IDF), turbine (opcional: turbinas
## para curtailments/SCADA do contexto), days_before / days_after, days_pre.
## NULL ou sem linhas = sem esta secao.
idf_malfunctions <- data.frame(
  unit = "GW89-58", datetime = "2026-05-27 02:30:00", 
  type = "WFOV camera module malfunction",
  turbine = "T94", 
  days_before = 7, 
  days_after = 7,
  days_pre = 21,                      # periodo adicional ANTES do "antes", mesma unidade (0 = sem); com a linha de base (earlier + before) compara-se o 'after'; 21 dias da' mais poder que 7
  stringsAsFactors = FALSE
)

## Inferencia bayesiana (MCMC) sobre o TOTAL de tracks da unidade avariada:
## probabilidade de a taxa de tracks por dia ter descido depois da falha, e de ter
## descido pelo menos wfov_fall_threshold (0.33 = 33%). wfov_prior_theta_sd = desvio
## padrao do prior (normal, centrado em 0, no log da razao depois/antes); a sensibilidade
## a 0.5 e a 2 aparece no relatorio.
wfov_fall_threshold <- 0.33
wfov_prior_theta_sd <- 1
