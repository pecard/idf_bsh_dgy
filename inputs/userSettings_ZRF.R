##
## User settings -- Zarafshan WPP (ZRSHAN), incident report
##
## Ao contrario de userSettings_BSH.R/userSettings_DGY.R (que alimentam o
## pipeline completo IDF_analysis.R/IDF_monthly_report.R), este ficheiro
## alimenta APENAS run_incident_zrshan.R -- um relatorio de incidente
## unico (turbina/especie/data/janela), nao o relatorio anual/mensal
## completo. Por isso nao define coisas como manual_turbine_clusters,
## dem_filename, tier_start_scheme_filename, etc. -- sem consumidor aqui.
##
## Dados obtidos diretamente do Paulo (2026-08) para este parque, ainda
## por confirmar no 1º corrida real (ver comentarios "A CONFIRMAR" abaixo):
##   - nome exato das colunas nos 2 shapefiles
##   - se o padrao de nome dos 4 ficheiros brutos e' mesmo so' o prefixo
##     dado (sem sufixo/codigo de parque, ao contrario de BSH/DGY que
##     partilham pasta)
##   - se o vocabulario de SpeciesTypeName e' identico ao de BSH/DGY
##

##
## Project inputs (inside folder inputs/)
##

project_ref <- "Zarafshan WPP"

## Shapefiles fornecidos pelo Paulo -- nomes exatos das colunas de ID
## confirmados por ele (ver wtg_source_id_col/idf_source_id_col abaixo).
## NOTA: o nome dado foi "Masdar_wtg_positions_utm41N.txt.shp" -- ".txt.shp"
## e' invulgar (parece um artefacto de conversao); assume-se aqui que o
## ficheiro real e' so' "Masdar_wtg_positions_utm41N.shp" (as 4
## extensoes .shp/.shx/.dbf/.prj do shapefile) -- AJUSTAR se o nome real
## for mesmo diferente.
wtg_filename <- "Masdar_wtg_positions_utm41N.shp"
idf_filename <- "identiflight.shp"

## Nome da coluna de ID no shapefile de origem -- ver normalizacao em
## run_incident_zrshan.R (mesma logica de IDF_analysis.R secção 0, com um
## ajuste extra para o formato "IDF-1" do parque, ver esse ficheiro)
wtg_source_id_col <- "ID"     # turbina de incidente: "T35"
idf_source_id_col <- "Name"   # unidades no formato "IDF-1", "IDF-2", ... "IDF-22", ...

## Ainda nao existe matriz manual turbina<->IDF para este parque -- aponta
## para um ficheiro que nao existe ainda de propósito: a logica de secção 0
## (turbine_idf_coverage.R) já trata "ficheiro nao encontrado" de forma
## graciosa, computando so' a matriz GEOMETRICA (buffer turbina<->IDF) e
## gravando-a em outputs/.../turbine_idf_coverage.xlsx -- e' essa a "IDF
## Coverage Matrix" pedida, gerada a partir dos 2 shapefiles acima, sem
## precisar de matriz manual pre-existente.
turbine_idf_matrix_filename <- "IDF_Coverage_Matrix_ZRF.xlsx"

proj_lat      <- 41.58115
proj_lon      <- 64.39011
proj_timezone <- "Asia/Samarkand" # Uzbequistao -- mesmo fuso do BSH/DGY, sem DST

## EPSG 32641 = WGS84 / UTM zone 41N -- mesma zona do BSH (64.67E), a
## longitude do Zarafshan (64.39E) cai na mesma zona.
crs_projection_plannar <- 32641


##
## Raw databases (tracks, curtailments, SCADA, heartbeats)
## Pasta UNICA e dedicada a este parque (ao contrario de BSH/DGY, que
## partilham pasta) -- por isso, ao contrario dessas, NAO se define
## databases_dir_alt nem farm_pattern (list_files_multi_dir(),
## R/read_utils.R, so aplica o filtro de farm_pattern quando definido).
##

databases_dir <- "G:/O meu disco/datasets/idf_zrfshan"

## Identificador curto -- usado em cache/ e outputs/AAAAMMDD_<farm_code>/
## (ver run_incident_zrshan.R). Pedido pelo Paulo (2026-08): "ZRSHAN", nao
## "ZRF" -- apesar de a pasta de dados brutos se chamar "idf_zrfshan"
## (nomes ligeiramente diferentes, confirmado que e' intencional: os
## ficheiros brutos em si NAO usam nenhum destes codigos no nome, ver
## *_pattern abaixo).
farm_code <- "ZRSHAN"

## Confirmado pelo Paulo (2026-08): mesmo estilo de nome do BSH/DGY, mas
## SEM sufixo/codigo de parque (pasta dedicada, sem outro parque para
## discriminar) -- TrackReport_, Curtailments_ (xlsx), SCADA_, Heartbeats_
trackreport_pattern  <- "TrackReport_"      # ex: TrackReport_20260201_....csv
curtailments_pattern <- "Curtailments_"     # ex: Curtailments_20260201_....xlsx
scada_pattern        <- "SCADA_.+csv"       # ex: SCADA_20260201_....csv
heartbeats_pattern   <- "Heartbeats_.+csv"  # ex: Heartbeats_20260201_....csv

## So as unidades IDF de interesse para este incidente (nao todo o parque)
## -- usadas como fallback_idf_units em summarise_fatality_windows() (R/
## fatality_window_analysis.R) e para restringir a reconciliacao de tracks
## candidatos (R/track_harmonization.R) a estas unidades. Formato
## "IDF22"/"IDF24"/... (2 digitos, sem hifen) -- ver normalizacao do
## shapefile identiflight.shp ("IDF-1", "IDF-2", ...) em
## run_incident_zrshan.R; A CONFIRMAR se o heartbeats bruto (instance_name)
## usa este mesmo formato ou o formato com hifen do shapefile -- o script
## imprime os valores unicos encontrados em heartb_dt$idf logo apos a
## leitura, para deteccao rapida de um mismatch.
heartbeat_idf_units <- c("IDF22", "IDF24", "IDF26", "IDF71")


##
## Timeframe for analysis/reporting period
##
## Dados disponiveis 2026-01-01 a 2026-08-15 (confirmado pelo Paulo,
## 2026-08); SCADA cobre a MESMA janela completa (ao contrario de BSH, onde
## scada_ini/scada_end sao mais estreitos que ini/end).
##

ini <- as.POSIXct('2026-01-01 00:00:00', tz = proj_timezone)
end <- as.POSIXct('2026-08-15 23:59:59', tz = proj_timezone)

scada_ini <- as.POSIXct('2026-01-01 00:00:00', tz = proj_timezone)
scada_end <- as.POSIXct('2026-08-15 23:59:59', tz = proj_timezone)

## So a turbina do incidente -- e' o unico foco deste relatorio (ao
## contrario do relatorio anual do BSH/DGY, que cobre turbinas_scada
## farm-wide)
turbinas_scada <- c("T35")


##
## Project's species groups -- mesmo vocabulario do BSH/DGY (confirmado
## pelo Paulo, 2026-08: exportacao do mesmo portal IdentiFlight)
##

prioritysp <- c(
  'Steppe-Eagle',
  'Bearded-Vulture',
  'Egyptian-Vulture',
  'Eurasian-Or-Himalayan-Griffon',
  'Cinereous-Vulture',
  'Golden-Eagle',
  'Imperial-Eagle',
  'Saker-Falcon',
  'Peregrine-Or-Saker-Falcon',
  'White-Tailed-Eagle',
  'Protected',
  'Booted-Eagle',
  'Short-Toed-Snake-Eagle'
)

nonprioritysp <- c(
  'Accipiter',
  'Kestrel',
  'Common-Buzzard',
  'Greater-Spotted-Eagle',
  "Harrier",
  'Honey-Buzzard',
  'Long-Legged-Buzzard',
  'Merlin',
  'Osprey',
  'Peregrine-Falcon',
  'Red-Or-Black-Kite',
  'Sparrow-Hawk',
  "Eagle",
  'Eagle-Unknown',
  'Eagle-Sp'
)

othersp <- c(
  'Common-Crane',
  'White-Stork',
  'Black-Stork',
  'Raven',
  'Pigeon',
  'Grey-Heron',
  'Cormorant',
  'Great-Egret',
  'Lark',
  'Gull',
  'Other',
  'Pelican',
  'Swan',
  'Not-Eagle',
  "Turbine-Blade"
)


##
## Analysis parameters -- copiados VERBATIM de userSettings_BSH.R
## (metodologia farm-independente, "NAO ALTERAR PARA GARANTIR
## COMPARABILIDADE" entre parques/relatorios)
##

idf_op_detection_range <- 1000 # em metros; raio de deteção operacional do IDF, usado no buffer geometrico turbina<->IDF

heartbeat_interval_min    <- 30
heartbeat_offline_gap_min <- 60

safe_shutdown_rpm <- 1
curtailment_start_end_gap_sec <- 10
curtailment_max_next_gap_sec   <- 20
curtailment_drop_pct_threshold <- 0.10
curtailment_window_sec         <- 90
curtailment_window_max_gap_sec <- 15

shutdown_time_thresholds <- c(2, 1, 0)
shutdown_time_low_cut    <- 40
shutdown_time_high_cut   <- 50
shutdown_time_buffer_sec <- 60

curtailment_latency_decline_pct <- 0.10
curtailment_cutin_rpm           <- 3
response_timeline_unit          <- "week"

## em metros; limiar de proximidade a turbina para identificar candidatos a
## colisao (rotor-swept zone, distancia horizontal/2D) -- mesmo criterio
## fisico/biologico do BSH/DGY, nao especifico do parque
track_proximity_threshold_m <- 100

## dias APOS o incidente a comparar com a janela pre-incidente, na
## abundancia (min individuals) da especie -- ver CLAUDE.md
fatality_post_incident_days <- 3

min_individuals_bin_min      <- 2
min_individuals_merge_dist_m <- 200

## Limiares de harmonizacao de tracks (R/track_harmonization.R) -- mesmos
## valores usados na exploracao BSH (2026-08), ainda genericos/nao
## validados para este parque especificamente; a rever com
## diagnose_handoff_candidates()/diagnose_overlap_candidates() se os
## resultados nao parecerem plausiveis (ver historial dessa exploracao)
harmonization_handoff_time_window_sec <- 30
harmonization_handoff_max_dist_m      <- 50
harmonization_duplicate_max_median_dist_m <- 300
harmonization_duplicate_max_spread_m      <- 50
harmonization_duplicate_min_overlap_frac  <- 0.8
harmonization_duplicate_min_overlap_sec   <- 10


##
## Incident details -- turbina/especie/data/janela deste relatorio
##
## incident_date: bird found 2026-05-11 (lido "5/11/2026" como M/D/Y --
## D/M/Y daria 2026-11-05, no futuro relativamente a hoje, 2026-08-28;
## CONFIRMAR com o Paulo se a leitura correta e' mesmo Maio e nao Novembro)
##

fatality_incidents <- data.table::data.table(
  incident_id   = "ZRF_0001",
  turbine       = "T35",
  species       = "Egyptian-Vulture",
  incident_date = as.Date("2026-05-11"),
  days_before   = 15
)
