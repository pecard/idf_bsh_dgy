##
## User settings
##
## As seccoes "## N. ... ----" abaixo espelham a numeracao das seccoes de
## analise em IDF_analysis.R, para facilitar encontrar que parametro
## alimenta que parte do script.
##


##
## Run switches -- que analises correm nesta ronda
##
## NAO condiciona a leitura de dados (seccao 0 de IDF_analysis.R, incluindo
## a matriz turbina<->IDF, corre sempre) -- so as analises abaixo, que sao
## caras ou nao sao precisas em toda corrida. FALSE salta o bloco (e grava
## uma mensagem a dizer que foi saltado), nao o remove do script.
##

run_sections <- list(
  curtailment_response   = TRUE,  # 3.5-3.7 (resposta a curtailments, shutdown time, safe distance) -- so corre se scada_dt tambem existir
  fatality_investigation = TRUE,  # 4 (tracks + disponibilidade + resposta na janela de cada incidente)
  coverage_3d             = FALSE,  # 5.2 (malha 3D com topografia) -- a mais morosa; poe FALSE depois da 1ª corrida com uma serie de dados estavel
  min_individuals          = TRUE,  # 6.4 (contagem minima de individuos por bin, farm-wide)
  turbine_clustering       = FALSE, # 10 (clusters ESTATISTICOS de turbinas, por distancia: curtailments e tracks de especie) -- corre sobre o historico completo (_unfilt), pode ser mais lento. Desligada (2026-08, pedido do Paulo): a via estatistica nao acrescentava clareza ao lado da via manual/risk_clusters no relatorio -- reativar aqui e' o unico passo necessario para a secção voltar a aparecer (o Rmd ja trata a ausencia dela)
  risk_clusters            = TRUE   # 10 (clusters MANUAIS/setores, a mesma analise mas para manual_turbine_clusters -- independente do estatistico acima, liga/desliga a sua vontade)
)


##
## Project inputs (inside folder inputs/)
##

project_ref <- "Bash WPP"

wtg_filename                <- "Bash_Turbines_UTM.shp"  # in .shp

idf_filename                <- "Bash_IDF_coord.shp"                         # in .shp
tier_start_scheme_filename  <- "Bash_tier_scheme_20260215.xlsx"             # em .xlsx
tier3_start_scheme_filename <- "Bash_tier_3_commissioning_20260215.xlsx"    # em .xlsx

# matriz turbina -> unidade(s) IDF (Primary/Secondary), mantida manualmente
# -- colunas usadas: Site, `Turbine ID`, `Primary IDF`, `Secondary IDF(s)`,
# `Secondary command capability` -- ver R/turbine_idf_coverage.R
turbine_idf_matrix_filename <- "ACWA_IDF_Coverage_Matrix.xlsx"              # em .xlsx

proj_lat      <- 40.65
proj_lon      <- 64.67
proj_timezone <- "Asia/Samarkand"

crs_projection_plannar <- 32641 # codigo CRS da projecao planar a usar


##
## Raw databases (tracks, curtailments, SCADA, heartbeats)
## Ficam FORA do repositorio, numa pasta acima da pasta do projeto.
## Os ficheiros sao identificados por padrao no nome, nao por nome fixo.
##

databases_dir <- "G:/O meu disco/Programacao/r/Bsh_Dgy_WPP/data-raw"
databases_dir_alt <- "//192.168.1.11/DadosBrutos(T2)/Lisboa/08_Tecnica/2025/T05-2025_BSH_DGY/IDF_PortalData/BSH"

## databases_dir acima e' PARTILHADA com o projeto DGY (userSettings_DGY.R
## aponta para a mesma pasta) -- os 4 patterns abaixo, por si so', nao
## discriminam por parque. farm_pattern e' um filtro ADICIONAL (2a
## passagem, por substring em qualquer posicao do nome do ficheiro -- ver
## list_files_multi_dir(), R/read_utils.R) para excluir ficheiros do DGY que
## estejam na mesma pasta partilhada, mesmo que a posicao exata do codigo do
## parque varie entre convencoes de nome (ex: SCADA_..._BSH_T014.csv vs
## SCADA_BSH014_..._BSH.csv). Alternancia "BSH|Bash" -- tracks/curtailments/
## SCADA usam o acronimo "BSH" no nome do ficheiro, mas heartbeats usa o
## nome completo "Bash" (ver heartbeats_pattern, "Bash_Heartbeats.+csv",
## abaixo) -- so' "BSH" deixava 0 ficheiros de heartbeats depois do filtro
## (bug encontrado 2026-08, corrida real: "0 de 1 ficheiro(s) mantidos").
farm_pattern <- "BSH|Bash"

## Identificador curto e limpo do parque (sem regex/alternancia, ao
## contrario de farm_pattern acima) -- usado so' para nao colidir com
## outros parques na pasta cache/ e outputs/AAAAMMDD_<farm_code>/ partilhadas
## (ver IDF_analysis.R, logo apos o source() deste ficheiro).
farm_code <- "BSH"

trackreport_pattern  <- "TrackReport_" #"TrackReport.+csv"          # ex: TrackReport_20260201_....csv
curtailments_pattern <- "curtail_orders|Curtailments" # ex: Curtailments_20260201_....xlsx
scada_pattern        <- "SCADA_.+csv"               # ex: SCADA_20260201_....csv
heartbeats_pattern   <- "Bash_Heartbeats.+csv"          # ex: Heartbeats_20260201_....csv
heartbeat_idf_units <- c("BSH55-09" ,"BSH53-10", "BSH52-11", 
                         "BSH64-04", "BSH62-05", "BSH61-06",
                         'BSH14-41', 'BSH12-40')

##
## Timeframe for analysis/reporting period
##

ini <- as.POSIXct('2025-01-01 00:00:00', tz = proj_timezone)
end <- as.POSIXct('2026-08-15 23:59:59', tz = proj_timezone)


##
## Project's species groups
## Usadas nas seccoes 3.3 (curtailments por especie) e 5 (biologico)
##

# 11
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

# 18
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
  'Harrier',
  "Eagle",
  'Eagle-Unknown',
  'Eagle-Sp'
)

# 17
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
  'Pigeon',
  'Other',
  'Pelican',
  'Swan',
  'Not-Eagle',
  "Lark",
  "Turbine-Blade" 
)


##
## Analysis parameters, por seccao de IDF_analysis.R -- NAO ALTERAR PARA GARANTIR COMPARABILIDADE
##


## -- 1. Performance --
## (sem parametros especificos definidos aqui, para ja)


## -- 3.1. System Availability (heartbeats) --

heartbeat_interval_min    <- 30 # intervalo esperado entre heartbeats de cada unidade IDF
heartbeat_offline_gap_min <- 60 # gap (min) a partir do qual se considera a unidade offline (perdeu >=2 heartbeats seguidos)
idf_availability_top_n    <- 12L # nº de unidades (com mais tempo offline) mostradas nos graficos de calendario/frequencia


## -- 3.2. Curtailments due to ID transitions --
##
## "Atraso" de um curtailment apos a reclassificacao NP->P (nao-prioritaria
## -> prioritaria) dentro do mesmo track e' avaliado por 2 criterios em
## paralelo, para comparar e depois decidir qual manter (ver R/id_transitions.R):
##   - por TEMPO: gap entre o 1º registo do track ja classificado como
##     prioritaria e o inicio do curtailment (id_transition_late_time_sec)
##   - por DISTANCIA: reutiliza track_proximity_threshold_m (definido abaixo,
##     seccao 4) -- se a ave ja estava dentro desse limiar no momento da
##     reclassificacao, considera-se tardio independentemente do tempo

id_transition_late_time_sec <- 50 # segundos; mesmo valor de shutdown_time_high_cut (3.6) como ponto de partida, a rever

# especie(s) a analisar na matriz de confusao de especies (que outras
# especies aparecem no mesmo track, em geral e para tracks com curtailment
# -- ver summarise_species_confusion() em R/id_transitions.R). Kestrel por
# omissao (pedido do Paulo, 2026-08); reutilizavel para outra especie so'
# mudando este parametro
id_confusion_species_of_interest <- "Kestrel"

# especie candidata a ser removida da estrategia de curtailment (discussao
# com o cliente, 2026-08) -- ver R/curtailment_removal_risk.R,
# evaluate_curtailment_removal_risk(). Quantifica o risco de remover os
# curtailments disparados enquanto classificados como esta especie, para os
# casos em que o track mais tarde revelou ser de especie prioritaria.
curtailment_removal_species_of_interest <- "Kestrel"

# em segundos; tolerancia maxima para associar um ponto do track ao momento
# exato do disparo do curtailment (x2d_at_curtailment) -- mesma logica de
# match_nearest_rpm() em R/curtailment_response.R (roll="nearest" com
# limite de tolerancia, nao um match arbitrariamente distante)
curtailment_removal_max_trigger_match_sec <- 30


## -- 3.4. Short-track curtailment --

shorttrack_min_points <- 6   # threshold de pontos dos tracks abaixo do qual se considera short-tracks
shorttrack_eval_range <- 300 # em metros; distancia ate qual se considera relevante avaliar short-tracks que despoletaram curtailments


## -- 3.5. Curtailment response assessment (roll join com SCADA) --

# Incluir time range e turbinas para o qual temos dados de SCADA, mesmo que no intervalo tenham "buracos" sem info
scada_ini <- as.POSIXct('2025-10-15 00:00:00', tz = proj_timezone)
scada_end <- as.POSIXct('2026-08-15 23:59:59', tz = proj_timezone)

turbinas_scada <- c('BSH54', 'BSH62', 'BSH14')

safe_shutdown_rpm <- 1 # threshold de rpm a considerar para uma safe passage; curtailment considerado feito com sucesso quando rpm da turbina < safe_shutdown_rpm

# tolerancia apertada (segundos) para considerar fiavel o RPM de baseline no
# start/end de um curtailment -- usada em assess_curtailment_response() (3.5)
# e reutilizada em time_to_rpm_thresholds() (3.6)
curtailment_start_end_gap_sec <- 2

curtailment_max_next_gap_sec   <- 20   # tolerancia (s) para aceitar a leitura SCADA seguinte como "resposta imediata"
curtailment_drop_pct_threshold <- 0.10 # queda minima (%) nessa leitura seguinte para considerar resposta imediata

curtailment_window_sec         <- 90 # janela larga (s) de monitorizacao usada na vista complementar (classify_curtailment_response)
curtailment_window_max_gap_sec <- 15 # tolerancia (s) de "nearest" usada nessa vista complementar


## -- 3.6. Shutdown time (tempo ate atingir limiares de RPM) --

shutdown_time_thresholds <- c(2, 1, 0) # limiares de rpm a avaliar (rpm)
shutdown_time_low_cut    <- 40 # segundos
shutdown_time_high_cut   <- 50 # segundos

# tempo extra (s) alem do "end" da propria ordem de curtailment, dado tanto
# ao shutdown time (3.6) como a latencia de resposta (3.6b) antes de um
# evento contar como "sem resposta" -- turbinas podem so' completar a
# paragem depois do fim da ordem (inercia mecanica). Adotado 2026-08 a
# partir da exploracao em explore_curtailment_response_buffer.R
# (R/curtailment_response_buffer.R, R/curtailment_response_latency.R).
shutdown_time_buffer_sec <- 60


## -- 3.6b. Response latency (tempo ate a turbina COMECAR a reagir) --

# queda relativa (fracao do RPM de baseline no start) que conta como
# inicio de resposta -- ver R/curtailment_response_latency.R. Mesmo valor
# de curtailment_drop_pct_threshold (3.5) mas mecanismo diferente: aquele
# so' olha para a leitura SCADA seguinte ao sinal (~10s depois); este
# procura em toda a janela [start, end + shutdown_time_buffer_sec].
curtailment_latency_decline_pct <- 0.10


## -- 3.7. Safe distance (metodologia KNE) --

safe_dist_rpm_threshold  <- 2   # rpm; "tempo ate parar em seguranca" usado na formula do KNE (distinto de safe_shutdown_rpm)
safe_dist_speed_trim_q   <- 0.95 # percentil de corte de outliers na velocidade de voo do track
safe_dist_reference_line_m <- 600 # em metros; linha de referencia nos plots -- POR CONFIRMAR/AJUSTAR

# rpm; abaixo deste valor no momento do disparo, considera-se que a turbina ja
# estava a abrandar/recuperar de outro curtailment (turbine_state =
# "already_slowing"), distinto do cenario mais gravoso de velocidade normal de
# operacao ("full_speed") -- operacional normal ~10-12 rpm
safe_dist_already_slowing_rpm <- 6


## -- 4. Fatality investigation (cross-cutting: 3.1 + 3.5-3.7 + 4) --
##
## Tudo o que precisa de um incidente + turbina + espécie + janela de dias
## fica agrupado aqui, mesmo que a analise em si corra secções diferentes de
## IDF_analysis.R:
##   - tracks da especie perto da turbina, na janela            (4, R/fatality_track_investigation.R)
##   - disponibilidade das unidades IDF dessa turbina, na janela (3.1, R/fatality_window_analysis.R)
##   - resposta a curtailments dessa turbina, na janela          (3.5-3.7, R/fatality_window_analysis.R)
##   - abundancia (min individuals) pre- e pos-incidente          (6.4, R/fatality_window_analysis.R)
## As analises de janela REUTILIZAM os thresholds ja definidos acima em 3.1
## (heartbeat_offline_gap_min, heartbeat_interval_min), 3.5-3.6
## (curtailment_start_end_gap_sec, curtailment_max_next_gap_sec,
## curtailment_drop_pct_threshold, safe_shutdown_rpm, shutdown_time_thresholds,
## shutdown_time_high_cut) e 6.4 (min_individuals_bin_min,
## min_individuals_merge_dist_m) -- nao ha parametros duplicados aqui.
##
## As unidades IDF de cada turbina sao resolvidas a partir da matriz manual
## (turbine_idf_matrix_filename, seccao "Project inputs" acima) -- ver
## R/turbine_idf_coverage.R e R/fatality_window_analysis.R.

# em metros; limiar de proximidade a turbina para identificar candidatos a
# colisao (rotor-swept zone, distancia horizontal/2D) -- global, o mesmo
# para todos os incidentes (criterio fisico/biologico, nao operacional)
track_proximity_threshold_m <- 100

# dias APOS o incidente a comparar com a janela pre-incidente, na abundancia
# (min individuals) da especie envolvida -- puramente descritivo, ver
# R/fatality_window_analysis.R (summarise_individuals_pre_post()): uma
# diferenca pode refletir o incidente OU so a fase natural da migracao a
# passar, nao se assume causalidade (CLAUDE.md)
fatality_post_incident_days <- 3

# incidentes de fatalidade conhecidos -- incident_date e' a data em que a ave
# foi ENCONTRADA numa prospecao, NAO a data da morte (ver CLAUDE.md); por
# isso days_before define uma janela de varios dias antes (E incluindo) essa
# data, nao so o proprio dia -- ajustar days_before por incidente conforme a
# frequencia de prospecao/deteção de cada caso
fatality_incidents <- data.table::data.table(
  incident_id   = c("BSH_0002", "BSH_0004", "BSH_0012"),
  turbine       = c("BSH54", "BSH62", 'BSH14'),
  species       = c("Steppe-Eagle", "Egyptian-Vulture", "Egyptian-Vulture"),
  incident_date = as.Date(c("2025-10-31", "2026-03-19", "2026-08-03")),
  days_before   = c(8, 8, 8)
)


## -- 5. Coverage --

idf_op_detection_range <- 1000 # em metros; distancia de deteção operational do IDF a considerar na análise

coverage_cylinder_height       <- 1000 # em metros; 3D coverage cylinder - height
coverage_cylinder_wider_radius <- 1100 # em metros; 3D coverage cylinder - wider radius
coverage_cylinder_inner_radius <- 600  # em metros; 3D coverage cylinder - inner radius

## -- 5.2. WTG coverage 3D com topografia (DEM) --

# GeoTIFF cobrindo toda a area do parque (ex: Copernicus GLO-30), descarregado
# manualmente e colocado em databases_dir -- o script faz o crop/mask ao raio
# de cada turbina automaticamente, nao e preciso pre-recortar o ficheiro.
dem_filename <- "Bash_DEM_copernicus30m.tif"

# Subconjunto de turbinas (nomes tal como aparecem na coluna InternalNa do
# shapefile wtg) para a analise 3D completa -- e cara (DEM + malha + KD-tree
# por turbina), por isso NAO corre para todas as turbinas por omissao.
# Nomes que nao existirem no shapefile geram um aviso explicito, nao sao
# ignorados em silencio (ver run_coverage_3d_all_turbines()).
# Usa wtg_3d_coverage <- "all" para correr a analise para todas as turbinas do shapefile.
wtg_3d_coverage <- c('all') #all' # c('BSH54')

coverage_mesh_step_xy   <- 50 # em metros; resolucao horizontal da malha 3D
coverage_mesh_step_z    <- 50 # em metros; resolucao vertical da malha 3D
coverage_prox_thresh_m  <- 50 # em metros; distancia 3D maxima ave-no da malha para considerar "covered"

# nº minimo de registos de track candidatos (por turbina) para considerar a
# amostra suficiente -- turbinas abaixo disto ficam assinaladas (low_sample)
# nos resumos de summarise_mesh_coverage(), a interpretar com cautela
coverage_min_sample_records <- 500000

# Bandas de risco (altura relativa a turbina, em metros) para a malha 3D com
# topografia -- AINDA POR DECIDIR: por agora usa os breaks/labels por omissao
# da funcao (c(200), c("at risk","above")), a rever depois para alinhar (ou
# nao) com riskHeight_lower/upper acima (usado no coverage_3D.R antigo, sem
# topografia).
# coverage_3d_risk_band_breaks <- c(200)
# coverage_3d_risk_band_labels <- c("at risk", "above")


## -- 6.1-6.2. Flight speed / flight height per species --
##
## Filtros de qualidade PARTILHADOS pelas 2 metricas e pelo grafico
## combinado (ver R/bio_flight_metrics.R) -- os 3 scripts originais
## (scripts_IDF/bio_flight_speed.R, bio_flight_height.R,
## bio_distrib_flight_height_speed_per_species.R) usavam bases ligeiramente
## diferentes entre si (count>2 vs count>3 vs sem filtro de count; so o
## grafico filtrava altura>0). Aqui e' um so conjunto de parametros, no
## valor mais exigente dos 2 scripts de tabela original.

flight_min_track_points <- 4   # nº min. de pontos do track (equivalente ao "count>3" do bio_flight_height.R original)
flight_speed_ms_min     <- 1   # m/s; abaixo disto considera-se ruido/estacionario
flight_speed_ms_max     <- 100 # m/s; acima disto considera-se erro de sensor


## -- 6.3. Risk per species --

riskHeight_lower <- 50  # in meters
riskHeight_upper <- 250 # in meters


## -- 6.4. Minimum individuals per time bin (modulo geral, farm-wide) --

# em minutos; duracao do bin usado para agrupar registos de tracks -- ver
# R/track_min_individuals.R
min_individuals_bin_min <- 2

# em metros; dois tracks no mesmo bin (mesmo de unidades IDF diferentes)
# contam como 1 individuo se a distancia minima entre qualquer par de
# pontos dos dois tracks, dentro desse bin, for inferior a este valor
min_individuals_merge_dist_m <- 200


## -- 8. System performance vs. bird phenology (evolucao temporal, cross-cutting) --
##
## Evolucao ao longo de todo o periodo da qualidade de resposta a
## curtailments (missed/delayed), sobreposta a abundancia de aves -- para
## explorar se a performance do sistema varia com os periodos de maior
## movimento migratorio. Ver R/curtailment_response_timeline.R. Reutiliza a
## classificacao missed/delayed de R/curtailment_response_classify.R (mesmos
## thresholds de 3.5-3.6) e os bins de R/track_min_individuals.R (6.4) --
## nao ha parametros novos alem da granularidade da serie temporal.

# "week" ou "month" -- granularidade da serie temporal (resposta e
# abundancia sao agregadas nesta mesma escala, para poderem ser comparadas)
response_timeline_unit <- "week"

# Exemplos ilustrativos de perfil de RPM -- para cada categoria (missed/
# delayed, mesma classificacao de response_flag_dt acima), ate'
# curtailment_example_n eventos sao escolhidos e desenhados (RPM +
# linhas verticais de inicio/fim do curtailment), janela de
# curtailment_example_window_before_min antes e
# curtailment_example_window_after_min depois do INICIO do curtailment.
# Ver select_curtailment_examples()/plot_curtailment_events_rpm(),
# R/curtailment_forensic_trace.R.
curtailment_example_n                  <- 3
curtailment_example_window_before_min  <- 1
curtailment_example_window_after_min   <- 3


## -- 9. Turbine recent activity (apoio a matriz de decisao do protocolo
##       de resposta a outages do IdentiFlight) --
##
## Atividade recente de aves prioritarias por turbina (farm-wide, nao so'
## as turbinas com SCADA) -- ver R/turbine_recent_activity.R. Alimenta,
## junto com a matriz turbina<->IDF (seccao 0/Project inputs) e o historico
## de missed/no_data (3.5-3.7), a matriz de decisao Excel do protocolo de
## resposta a outages (documento externo, IDF_Response_Protocol).

# em dias; janela "recente" considerada (a contar para tras a partir de
# `end`) -- nao e' o mesmo conceito de days_before da seccao "Fatality
# investigation" (essa e' fixa, em torno de um incidente ja conhecido)
recent_activity_days <- 14


## -- 10. Turbine spatial/temporal clustering --
##
## 2 componentes, ver R/turbine_spatial_clusters.R,
## R/curtailment_cluster_patterns.R, R/track_species_clusters.R:
##   10.1 -- padroes espaciais/temporais de curtailments por cluster de
##           turbinas (estatistico, por distancia, E manual, por setor),
##           com o contributo marginal das turbinas de fatality_incidents
##           (secção 4 acima) dentro do seu cluster
##   10.2 -- ocorrencia de tracks de uma especie (Kestrel por omissao) por
##           cluster de turbinas -- reutiliza os MESMOS clusters de 10.1
##
## Periodo (10.1): por omissao NULL = todo o periodo coberto por
## curtl_dt_unfilt (nao so' a janela do relatorio) -- ajustar so' se for
## preciso restringir.
curtl_cluster_date_from <- NULL
curtl_cluster_date_to   <- NULL

# em metros; limiar de distancia "duro" para o cluster ESTATISTICO
# (single-linkage -- 2 turbinas ficam no mesmo cluster se houver uma cadeia
# de vizinhas, cada par consecutivo a <= este valor).
#
# Distancia real entre turbinas consecutivas no Bash: ~560m (confirmado
# pelo Paulo). Isto importa MUITO para single-linkage: qualquer limiar
# acima de ~560m ja liga turbinas consecutivas, e por chaining transitivo
# (A-B<=limiar E B-C<=limiar => A,C no mesmo cluster mesmo que A-C>limiar)
# um limiar generoso pode encadear uma fiada INTEIRA de turbinas num so
# cluster gigante, mesmo que o objetivo fosse separar setores. 650m fica
# logo acima do espacamento tipico (liga vizinhos diretos de uma fiada,
# sem inflacionar mais o limiar do que o necessario), mas e' so' um ponto
# de partida -- usar SEMPRE a tabela de sweep abaixo
# (turbine_cluster_threshold_sensitivity(): nº de clusters + silhouette por
# limiar) antes de confiar no resultado, e comparar o nº de clusters obtido
# com o nº de setores manuais (9, manual_turbine_clusters abaixo) como
# referencia de plausibilidade -- se o estatistico colapsar para 1-2
# clusters gigantes bem abaixo do manual, e' sinal de chaining a mais.
cluster_max_dist_m <- 650

# vetor de limiares (m) para o sweep de sensibilidade acima -- resolucao
# fina a volta da escala critica (560m, o espacamento real) para conseguir
# ver ONDE e' que o chaining comeca a colapsar clusters entre si
cluster_threshold_sweep_m <- seq(400, 3000, by = 100)

# nº de permutacoes no teste de validacao estatistica do contributo
# marginal (permutation_test_marginal_contribution(), ver
# R/curtailment_cluster_patterns.R)
cluster_perm_n <- 999

# Clusters MANUAIS (setores definidos a olho a partir do layout da quinta
# -- desiguais em tamanho de proposito, representam um agrupamento espacial
# coerente e nao um numero fixo de turbinas por setor).
# Nota: BSH69 nao existe -- a numeracao das turbinas salta mesmo de BSH68
# para BSH70 (confirmado, nao e' uma turbina em falta desta lista).
# IDs sempre com 2 digitos (BSH01, nao BSH1) -- mesma convencao usada em
# curtl_dt/scada_dt/track_dt e no shapefile normalizado (ver zero-padding
# de wtg$InternalNa, secção 0 acima); Setor_E/Setor_F tinham "BSH1".."BSH9"
# sem zero, o que fazia join_curtailments_to_clusters() (por "turbine")
# nunca corresponder essas 9 turbinas -- mesmo bug de fundo do plot
# espacial (2026-08).
manual_turbine_clusters <- list(
  Setor_A = c("BSH33", "BSH34", "BSH35", "BSH36", "BSH37", "BSH38", "BSH39", "BSH40"),
  Setor_B = c("BSH41", "BSH42", "BSH43", "BSH44", "BSH45", "BSH46", "BSH47", "BSH48"),
  Setor_C = c("BSH30", "BSH31", "BSH32"),
  Setor_D = c("BSH16", "BSH17", "BSH18", "BSH19", "BSH20", "BSH21", "BSH22", "BSH23", "BSH24", "BSH25", "BSH26", "BSH27", "BSH28", "BSH29"),
  Setor_E = c("BSH05", "BSH06", "BSH07", "BSH08", "BSH09", "BSH10", "BSH11", "BSH12", "BSH13", "BSH14", "BSH15"),
  Setor_F = c("BSH01", "BSH02", "BSH03", "BSH04"),
  Setor_G = c("BSH49", "BSH50", "BSH51", "BSH52", "BSH53", "BSH54", "BSH55", "BSH56", "BSH57", "BSH58", "BSH59", "BSH60"),
  Setor_H = c("BSH61", "BSH62", "BSH63", "BSH64", "BSH65", "BSH66", "BSH67", "BSH68", "BSH70"),
  Setor_I = c("BSH71", "BSH72", "BSH73", "BSH74", "BSH75", "BSH76", "BSH77", "BSH78", "BSH79", "BSH80")
)

## -- 10.2. Kestrel (ou outra especie) track occurrence por cluster --
##
## Especie(s) a analisar -- vetor, para permitir varias de uma vez;
## Kestrel por omissao (unica nao-prioritaria com peso relevante no farm),
## reutilizavel para outra especie no futuro so' mudando este parametro
cluster_species_sel <- c("Kestrel")
