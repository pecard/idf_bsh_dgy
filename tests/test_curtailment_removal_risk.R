##
## Teste com dados simulados para R/curtailment_removal_risk.R
##
## Objetivo: confirmar o gap de tempo/distancia entre uma curtailment
## disparada enquanto classificada como removed_species (Kestrel) e a 1ª
## deteccao de especie prioritaria a seguir no MESMO track (abordagem
## conservadora pedida pelo Paulo: qualquer prioritaria a seguir, nao so' a
## classificacao final), evento a evento (nao deduplicado por track).
##
## prioritysp_test <- c("Saker-Falcon", "Steppe-Eagle")
##
## R1 -- 1 curtailment Kestrel, reclassificada para Saker-Falcon 60s depois,
##       150m mais perto -- caso "protegido", gap moderado
## R2 -- 1 curtailment Kestrel, reclassificada para Steppe-Eagle 180s
##       depois, 450m mais perto -- caso "protegido", gap grande (mais
##       critico)
## R3 -- 1 curtailment Kestrel, NUNCA reclassificada (fica Kestrel sempre)
##       -- caso "nao protegido": removendo esta curtailment, o track fica
##       sem qualquer proteccao
## R4 -- 2 curtailments Kestrel (R4a, R4b) no MESMO track, ambas antes da
##       UNICA reclassificacao para Saker-Falcon que existe nesse track --
##       confirma que cada EVENTO e' avaliado a parte (gaps diferentes),
##       mas o track so' conta 1 vez em n_tracks_affected
## R5 -- 1 curtailment Kestrel cujo ponto de track mais proximo no tempo
##       esta a 50s de distancia (> max_trigger_match_sec=30s por omissao)
##       -- x2d_at_curtailment fica NA (fora de tolerancia), mas
##       next_priority continua a ser encontrada normalmente (dist_gap_m
##       fica NA por arrastamento, time_gap_sec NAO)
##
## curtl_dt_test tem ainda uma curtailment "decoy" (species="Steppe-Eagle",
## nao "Kestrel") para confirmar que fica DE FORA da populacao analisada.
##
## Nao usa testthat -- dados sinteticos, resultado calculado a mao.
##
## Correr: source("tests/test_curtailment_removal_risk.R")
##
## Depende de: data.table
##

source("R/curtailment_removal_risk.R")

prioritysp_test <- c("Saker-Falcon", "Steppe-Eagle")
t0 <- function(base_min) as.POSIXct("2026-06-01 00:00:00", tz = "UTC") + base_min * 60

track_dt_test <- data.table(
  track_id = c(
    rep("R1", 3), rep("R2", 3), rep("R3", 3), rep("R4", 4), rep("R5", 3)
  ),
  spec = c(
    "Kestrel", "Kestrel", "Saker-Falcon",                  # R1
    "Kestrel", "Kestrel", "Steppe-Eagle",                  # R2
    "Kestrel", "Kestrel", "Kestrel",                       # R3 -- nunca prioritaria
    "Kestrel", "Kestrel", "Kestrel", "Saker-Falcon",        # R4
    "Kestrel", "Kestrel", "Saker-Falcon"                    # R5
  ),
  timestamp = c(
    t0(0)  + c(0, 30, 90),
    t0(10) + c(0, 20, 200),
    t0(20) + c(0, 15, 300),
    t0(30) + c(0, 30, 60, 120),
    t0(40) + c(0, 200, 400)
  ),
  dist3d = c(
    500, 450, 300,      # R1
    600, 550, 100,      # R2
    700, 650, 500,      # R3
    800, 750, 700, 400, # R4
    900, 850, 200       # R5
  )
)

curtl_dt_test <- data.table(
  track_id = c("R1", "R2", "R3", "R4", "R4", "R5", "R1"),
  turbine  = "TESTRM1",
  species  = c("Kestrel", "Kestrel", "Kestrel", "Kestrel", "Kestrel", "Kestrel", "Steppe-Eagle"),
  start    = c(
    t0(0) + 30, t0(10) + 20, t0(20) + 15,
    t0(30) + 30, t0(30) + 60,
    t0(40) + 50,
    t0(0) + 30 # decoy -- mesmo momento do R1, mas species != Kestrel, deve ficar de fora
  )
)

removal_dt <- evaluate_curtailment_removal_risk(
  curtl_dt_test, track_dt_test, prioritysp_test, removed_species = "Kestrel"
)

cat("\n===== evaluate_curtailment_removal_risk() =====\n")
print(removal_dt[, .(event_id, track_id, curtailment_start, x2d_at_curtailment,
                      next_priority_ts, next_priority_species, dist_at_next_priority,
                      time_gap_sec, dist_gap_m, protected_by_reclassification)])

cat(paste(
  "\nEsperado 6 eventos (a decoy 'Steppe-Eagle' fica de fora -- so' species==Kestrel entra):\n",
  "R1: x2d=450, next=Saker-Falcon 60s depois a 300m -> time_gap=60, dist_gap=150, protected=TRUE\n",
  "R2: x2d=550, next=Steppe-Eagle 180s depois a 100m -> time_gap=180, dist_gap=450, protected=TRUE\n",
  "R3: x2d=650, SEM prioritaria a seguir -> next_priority_ts=NA, time_gap=NA, dist_gap=NA, protected=FALSE\n",
  "R4a (start=+30s): x2d=750, next=Saker-Falcon em t0(30)+120s -> time_gap=90, dist_gap=350, protected=TRUE\n",
  "R4b (start=+60s): x2d=700, MESMA next (t0(30)+120s) -> time_gap=60, dist_gap=300, protected=TRUE\n",
  "R5: ponto mais proximo (0s) fica a 50s do disparo (>30s tolerancia) -> x2d_at_curtailment=NA;",
  "next=Saker-Falcon 350s depois a 200m -> time_gap=350 (nao afetado pela tolerancia),",
  "dist_gap=NA (arrasta o NA de x2d), protected=TRUE\n"
))

removal_summary <- summarise_curtailment_removal_risk(removal_dt)

cat("\n===== summarise_curtailment_removal_risk()$overview =====\n")
print(removal_summary$overview)
cat(paste(
  "\nEsperado: n_events_removed=6, n_tracks_affected=5 (R4 conta 1 vez apesar",
  "de 2 eventos), n_with_later_priority=5 (todos exceto R3), pct=83.3%,",
  "n_never_priority=1 (so' R3, genuinamente nunca-prioritario), pct=16.7%.\n"
))

cat("\n===== summarise_curtailment_removal_risk()$gap_stats =====\n")
print(removal_summary$gap_stats)
cat(paste(
  "\nEsperado (n=5 protegidos: R1,R2,R4a,R4b,R5): time_gap_sec = [60,180,90,60,350] ->",
  "mean=148.0, median=90.0, max=350.0. dist_gap_m (so' 4 nao-NA: R1=150,R2=450,R4a=350,",
  "R4b=300, R5=NA excluido) -> mean=312.5, median=325.0, max=450.0.",
  "pct_dist_gap_gt_0 = 100*4/5 = 80.0% (R5 com dist_gap NA nao conta como positivo,",
  "mas continua a contar no denominador de 5 eventos protegidos).\n"
))

cat("\n===== summarise_curtailment_removal_risk()$proximity_check =====\n")
print(removal_summary$proximity_check)
cat(paste(
  "\nEsperado: proximity_threshold_m=100 (omissao), n_protected_events=5.",
  "dist_at_next_priority: R1=300, R2=100, R4a=400, R4b=400, R5=200 -- so' R2",
  "fica <=100 (igual conta como dentro) -> n_within_threshold=1, pct=20.0%.",
  "Nota: isto e' DIFERENTE do dist_gap_m visto acima -- aqui e' a distancia",
  "ABSOLUTA no momento da reclassificacao, nao a variacao face ao momento",
  "do disparo original.\n"
))

cat("\n===== summarise_curtailment_removal_risk()$by_next_priority_species =====\n")
print(removal_summary$by_next_priority_species)
cat(paste(
  "\nEsperado 2 linhas, ordenadas por n_events decrescente:\n",
  "Saker-Falcon: n_events=4 (R1,R4a,R4b,R5) -- time_gap=[60,90,60,350] -> mean=140.0,",
  "median=75.0; dist_gap (3 nao-NA: 150,350,300) -> mean=266.7, median=300.0;",
  "dist_at_next_priority=[300,400,400,200], nenhum <=100 ->",
  "n_within_proximity_threshold=0, pct=0.0%\n",
  "Steppe-Eagle: n_events=1 (R2) -- mean_time_gap=180.0, median_time_gap=180.0,",
  "mean_dist_gap=450.0, median_dist_gap=450.0; dist_at_next_priority=100 <=100 ->",
  "n_within_proximity_threshold=1, pct=100.0%\n"
))

cat("\n===== print_curtailment_removal_risk_summary() =====\n")
print_curtailment_removal_risk_summary(removal_summary, "Kestrel")
cat(paste(
  "\nEsperado: os mesmos numeros acima, so' formatados em texto corrido -- confirmar que nao da erro.",
  "Confirmar tambem a nova linha do proximity check (1 de 5 eventos, 20.0%,",
  "dentro de 100m) e a coluna extra por especie (Saker-Falcon 0 de 4, 0.0%;",
  "Steppe-Eagle 1 de 1, 100.0%).\n"
))

## ---- curtailment_removal_case_detail() ----
## Steppe-Eagle so' tem 1 evento (R2) -- mais simples para conferir a mao
## que a linha COMPLETA de curtl_dt_test volta, com as colunas extra do
## removal_dt anexadas

case_detail_dt <- curtailment_removal_case_detail(removal_dt, curtl_dt_test, "Steppe-Eagle")
cat("\n===== curtailment_removal_case_detail('Steppe-Eagle') =====\n")
print(case_detail_dt)
cat(paste(
  "\nEsperado: 1 linha -- track_id=R2, turbine=TESTRM1, species=Kestrel,",
  "start=t0(10)+20s (a curtailment original completa, com TODAS as colunas",
  "de curtl_dt_test), mais next_priority_ts=t0(10)+200s,",
  "next_priority_species=Steppe-Eagle, x2d_at_curtailment=550,",
  "dist_at_next_priority=100, time_gap_sec=180, dist_gap_m=450.\n"
))
