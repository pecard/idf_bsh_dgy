##
## Teste com dados simulados para R/id_transitions.R
##
## Objetivo: confirmar a contagem de especies distintas por track_id
## (riqueza/histograma + entropia de Shannon), e a classificacao de risco de
## transicao P<->NP entre tracks multi-ID, incluindo os 2 criterios de
## "curtailment tarde demais" (por tempo e por distancia) pedidos pelo Paulo
## depois de apontar que "nenhum curtailment disparado" nao cobre o caso de
## um curtailment disparado TARDE (a reclassificacao NP->P so' acontecer
## quando a ave ja esta perto demais):
##
##   - "P_to_NP_unnecessary_curtailment": curtailment disparado, especie
##     final nao-prioritaria -- custo de producao (inalterado desde a 1ª
##     versao deste modulo).
##   - "NP_to_P_no_curtailment_near"/"_far": NENHUM curtailment disparado,
##     especie final prioritaria -- so' "_near" (ave ja dentro de
##     late_dist_threshold_m quando reclassificada) e' gap real; "_far" e'
##     so' descritivo (nunca chegou perto o suficiente para justificar
##     curtailment). Separacao adicionada depois de, nos dados reais de
##     Bash, ~99% dos "sem curtailment" carem em "_far" -- sem isto a
##     contagem agregada sugeria um problema ~85x maior do que a distancia
##     justificava.
##   - "NP_to_P_late_curtailment": curtailment disparado, especie final
##     prioritaria, MAS tarde demais por >=1 dos 2 criterios:
##       - late_by_time: gap entre a 1ª deteccao ja prioritaria e o inicio
##         do curtailment > late_time_threshold_sec
##       - late_by_dist: distancia (dist3d) da ave a turbina na 1ª deteccao
##         ja prioritaria <= late_dist_threshold_m (independente do tempo)
##   - "no_risk": ou estavel (1 unica especie, nem entra nesta tabela), ou
##     NP->P com curtailment a tempo por AMBOS os criterios.
##
## Nao usa testthat -- script normal que constroi dados sinteticos, corre as
## funcoes reais, e imprime "esperado vs obtido" para inspecionares.
##
## Correr: source("tests/test_id_transitions.R")
##
## Depende de: data.table
##

source("R/id_transitions.R")

test_prioritysp <- c("Steppe-Eagle", "Golden-Eagle") # subconjunto local, independente do userSettings_BSH.R
test_late_time_threshold_sec <- 50  # mesmo default do modulo / id_transition_late_time_sec
test_late_dist_threshold_m   <- 100 # mesmo default do modulo / track_proximity_threshold_m

t0 <- function(base_min) as.POSIXct("2026-04-01 00:00:00", tz = "UTC") + base_min * 60

##
## Tracks sinteticos -- cada um com o seu proprio "base" (bloco de minutos),
## para nao haver ambiguidade de timestamps entre tracks. species/timestamp/
## dist3d ja em ordem cronologica (track_species_summary() e
## track_first_priority_state() assumem isso, tal como track_dt real vem
## ordenado por R/read_tracks.R).
##
## T1  -- estavel, 1 especie prioritaria -- fora da tabela de risco (so'
##        entra na riqueza/entropia)
## T6  -- estavel, 1 especie prioritaria, com curtailment -- idem
## T2  -- NP -> P, curtailment PROMPT (5s depois, longe da turbina) -- no_risk
## T3  -- P -> NP, curtailment disparado -- P_to_NP_unnecessary_curtailment
## T4a -- NP -> P, SEM curtailment, longe (400m) quando reclassificada --
##        NP_to_P_no_curtailment_far (descritivo, nao alarmante)
## T4b -- NP -> P, curtailment disparado mas 100s depois (>50s) -- late SO' por tempo
## T4c -- NP -> P, curtailment disparado so' 2s depois (rapido!), MAS a ave
##        ja estava a 80m (<=100m) quando foi reclassificada -- late SO' por
##        distancia -- este e' o caso concreto que o Paulo levantou: rapido
##        em tempo nao chega se a reclassificacao em si ja' foi tardia
## T4d -- NP -> P, SEM curtailment, so' 70m (<=100m) quando reclassificada --
##        NP_to_P_no_curtailment_near (gap de proteccao real)
## T5  -- NP -> NP (troca entre 2 nao-prioritarias), sem curtailment -- no_risk
##

track_dt <- data.table(
  track_id = c(
    rep("T1", 5), rep("T6", 3), rep("T2", 3), rep("T3", 4),
    rep("T4a", 4), rep("T4b", 4), rep("T4c", 3), rep("T4d", 3), rep("T5", 4)
  ),
  spec = c(
    rep("Steppe-Eagle", 5),                                     # T1
    rep("Golden-Eagle", 3),                                     # T6
    "Kestrel", "Steppe-Eagle", "Steppe-Eagle",                  # T2
    "Steppe-Eagle", "Steppe-Eagle", "Kestrel", "Kestrel",       # T3
    "Kestrel", "Kestrel", "Steppe-Eagle", "Steppe-Eagle",       # T4a
    "Kestrel", "Kestrel", "Steppe-Eagle", "Steppe-Eagle",       # T4b
    "Kestrel", "Steppe-Eagle", "Steppe-Eagle",                  # T4c
    "Kestrel", "Steppe-Eagle", "Steppe-Eagle",                  # T4d
    "Kestrel", "Kestrel", "Common-Buzzard", "Common-Buzzard"    # T5
  ),
  timestamp = c(
    t0(0)  + c(0, 10, 20, 30, 40),
    t0(10) + c(0, 10, 20),
    t0(20) + c(0, 10, 20),
    t0(30) + c(0, 10, 20, 30),
    t0(40) + c(0, 10, 20, 30),
    t0(50) + c(0, 10, 20, 30),
    t0(60) + c(0, 10, 20),
    t0(80) + c(0, 10, 20),
    t0(70) + c(0, 10, 20, 30)
  ),
  dist3d = c(
    c(500, 400, 300, 200, 100),   # T1 -- irrelevante (single-ID, fora da tabela de risco)
    c(300, 250, 200),             # T6 -- idem
    c(500, 450, 400),             # T2 -- 1ª deteccao prioritaria (t=10) a 450m -- longe
    c(600, 550, 500, 450),        # T3 -- irrelevante (P->NP, late_by_dist exige last_is_priority)
    c(500, 450, 400, 350),        # T4a -- 1ª deteccao prioritaria (t=20) a 400m -- longe, mas sem curtailment
    c(500, 450, 400, 350),        # T4b -- 1ª deteccao prioritaria (t=20) a 400m -- longe, mas curtailment so' aos 100s
    c(200, 80, 50),                # T4c -- 1ª deteccao prioritaria (t=10) a SO' 80m -- ja perto ao reclassificar
    c(200, 70, 50),                # T4d -- 1ª deteccao prioritaria (t=10) a SO' 70m, SEM curtailment
    c(500, 450, 400, 350)         # T5 -- nunca prioritaria
  )
)

curtl_dt <- data.table(
  track_id = c("T6", "T2", "T3", "T4b", "T4c"),
  turbine  = c("TESTID0", "TESTID1", "TESTID2", "TESTID3", "TESTID4"),
  start    = c(
    t0(10) + 0,   # T6 -- prompt, so' para dar dado a este track (single-ID, nao entra na tabela de risco)
    t0(20) + 15,  # T2 -- 1ª deteccao prioritaria em t0(20)+10 -> curtailment 5s depois -- prompt
    t0(30) + 5,   # T3 -- irrelevante para o timing (P->NP nao usa late_by_time/dist)
    t0(50) + 120, # T4b -- 1ª deteccao prioritaria em t0(50)+20 -> curtailment 100s depois -- TARDE (tempo)
    t0(60) + 12   # T4c -- 1ª deteccao prioritaria em t0(60)+10 -> curtailment so' 2s depois -- rapido, MAS ja a 80m
  )
)


##
## PARTE A -- riqueza de especies por track (histograma) + entropia
## (inalterado desde a 1ª versao deste modulo -- mesma logica, mais tracks)
##

richness_dt <- track_species_summary(track_dt)

cat("\n===== PARTE A: track_species_summary() =====\n")
print(richness_dt[order(track_id)])

expected_n_species <- data.table(
  track_id           = c("T1", "T6", "T2", "T3", "T4a", "T4b", "T4c", "T4d", "T5"),
  expected_n_species = c(1, 1, 2, 2, 2, 2, 2, 2, 2),
  expected_last_spec = c(
    "Steppe-Eagle", "Golden-Eagle", "Steppe-Eagle", "Kestrel",
    "Steppe-Eagle", "Steppe-Eagle", "Steppe-Eagle", "Steppe-Eagle", "Common-Buzzard"
  )
)
check_a <- merge(richness_dt, expected_n_species, by = "track_id")
check_a[, n_species_ok := n_species == expected_n_species]
check_a[, last_species_ok := last_species == expected_last_spec]

cat("\nCheck n_species e last_species (esperado vs obtido):\n")
print(check_a[, .(track_id, n_species, expected_n_species, n_species_ok, last_species, expected_last_spec, last_species_ok)])
cat(sprintf(
  "\nResultado: %d/%d n_species corretos, %d/%d last_species corretos.\n",
  sum(check_a$n_species_ok), nrow(check_a), sum(check_a$last_species_ok), nrow(check_a)
))


##
## PARTE B -- classificacao de risco P<->NP, com os 2 criterios de "tarde
## demais" (o foco deste teste)
##

risk_dt <- classify_id_transition_risk(
  richness_dt, track_dt, curtl_dt, test_prioritysp,
  late_time_threshold_sec = test_late_time_threshold_sec,
  late_dist_threshold_m = test_late_dist_threshold_m
)

cat("\n\n===== PARTE B: classify_id_transition_risk() =====\n")
print(risk_dt[order(track_id), .(
  track_id, triggered_curtailment, last_is_priority,
  time_to_curtailment_after_priority_sec, dist_at_first_priority,
  late_by_time, late_by_dist, risk_direction
)])

expected_risk <- data.table(
  track_id                = c("T2",      "T3",                               "T4a",                        "T4b",                     "T4c",                     "T4d",                       "T5"),
  expected_risk_direction = c("no_risk", "P_to_NP_unnecessary_curtailment", "NP_to_P_no_curtailment_far", "NP_to_P_late_curtailment", "NP_to_P_late_curtailment", "NP_to_P_no_curtailment_near", "no_risk"),
  expected_late_by_time   = c(FALSE, NA, NA, TRUE, FALSE, NA, NA),
  expected_late_by_dist   = c(FALSE, NA, FALSE, FALSE, TRUE, TRUE, NA)
)
check_b <- merge(risk_dt, expected_risk, by = "track_id")
check_b[, risk_ok := risk_direction == expected_risk_direction]
check_b[, late_time_ok := is.na(expected_late_by_time) | late_by_time == expected_late_by_time]
check_b[, late_dist_ok := is.na(expected_late_by_dist) | late_by_dist == expected_late_by_dist]

cat("\nCheck risk_direction/late_by_time/late_by_dist (esperado vs obtido):\n")
print(check_b[, .(track_id, risk_direction, expected_risk_direction, risk_ok,
                   late_by_time, late_by_dist, late_time_ok, late_dist_ok)])
cat(sprintf(
  "\nResultado: %d/%d risk_direction corretos, %d/%d late_by_time corretos, %d/%d late_by_dist corretos.\n",
  sum(check_b$risk_ok), nrow(check_b), sum(check_b$late_time_ok), nrow(check_b),
  sum(check_b$late_dist_ok), nrow(check_b)
))

cat(paste(
  "\nNota sobre T4b vs T4c -- o par que motivou os 2 criterios: em ambos a",
  "especie final e' prioritaria e HOUVE curtailment (nao sao",
  "'no_curtailment'). T4b dispara depressa a olhar para a distancia (longe,",
  "400m, quando reclassificado) mas so' 100s depois no tempo -> late SO'",
  "por tempo. T4c e' o inverso: dispara rapidissimo em tempo (2s!) mas a",
  "ave ja estava a 80m da turbina no momento em que foi reclassificada",
  "como prioritaria -> late SO' por distancia, mesmo com resposta",
  "'imediata'. Um criterio sozinho perderia um dos dois casos -- e' esta a",
  "lacuna que motivou correr os 2 em paralelo.\n"
))

cat(paste(
  "\nNota sobre T4a vs T4d -- o par que motivou a separacao near/far do",
  "'sem curtailment': em nenhum dos dois ha curtailment, mas T4a estava a",
  "400m (longe) quando foi reclassificada -- nunca precisou de curtailment",
  "por proximidade, e' so descritivo (_far). T4d estava a 70m (perto) -- gap",
  "de proteccao real (_near). Sem esta distincao os dois cairiam no mesmo",
  "balde 'NP_to_P_no_curtailment', escondendo que so um dos dois e'",
  "realmente preocupante -- exatamente o que aconteceu nos dados reais de",
  "Bash (29.483 '_far' vs 354 '_near', ver conversa com o Paulo).\n"
))


##
## PARTE C -- sumarios para relatorio, incluindo a comparacao dos 2 criterios
##

risk_summary <- summarise_id_transition_risk(risk_dt, curtl_dt)

cat("\n\n===== PARTE C: summarise_id_transition_risk() =====\n")
cat("\n----- by_direction -----\n")
print(risk_summary$by_direction)
cat(paste(
  "\nEsperado (n_multi_id_tracks=7: T2,T3,T4a,T4b,T4c,T4d,T5): no_risk=2",
  "(T2,T5), P_to_NP_unnecessary_curtailment=1 (T3),",
  "NP_to_P_no_curtailment_far=1 (T4a), NP_to_P_no_curtailment_near=1 (T4d),",
  "NP_to_P_late_curtailment=2 (T4b,T4c).\n"
))

cat("\n----- pnp_curtailments -----\n")
print(risk_summary$pnp_curtailments)
cat(paste(
  "\nEsperado: total_curtailments=5 (T6,T2,T3,T4b,T4c). De entre esses,",
  "curtailments_from_multi_id_tracks=4 (T2,T3,T4b,T4c -- T6 e' single-ID,",
  "nao conta; T4a/T4d nunca dispararam curtailment).",
  "curtailments_due_to_p_to_np=1 (a curtailment de T3) -- pct_of_total =",
  "1/5 = 20.0%.\n"
))

cat("\n----- late_criteria_compare -----\n")
print(risk_summary$late_criteria_compare)
cat(paste(
  "\nEsperado: 3 tracks entram nesta comparacao (NP->P E com curtailment:",
  "T2, T4b, T4c). T2 -> (late_by_time=FALSE, late_by_dist=FALSE): 1. T4b ->",
  "(TRUE, FALSE): 1. T4c -> (FALSE, TRUE): 1. Nenhum caso (TRUE,TRUE) neste",
  "conjunto sintetico -- os 2 criterios discordam completamente aqui, o que",
  "e' deliberado para se ver os 2 tipos de deteccao lado a lado antes de",
  "decidir qual manter.\n"
))
