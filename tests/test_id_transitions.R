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
## track_dt_test/curtl_dt_test (sufixo _test) em vez de track_dt/curtl_dt --
## convencao do projeto (ver CLAUDE.md): objetos sinteticos de teste nunca
## usam o nome de um objeto real do pipeline (track_dt, curtl_dt, scada_dt,
## heartb_dt, wtg, idf, ...), para nao os substituir por engano se este
## script for corrido na mesma sessao R que o IDF_analysis.R. So' usar o
## nome original quando o teste referencia mesmo o objeto real em memoria
## (ver tests/smoke_test_coverage_3d.R).
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
## T4e -- NP -> P, curtailment disparado 150s depois (>50s) E a ave ja
##        estava a 90m (<=100m) quando reclassificada -- late pelos 2
##        criterios em simultaneo ("both", o grupo mais grave da vista de
##        inspecao caso a caso -- PARTE D)
## T5  -- NP -> NP (troca entre 2 nao-prioritarias), sem curtailment -- no_risk
##

track_dt_test <- data.table(
  track_id = c(
    rep("T1", 5), rep("T6", 3), rep("T2", 3), rep("T3", 4),
    rep("T4a", 4), rep("T4b", 4), rep("T4c", 3), rep("T4d", 3), rep("T4e", 3), rep("T5", 4)
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
    "Kestrel", "Steppe-Eagle", "Steppe-Eagle",                  # T4e
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
    t0(90) + c(0, 10, 20),
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
    c(200, 90, 60),                # T4e -- 1ª deteccao prioritaria (t=10) a 90m -- perto, E curtailment tarde
    c(500, 450, 400, 350)         # T5 -- nunca prioritaria
  )
)

curtl_dt_test <- data.table(
  track_id = c("T6", "T2", "T3", "T4b", "T4c", "T4e"),
  turbine  = c("TESTID0", "TESTID1", "TESTID2", "TESTID3", "TESTID4", "TESTID5"),
  start    = c(
    t0(10) + 0,   # T6 -- prompt, so' para dar dado a este track (single-ID, nao entra na tabela de risco)
    t0(20) + 15,  # T2 -- 1ª deteccao prioritaria em t0(20)+10 -> curtailment 5s depois -- prompt
    t0(30) + 5,   # T3 -- irrelevante para o timing (P->NP nao usa late_by_time/dist)
    t0(50) + 120, # T4b -- 1ª deteccao prioritaria em t0(50)+20 -> curtailment 100s depois -- TARDE (tempo)
    t0(60) + 12,  # T4c -- 1ª deteccao prioritaria em t0(60)+10 -> curtailment so' 2s depois -- rapido, MAS ja a 80m
    t0(90) + 160  # T4e -- 1ª deteccao prioritaria em t0(90)+10 -> curtailment 150s depois -- TARDE (tempo E ja a 90m)
  )
)


##
## PARTE A -- riqueza de especies por track (histograma) + entropia bruta e
## normalizada (shannon_evenness_pct, 0-100% -- adicionada depois do Paulo
## achar o indice bruto "abstrato, sem referencia numerica", 2026-08: o
## maximo teorico de H e' log(n_species), diferente por track, por isso H
## bruto nao e' diretamente comparavel entre tracks com nº de especies
## diferente; a versao normalizada tem sempre teto 100%)
##

richness_dt <- track_species_summary(track_dt_test)

cat("\n===== PARTE A: track_species_summary() =====\n")
print(richness_dt[order(track_id)])

expected_n_species <- data.table(
  track_id           = c("T1", "T6", "T2", "T3", "T4a", "T4b", "T4c", "T4d", "T4e", "T5"),
  expected_n_species = c(1, 1, 2, 2, 2, 2, 2, 2, 2, 2),
  expected_last_spec = c(
    "Steppe-Eagle", "Golden-Eagle", "Steppe-Eagle", "Kestrel",
    "Steppe-Eagle", "Steppe-Eagle", "Steppe-Eagle", "Steppe-Eagle", "Steppe-Eagle", "Common-Buzzard"
  ),
  # T1/T6 (1 especie): 0% por definicao. T3/T4a/T4b/T5 (split 50/50 entre 2
  # especies): H = log(2) = H_max -> 100%. T2/T4c/T4d/T4e (split 1/3, 2/3
  # entre 2 especies -- menos equilibrado): H = log(3) - (2/3)log(2) =
  # 0.6365142 -> 100*0.6365142/log(2) = 91.8%.
  expected_evenness_pct = c(0, 0, 91.8, 100, 100, 100, 91.8, 91.8, 91.8, 100)
)
check_a <- merge(richness_dt, expected_n_species, by = "track_id")
check_a[, n_species_ok := n_species == expected_n_species]
check_a[, last_species_ok := last_species == expected_last_spec]
check_a[, evenness_ok := shannon_evenness_pct == expected_evenness_pct]

cat("\nCheck n_species, last_species e shannon_evenness_pct (esperado vs obtido):\n")
print(check_a[, .(track_id, n_species, expected_n_species, n_species_ok, last_species, expected_last_spec,
                   last_species_ok, shannon_evenness_pct, expected_evenness_pct, evenness_ok)])
cat(sprintf(
  "\nResultado: %d/%d n_species corretos, %d/%d last_species corretos, %d/%d shannon_evenness_pct corretos.\n",
  sum(check_a$n_species_ok), nrow(check_a), sum(check_a$last_species_ok), nrow(check_a),
  sum(check_a$evenness_ok), nrow(check_a)
))

richness_summary <- summarise_species_richness(richness_dt)
cat("\n----- summarise_species_richness()$entropy -----\n")
print(richness_summary$entropy)
cat(paste(
  "\nEsperado (10 tracks: 2 com H=0/0%, 4 com H=0.6365142/91.8% (split 1/3-2/3:",
  "T2,T4c,T4d,T4e), 4 com H=0.6931472/100% (split 50/50: T3,T4a,T4b,T5)):",
  "mean_entropy ~= 0.5319, median_entropy = 0.6365142, mean_evenness_pct =",
  "76.7, median_evenness_pct = 91.8.\n"
))


##
## PARTE B -- classificacao de risco P<->NP, com os 2 criterios de "tarde
## demais" (o foco deste teste)
##

risk_dt <- classify_id_transition_risk(
  richness_dt, track_dt_test, curtl_dt_test, test_prioritysp,
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
  track_id                = c("T2",      "T3",                               "T4a",                        "T4b",                     "T4c",                     "T4d",                        "T4e",                     "T5"),
  expected_risk_direction = c("no_risk", "P_to_NP_unnecessary_curtailment", "NP_to_P_no_curtailment_far", "NP_to_P_late_curtailment", "NP_to_P_late_curtailment", "NP_to_P_no_curtailment_near", "NP_to_P_late_curtailment", "no_risk"),
  expected_late_by_time   = c(FALSE, NA, NA, TRUE, FALSE, NA, TRUE, NA),
  expected_late_by_dist   = c(FALSE, NA, FALSE, FALSE, TRUE, TRUE, TRUE, NA)
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

risk_summary <- summarise_id_transition_risk(risk_dt, curtl_dt_test)

cat("\n\n===== PARTE C: summarise_id_transition_risk() =====\n")
cat("\n----- by_direction -----\n")
print(risk_summary$by_direction)
cat(paste(
  "\nEsperado (n_multi_id_tracks=8: T2,T3,T4a,T4b,T4c,T4d,T4e,T5): no_risk=2",
  "(T2,T5), P_to_NP_unnecessary_curtailment=1 (T3),",
  "NP_to_P_no_curtailment_far=1 (T4a), NP_to_P_no_curtailment_near=1 (T4d),",
  "NP_to_P_late_curtailment=3 (T4b,T4c,T4e).\n"
))

cat("\n----- pnp_curtailments -----\n")
print(risk_summary$pnp_curtailments)
cat(paste(
  "\nEsperado: total_curtailments=6 (T6,T2,T3,T4b,T4c,T4e). De entre esses,",
  "curtailments_from_multi_id_tracks=5 (T2,T3,T4b,T4c,T4e -- T6 e' single-ID,",
  "nao conta; T4a/T4d nunca dispararam curtailment).",
  "curtailments_due_to_p_to_np=1 (a curtailment de T3) -- pct_of_total =",
  "1/6 = 16.7%.\n"
))

cat("\n----- late_criteria_compare -----\n")
print(risk_summary$late_criteria_compare)
cat(paste(
  "\nEsperado: 4 tracks entram nesta comparacao (NP->P E com curtailment:",
  "T2, T4b, T4c, T4e). T2 -> (FALSE, FALSE): 1. T4b -> (TRUE, FALSE): 1.",
  "T4c -> (FALSE, TRUE): 1. T4e -> (TRUE, TRUE): 1 -- o caso 'both' que",
  "faltava no conjunto anterior, agora coberto.\n"
))


##
## PARTE D -- vista detalhada dos 3 casos "NP_to_P_late_curtailment", para
## inspecao caso a caso (o proximo passo pedido pelo Paulo depois de ver os
## resultados reais de Bash: rever os 146 casos individualmente antes de
## decidir entre late_by_time/late_by_dist)
##

late_cases_dt <- id_transition_late_cases(risk_dt, curtl_dt_test)

cat("\n\n===== PARTE D: id_transition_late_cases() =====\n")
print(late_cases_dt[, .(track_id, turbine, late_severity, dist_at_first_priority,
                         time_to_curtailment_after_priority_sec, late_by_time, late_by_dist)])

expected_order <- c("T4e", "T4c", "T4b")
order_ok <- identical(late_cases_dt$track_id, expected_order)
cat(sprintf(
  "\nOrdem esperada: %s (grupo 'both' primeiro -- T4e, unico caso -- depois",
  paste(expected_order, collapse = ", ")
))
cat(sprintf(
  "\n'dist_only' -- T4c -- depois 'time_only' -- T4b). Obtida: %s. Ordem correta: %s.\n",
  paste(late_cases_dt$track_id, collapse = ", "), order_ok
))
cat(paste(
  "\nCom os 146 casos reais de Bash, esta vista fica: todos os 'both'",
  "primeiro (ordenados por distancia, mais perto primeiro), depois todos os",
  "'dist_only' (idem), depois todos os 'time_only' (ordenados por atraso,",
  "maior atraso primeiro) -- para percorreres do caso mais para o menos",
  "preocupante. A coluna turbine e' do 1º curtailment do track, para",
  "cruzares com o portal IdentiFlight.\n"
))


##
## PARTE E -- sensibilidade dos 2 limiares "tarde demais", para despistar se
## algum e' pouco efetivo/irrelevante (Paulo decidiu manter os 2, mas quer
## confirmar que ambos discriminam alguma coisa antes do relatorio mensal)
##

time_sens_dt <- id_transition_late_time_sensitivity(risk_dt)

cat("\n\n===== PARTE E: id_transition_late_time_sensitivity() =====\n")
print(time_sens_dt)
cat(paste(
  "\nBase = tracks NP->P com curtailment disparado (T2=5s, T4b=100s,",
  "T4c=2s, T4e=150s; n_base=4). Esperado por limiar: 10/20/30/50/75s -> 2",
  "sinalizados (T4b,T4e, os unicos >50s); 100s -> 1 (so' T4e, 150>100;",
  "T4b=100 NAO e' >100); 150/200/300s -> 0 (nenhum valor chega a superar",
  "150). O 'degrau' entre 75s e 150s mostra que o criterio esta a",
  "discriminar dentro dessa gama -- com um conjunto de 146 casos reais,",
  "olhar para onde esse degrau acontece ajuda a perceber se 50s e' um corte",
  "razoavel ou se esta a meio de uma nuvem continua sem uma quebra clara.\n"
))

dist_sens_dt <- id_transition_late_dist_sensitivity(risk_dt)

cat("\n----- id_transition_late_dist_sensitivity() -----\n")
print(dist_sens_dt)
cat(paste(
  "\nBase = tracks NP->P (com ou sem curtailment): T2=450m, T4a=400m,",
  "T4b=400m, T4c=80m, T4d=70m, T4e=90m; n_base=6. Esperado por limiar:",
  "25/50m -> 0; 75m -> 1 (so' T4d, 70m); 100/150/200/300m -> 3 (T4c,T4d,T4e",
  "-- os 3 com dist <=100m); 500m -> 6 (todos, incluindo os 3 a 400-450m).",
  "Dois degraus claros aqui (perto de 75-100m, e perto de 500m) --",
  "novamente, com os dados reais o interessante e' ver se ha um vale",
  "parecido perto de 100m ou se a subida e' gradual (nesse caso o limiar",
  "atual seria mais arbitrario).\n"
))

cat(paste(
  "\nNota geral sobre a PARTE E: um criterio 'pouco efetivo' mostraria a",
  "MESMA contagem em toda a gama de limiares testada (ex: sempre 0, ou",
  "sempre = n_base) -- sinal de que a distribuicao nao tem massa relevante",
  "perto de nenhum limiar plausivel, e o corte concreto escolhido nao muda",
  "nada. Os graficos plot_late_time_distribution()/plot_late_dist_distribution()",
  "mostram a mesma informacao visualmente, com uma linha vertical no limiar",
  "atual do projeto -- uteis para ver se esse limiar cai num vale natural",
  "da distribuicao real (bom sinal) ou a meio de uma nuvem sem quebra clara",
  "(limiar mais arbitrario, mas nao necessariamente errado).\n"
))


##
## PARTE F -- matriz de confusao de especies (que outras especies aparecem
## no mesmo track), em geral e restrita a tracks com curtailment, com foco
## em Kestrel (pedido do Paulo, 2026-08)
##
## Dataset PROPRIO desta parte (track_dt_confusion_test/curtl_dt_confusion_test,
## nao reutiliza track_dt_test/curtl_dt_test das Partes A-E) -- inclui de
## proposito um track Kestrel "puro" (K1, nunca confundido) para nao dar um
## resultado degenerado de 100% confuso em toda a parte, o que aconteceria
## se reutilizasse o dataset das Partes A-E tal como esta (la' todos os
## tracks com Kestrel sao multi-ID).
##

t0f <- function(base_min) as.POSIXct("2026-05-01 00:00:00", tz = "UTC") + base_min * 60

## K1 -- Kestrel puro, SEM curtailment (nunca confundido)
## K2 -- Kestrel puro, COM curtailment (confirma que "puro" nao exige
##       ausencia de curtailment -- sao perguntas independentes)
## K3 -- Kestrel + Steppe-Eagle, SEM curtailment
## K4 -- Kestrel + Steppe-Eagle, COM curtailment
## K5 -- Kestrel + Common-Buzzard, COM curtailment
## K6 -- so' Steppe-Eagle (nunca Kestrel), COM curtailment -- "decoy": deve
##       ficar de fora de qualquer resultado especifico de Kestrel

track_dt_confusion_test <- data.table(
  track_id = c(rep("K1", 3), rep("K2", 2), rep("K3", 2), rep("K4", 2), rep("K5", 2), rep("K6", 2)),
  spec = c(
    rep("Kestrel", 3),                    # K1
    rep("Kestrel", 2),                    # K2
    c("Kestrel", "Steppe-Eagle"),         # K3
    c("Kestrel", "Steppe-Eagle"),         # K4
    c("Kestrel", "Common-Buzzard"),       # K5
    rep("Steppe-Eagle", 2)                # K6
  ),
  timestamp = c(
    t0f(0)  + c(0, 10, 20),
    t0f(10) + c(0, 10),
    t0f(20) + c(0, 10),
    t0f(30) + c(0, 10),
    t0f(40) + c(0, 10),
    t0f(50) + c(0, 10)
  )
)

curtl_dt_confusion_test <- data.table(
  track_id = c("K2", "K4", "K5", "K6"),
  turbine  = "TESTCONF",
  species  = c("Kestrel", "Steppe-Eagle", "Common-Buzzard", "Steppe-Eagle"),
  start    = c(t0f(10) + 5, t0f(30) + 5, t0f(40) + 5, t0f(50) + 5)
)

richness_dt_confusion_test <- track_species_summary(track_dt_confusion_test)

general_pairs_dt <- species_confusion_pairs(track_dt_confusion_test)
cat("\n\n===== PARTE F: species_confusion_pairs() -- geral =====\n")
print(general_pairs_dt)
cat(paste(
  "\nEsperado: n_multi_id_tracks=3 (K3,K4,K5 -- K1,K2,K6 sao single-species,",
  "ficam de fora). (Kestrel,Steppe-Eagle): n_tracks=2 (K3,K4), pct=66.7%.",
  "(Common-Buzzard,Kestrel): n_tracks=1 (K5), pct=33.3% (ordem alfabetica",
  "dentro do par, Common-Buzzard < Kestrel).\n"
))

curtl_track_ids_f <- unique(as.character(curtl_dt_confusion_test$track_id))
curtl_pairs_dt <- species_confusion_pairs(track_dt_confusion_test, track_ids = curtl_track_ids_f)
cat("\n===== species_confusion_pairs() -- so' tracks com curtailment (K2,K4,K5,K6) =====\n")
print(curtl_pairs_dt)
cat(paste(
  "\nEsperado: dentro deste subconjunto so' K4/K5 sao multi-ID (K2 e' Kestrel",
  "puro, K6 nao tem Kestrel) -- n_multi_id_tracks=2. (Kestrel,Steppe-Eagle):",
  "n_tracks=1 (K4), pct=50.0%. (Common-Buzzard,Kestrel): n_tracks=1 (K5),",
  "pct=50.0% (empate -- ordem entre as 2 linhas pode variar, os valores e'",
  "que importam).\n"
))

involving_general_dt <- species_confusion_involving(general_pairs_dt, "Kestrel")
cat("\n===== species_confusion_involving() -- Kestrel, geral =====\n")
print(involving_general_dt)
cat(paste(
  "\nEsperado: 2 linhas -- Steppe-Eagle n_tracks=2 (66.7%), Common-Buzzard",
  "n_tracks=1 (33.3%), por esta ordem (ordenado por n_tracks decrescente).\n"
))

involving_curtl_dt <- species_confusion_involving(curtl_pairs_dt, "Kestrel")
cat("\n===== species_confusion_involving() -- Kestrel, so' curtailment tracks =====\n")
print(involving_curtl_dt)
cat("\nEsperado: 2 linhas -- Steppe-Eagle e Common-Buzzard, ambas n_tracks=1 (50.0%).\n")

rate_general_dt <- species_confusion_rate(track_dt_confusion_test, richness_dt_confusion_test, "Kestrel")
cat("\n===== species_confusion_rate() -- Kestrel, geral =====\n")
print(rate_general_dt)
cat(paste(
  "\nEsperado: tracks com Kestrel em algum momento = K1,K2,K3,K4,K5 (K6 fica",
  "de fora, nunca teve Kestrel) -> n_tracks_total=5. Puros (n_species==1):",
  "K1,K2 -> n_tracks_pure=2. Confusos: K3,K4,K5 -> n_tracks_confused=3,",
  "pct_confused=60.0%.\n"
))

rate_curtl_dt <- species_confusion_rate(
  track_dt_confusion_test, richness_dt_confusion_test, "Kestrel", track_ids = curtl_track_ids_f
)
cat("\n===== species_confusion_rate() -- Kestrel, so' curtailment tracks =====\n")
print(rate_curtl_dt)
cat(paste(
  "\nEsperado: dentro de K2,K4,K5,K6, so' K2/K4/K5 tem Kestrel (K6 fica de",
  "fora mesmo estando no subconjunto de curtailment, porque nunca teve",
  "Kestrel) -> n_tracks_total=3. Puro: K2 -> n_tracks_pure=1. Confusos:",
  "K4,K5 -> n_tracks_confused=2, pct_confused=66.7% -- MAIOR do que o geral",
  "(60.0%), sinal (neste exemplo sintetico) de que a confusao pesa mais",
  "precisamente nos tracks que dispararam curtailment.\n"
))

confusion_summary_test <- summarise_species_confusion(
  track_dt_confusion_test, richness_dt_confusion_test, curtl_dt_confusion_test, "Kestrel"
)
cat("\n===== summarise_species_confusion() -- rate_compare =====\n")
print(confusion_summary_test$rate_compare)
cat(paste(
  "\nEsperado: 2 linhas (scope='all_tracks' com os valores gerais acima,",
  "scope='curtailment_tracks' com os valores restritos acima) -- confirma",
  "que a funcao de conveniencia junta exatamente os resultados ja",
  "verificados em separado nesta Parte F.\n"
))

cat("\n===== print_species_confusion_summary() =====\n")
print_species_confusion_summary(confusion_summary_test, "Kestrel")
cat(paste(
  "\nEsperado: os mesmos numeros impressos acima (5/2/3/60.0%, 3/1/2/66.7%),",
  "so' formatados em texto corrido -- confirmar que nao da erro e que",
  "Steppe-Eagle/Common-Buzzard aparecem nas 2 listas de 'Confused with'",
  "com as contagens certas.\n"
))
