##
## Teste com dados simulados para R/id_transitions.R
##
## Objetivo: confirmar a contagem de especies distintas por track_id
## (riqueza/histograma + entropia de Shannon), e a classificacao de risco
## de transicao P<->NP entre tracks multi-ID:
##   - P->NP ("P_to_NP_unnecessary_curtailment"): curtailment disparado, mas
##     a especie final do track e' nao-prioritaria -- custo de producao.
##   - NP->P ("NP_to_P_protection_gap"): NENHUM curtailment disparado, mas a
##     especie final do track e' prioritaria -- risco biologico (a direcao
##     nova, pedida pelo Paulo, que o script original scripts_IDF/ nao
##     cobria).
##   - "no_risk": ou o track ficou estavel (1 unica especie, nem entra nesta
##     tabela), ou a decisao tomada (curtailment / nao-curtailment) acabou a
##     bater certo com a especie final.
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

##
## Tracks sinteticos (ja em ordem cronologica -- track_species_summary()
## assume track_dt ordenado por track_id, timestamp, tal como o pipeline
## real garante em R/read_tracks.R)
##

track_dt <- data.table(
  track_id = c(
    rep("T1", 5), # estavel, 1 especie prioritaria -- sem risco (nem entra na tabela multi-ID)
    rep("T2", 3), # NP -> P, MAS curtailment disparado -- decisao correta, no_risk
    rep("T3", 4), # P -> NP, curtailment disparado -- custo de producao
    rep("T4", 4), # NP -> P, SEM curtailment -- risco biologico (o caso novo)
    rep("T5", 4), # NP -> NP (troca entre 2 nao-prioritarias), sem curtailment -- no_risk
    rep("T6", 3)  # estavel, 1 especie prioritaria, com curtailment -- sem risco (nem entra na tabela multi-ID)
  ),
  spec = c(
    rep("Steppe-Eagle", 5),
    "Kestrel", "Steppe-Eagle", "Steppe-Eagle",
    "Steppe-Eagle", "Steppe-Eagle", "Kestrel", "Kestrel",
    "Kestrel", "Kestrel", "Steppe-Eagle", "Steppe-Eagle",
    "Kestrel", "Kestrel", "Common-Buzzard", "Common-Buzzard",
    rep("Golden-Eagle", 3)
  )
)

curtl_dt <- data.table(
  track_id = c("T2", "T3", "T3", "T6"),
  turbine  = c("TESTID1", "TESTID2", "TESTID2", "TESTID3"),
  start    = as.POSIXct("2026-04-01 00:00:00", tz = "UTC") + c(0, 100, 200, 300)
)


##
## PARTE A -- riqueza de especies por track (histograma) + entropia
##

richness_dt <- track_species_summary(track_dt)

cat("\n===== PARTE A: track_species_summary() =====\n")
print(richness_dt[order(track_id)])

expected_n_species <- data.table(
  track_id            = c("T1", "T2", "T3", "T4", "T5", "T6"),
  expected_n_species  = c(1, 2, 2, 2, 2, 1),
  expected_last_spec  = c("Steppe-Eagle", "Steppe-Eagle", "Kestrel", "Steppe-Eagle", "Common-Buzzard", "Golden-Eagle")
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

cat(paste(
  "\nNota sobre entropia: T1/T6 tem H=0 (1 unica especie -> log(1)=0, sem",
  "alternancia). T3/T4/T5 tem split 50/50 entre as 2 especies (2 registos",
  "cada) -> H = -ln(0.5) = 0.6931. T2 tem split 1/3 Kestrel, 2/3",
  "Steppe-Eagle -> H = -(1/3*ln(1/3) + 2/3*ln(2/3)) = 0.6365 (mais baixo",
  "que 0.6931 porque a distribuicao e' menos equilibrada). Confirma visualmente",
  "os valores impressos acima contra estas contas.\n"
))

richness_summary <- summarise_species_richness(richness_dt)
cat("\n----- summarise_species_richness()$by_n_species -----\n")
print(richness_summary$by_n_species)
cat("\n----- summarise_species_richness()$rate -----\n")
print(richness_summary$rate)
cat(paste(
  "\nEsperado: total_tracks=6, tracks_with_transition=4 (T2,T3,T4,T5),",
  "id_transition_rate_pct=66.7%.\n"
))
cat("\n----- summarise_species_richness()$entropy -----\n")
print(richness_summary$entropy)


##
## PARTE B -- classificacao de risco P<->NP (tracks multi-ID)
##

risk_dt <- classify_id_transition_risk(richness_dt, curtl_dt, test_prioritysp)

cat("\n\n===== PARTE B: classify_id_transition_risk() =====\n")
print(risk_dt[order(track_id), .(track_id, species, triggered_curtailment, last_is_priority, risk_direction)])

expected_risk <- data.table(
  track_id                 = c("T2", "T3", "T4", "T5"),
  expected_risk_direction  = c("no_risk", "P_to_NP_unnecessary_curtailment", "NP_to_P_protection_gap", "no_risk")
)
check_b <- merge(risk_dt, expected_risk, by = "track_id")
check_b[, risk_ok := risk_direction == expected_risk_direction]

cat("\nCheck risk_direction (esperado vs obtido):\n")
print(check_b[, .(track_id, risk_direction, expected_risk_direction, risk_ok)])
cat(sprintf("\nResultado: %d/%d casos corretos.\n", sum(check_b$risk_ok), nrow(check_b)))
cat(paste(
  "\nNota: T1 e T6 (1 unica especie) NAO aparecem em risk_dt -- nunca mudaram",
  "de classificacao, por isso nao ha risco de transicao a avaliar, so' os 4",
  "tracks multi-ID (T2-T5) sao classificados.\n",
  "T2 ilustra o caso 'bom': o track chegou a ser visto como nao-prioritario",
  "(Kestrel) mas o curtailment SO disparou (e ficou) quando a classificacao",
  "final e' prioritaria -- decisao correta, no_risk (nao e' o mesmo que",
  "'NP->P sem curtailment', porque aqui HOUVE curtailment).\n"
))


##
## PARTE C -- sumarios para relatorio
##

risk_summary <- summarise_id_transition_risk(risk_dt, curtl_dt)

cat("\n\n===== PARTE C: summarise_id_transition_risk() =====\n")
cat("\n----- by_direction -----\n")
print(risk_summary$by_direction)
cat(paste(
  "\nEsperado: no_risk=2 (50.0%), P_to_NP_unnecessary_curtailment=1 (25.0%),",
  "NP_to_P_protection_gap=1 (25.0%), sobre n_multi_id_tracks=4.\n"
))

cat("\n----- pnp_curtailments -----\n")
print(risk_summary$pnp_curtailments)
cat(paste(
  "\nEsperado: total_curtailments=4 (T2:1, T3:2, T6:1). De entre esses,",
  "curtailments_from_multi_id_tracks=3 (T2:1 + T3:2 -- T6 e' single-ID, nao",
  "conta; T4/T5 nunca dispararam curtailment). curtailments_due_to_p_to_np=2",
  "(as 2 curtailments de T3, o unico track P->NP) -- pct_of_total = 2/4 =",
  "50.0%. Isto conta EVENTOS de curtailment, nao tracks -- um track P->NP",
  "com varios curtailments pesa mais aqui do que na tabela by_direction",
  "(que conta tracks).\n"
))

cat(paste(
  "\nImportante para leitura do relatorio: 'curtailments_due_to_p_to_np'",
  "quantifica o CUSTO DE PRODUCAO (paragens desnecessarias). O caso",
  "NP_to_P_protection_gap NAO tem equivalente em curtailments -- por",
  "definicao esses tracks tiveram ZERO curtailments (e' precisamente o",
  "problema) -- o que se reporta la e' a CONTAGEM DE TRACKS em risco",
  "(by_direction), nao um numero de eventos evitados.\n"
))
