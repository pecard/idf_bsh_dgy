##
## Teste com dados simulados para R/curtailment_species.R
##
## Objetivo: confirmar a classificacao de curtailments por grupo de especie
## (priority/nonpriority/other/uncategorized) e os 2 niveis de sumario (por
## especie dentro do grupo, e roll-up por grupo). O ponto principal a testar
## e' o grupo "uncategorized" -- o script original (scripts_IDF/curtailments_species.R)
## descartava silenciosamente qualquer species fora dos 3 grupos conhecidos;
## aqui deve ficar visivel.
##
## Nao usa testthat -- script normal com dados sinteticos e resultado
## calculado a mao.
##
## Correr: source("tests/test_curtailment_species.R")
##
## Depende de: data.table
##

source("R/curtailment_species.R")

test_prioritysp    <- c("Steppe-Eagle", "Golden-Eagle")
test_nonprioritysp <- c("Kestrel")
test_othersp       <- c("Common-Crane")

## 8 curtailments: 3 Steppe-Eagle + 1 Golden-Eagle (priority), 2 Kestrel
## (nonpriority), 1 Common-Crane (other), 1 Mystery-Bird (fora dos 3 grupos
## -- deve cair em "uncategorized", nao desaparecer)
curtl_dt_test <- data.table(
  track_id = as.character(1:8),
  turbine  = "TESTCS1",
  species  = c(
    "Steppe-Eagle", "Steppe-Eagle", "Steppe-Eagle", "Golden-Eagle",
    "Kestrel", "Kestrel",
    "Common-Crane",
    "Mystery-Bird"
  ),
  start    = as.POSIXct("2026-04-01 00:00:00", tz = "UTC") + (0:7) * 60
)

species_curt_dt <- classify_curtailment_species(curtl_dt_test, test_prioritysp, test_nonprioritysp, test_othersp)

cat("\n===== classify_curtailment_species() =====\n")
print(species_curt_dt[, .(track_id, species, species_group)])

expected_group <- data.table(
  track_id             = as.character(1:8),
  expected_species_group = c("priority", "priority", "priority", "priority",
                              "nonpriority", "nonpriority", "other", "uncategorized")
)
check_group <- merge(species_curt_dt, expected_group, by = "track_id")
check_group[, group_ok := species_group == expected_species_group]
cat(sprintf("\nResultado: %d/%d species_group corretos.\n", sum(check_group$group_ok), nrow(check_group)))
cat(paste(
  "\nNota: 'Mystery-Bird' (track_id 8) nao esta em nenhum dos 3 grupos --",
  "fica 'uncategorized', nao desaparece (era o comportamento do script",
  "original, sem aviso).\n"
))

by_species_dt <- summarise_curtailment_species(species_curt_dt)
cat("\n===== summarise_curtailment_species() =====\n")
print(by_species_dt)
cat(paste(
  "\nEsperado (n_total=8): priority/Steppe-Eagle n=3, pct_of_group=75.0%",
  "(3/4), pct_of_total=37.5% (3/8). priority/Golden-Eagle n=1,",
  "pct_of_group=25.0%, pct_of_total=12.5%. nonpriority/Kestrel n=2,",
  "pct_of_group=100.0%, pct_of_total=25.0%. other/Common-Crane n=1,",
  "pct_of_group=100.0%, pct_of_total=12.5%. uncategorized/Mystery-Bird n=1,",
  "pct_of_group=100.0%, pct_of_total=12.5%.\n"
))

by_group_dt <- summarise_curtailment_species_group(species_curt_dt)
cat("\n===== summarise_curtailment_species_group() =====\n")
print(by_group_dt)
cat(paste(
  "\nEsperado: priority n=4 (50.0%), nonpriority n=2 (25.0%), other n=1",
  "(12.5%), uncategorized n=1 (12.5%).\n"
))
