##
## Curtailments por grupo de especie (prioritaria/nao-prioritaria/outra) ----
##
## Migrado de scripts_IDF/curtailments_species.R.
##
## Usa curtl_dt$species -- a especie logada NO PROPRIO curtailment (a
## classificacao NESSE momento), nao a sequencia de classificacoes do track
## inteiro. Para saber se essa classificacao mudou a meio do track (e por
## isso o curtailment pode ter sido desnecessario ou tardio), ver
## R/id_transitions.R (secção 3.2) -- sao duas perguntas diferentes:
##   3.2 -- "esta classificacao mudou a meio do track?"
##   3.3 -- "que especies estao, no total, a disparar curtailments?"
##
## Cada curtailment e' classificado num dos 3 grupos definidos no
## userSettings (prioritysp/nonprioritysp/othersp). O script original
## calculava 3 tabelas separadas com filter(species %in% grupo) -- qualquer
## curtailment cujo species NAO estivesse em NENHUM dos 3 grupos (NA, ou uma
## especie nova ainda nao categorizada) ficava silenciosamente EXCLUIDO das
## 3 tabelas, sem aviso. Aqui fica visivel como grupo "uncategorized", para
## nao perder cobertura de forma invisivel -- se este grupo aparecer com
## contagem > 0, e' sinal de espécie nova em curtl_dt$species ainda por
## adicionar a prioritysp/nonprioritysp/othersp no userSettings.
##
## Depende de: data.table
##
## Uso:
##   source("R/curtailment_species.R")
##
##   species_curt_dt <- classify_curtailment_species(curtl_dt, prioritysp, nonprioritysp, othersp)
##   by_species_dt <- summarise_curtailment_species(species_curt_dt)
##   by_group_dt   <- summarise_curtailment_species_group(species_curt_dt)
##

classify_curtailment_species <- function(curtl_dt, prioritysp, nonprioritysp, othersp) {

  out <- data.table::copy(curtl_dt)
  out[, species_group := data.table::fcase(
    species %in% prioritysp,    "priority",
    species %in% nonprioritysp, "nonpriority",
    species %in% othersp,       "other",
    default = "uncategorized"
  )]
  out[]
}


## Contagem por especie, com % DENTRO do proprio grupo (como no script
## original -- ex: "% dos curtailments de nao-prioritarias que sao Kestrel")
## e % sobre o TOTAL de curtailments da quinta (novo -- a % de grupo sozinha
## pode enganar: "50% dos curtailments nao-prioritarios sao Kestrel" nao diz
## nada sobre quanto o Kestrel pesa no total da quinta)
summarise_curtailment_species <- function(species_curt_dt) {

  if (nrow(species_curt_dt) == 0L) {
    return(data.table::data.table(
      species_group = character(), species = character(), n = integer(),
      pct_of_group = numeric(), pct_of_total = numeric()
    ))
  }

  n_total <- nrow(species_curt_dt)

  out <- species_curt_dt[, .(n = .N), by = .(species_group, species)]
  out[, pct_of_group := round(100 * n / sum(n), 1), by = species_group]
  out[, pct_of_total := round(100 * n / n_total, 1)]
  data.table::setorder(out, species_group, -n)
  out[]
}


## Roll-up por grupo -- panorama rapido: que fraccao dos curtailments da
## quinta e' de especies prioritarias vs nao-prioritarias vs outras vs
## ainda por categorizar
summarise_curtailment_species_group <- function(species_curt_dt) {

  if (nrow(species_curt_dt) == 0L) {
    return(data.table::data.table(species_group = character(), n = integer(), pct_of_total = numeric()))
  }

  n_total <- nrow(species_curt_dt)
  out <- species_curt_dt[, .(n = .N), by = species_group]
  out[, pct_of_total := round(100 * n / n_total, 1)]
  data.table::setorder(out, -n)
  out[]
}
