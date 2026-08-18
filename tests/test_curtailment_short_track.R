##
## Teste com dados simulados para R/curtailment_short_track.R
##
## Objetivo: confirmar a classificacao de curtailments de short-tracks perto
## da turbina (short_track_close), com foco nas 2 correcoes feitas ao portar
## do script original:
##   - x2d recalculado localmente (distancia 2D no 1º registo do track), em
##     vez de depender de uma coluna que ja nao existe em curtl_dt
##   - n_points recalculado a partir do track_dt PASSADO A FUNCAO (reflete
##     qualquer filtro de datas ja aplicado a montante), em vez de reutilizar
##     track_dt$count (que pode estar desatualizado depois de um filtro)
##
## Nao usa testthat -- dados sinteticos, resultado calculado a mao.
##
## Correr: source("tests/test_curtailment_short_track.R")
##
## Depende de: data.table
##

source("R/curtailment_short_track.R")

test_prioritysp <- c("Steppe-Eagle", "Golden-Eagle")
test_min_points <- 6
test_eval_range_m <- 300

t0 <- function(base_min) as.POSIXct("2026-04-01 00:00:00", tz = "UTC") + base_min * 60

## T1 -- curto (3 pts), perto (x2d=250m), com curtailment -- short_track_close = TRUE
## T2 -- curto (3 pts), longe (x2d=500m), com curtailment -- FALSE (longe)
## T3 -- longo (10 pts), perto (x2d=100m), com curtailment -- FALSE (nao e' curto)
## T4 -- curto (2 pts), perto (x2d=50m), Steppe-Eagle, com curtailment -- TRUE
## T5 -- curto (4 pts), perto (x2d=80m), Kestrel (nao-prioritaria), com curtailment -- TRUE
##       (conta no total, mas fica fora do breakdown por especie prioritaria)
## T6 -- curto (5 pts), perto, SEM curtailment -- so' conta no total farm-wide
##       de short-tracks, nao aparece em short_track_dt (nao ha curtailment)

track_dt_test <- data.table(
  track_id = c(
    rep("T1", 3), rep("T2", 3), rep("T3", 10), rep("T4", 2), rep("T5", 4), rep("T6", 5)
  ),
  dist = c(
    c(250, 200, 150),
    c(500, 450, 400),
    seq(100, 460, by = 40),
    c(50, 40),
    c(80, 70, 60, 50),
    c(90, 80, 70, 60, 50)
  ),
  timestamp = c(
    t0(0)  + c(0, 10, 20),
    t0(10) + c(0, 10, 20),
    t0(20) + seq(0, 90, by = 10),
    t0(40) + c(0, 10),
    t0(50) + c(0, 10, 20, 30),
    t0(60) + c(0, 10, 20, 30, 40)
  )
)

curtl_dt_test <- data.table(
  track_id = c("T1", "T2", "T3", "T4", "T5"),
  turbine  = "TESTST1",
  species  = c("Golden-Eagle", "Golden-Eagle", "Kestrel", "Steppe-Eagle", "Kestrel"),
  start    = c(t0(0) + 25, t0(10) + 25, t0(20) + 95, t0(40) + 15, t0(50) + 35)
)


short_track_dt <- classify_short_track_curtailments(
  track_dt_test, curtl_dt_test, min_points = test_min_points, eval_range_m = test_eval_range_m
)

cat("\n===== classify_short_track_curtailments() =====\n")
print(short_track_dt[, .(track_id, species, n_points, x2d, is_short_track, is_close, short_track_close)])

expected <- data.table(
  track_id                  = c("T1", "T2", "T3", "T4", "T5"),
  expected_short_track_close = c(TRUE, FALSE, FALSE, TRUE, TRUE)
)
check <- merge(short_track_dt, expected, by = "track_id")
check[, ok := short_track_close == expected_short_track_close]
cat(sprintf("\nResultado: %d/%d short_track_close corretos.\n", sum(check$ok), nrow(check)))
cat(paste(
  "\nNota: T2 falha por DISTANCIA (500m > 300m, apesar de curto). T3 falha",
  "por NUMERO DE PONTOS (10 >= 6, apesar de perto). So T1/T4/T5 cumprem os",
  "2 criterios em simultaneo.\n"
))

short_track_summary <- summarise_short_track_curtailments(track_dt_test, short_track_dt, test_min_points)
cat("\n===== summarise_short_track_curtailments() =====\n")
print(short_track_summary)
cat(paste(
  "\nEsperado: Total short tracks (farm-wide) = 5 (T1,T2,T4,T5,T6 tem <6",
  "pontos; T3 tem 10, nao conta). Short-track curtailments (close range) =",
  "3 (T1,T4,T5). Percentagem = 3/5 curtailments totais = 60.0% (n_total_curt",
  "= nrow(curtl_dt_test) = 5, T6 nao tem curtailment por isso nao entra",
  "nesse denominador).\n"
))

short_track_by_species <- summarise_short_track_by_species(short_track_dt, test_prioritysp)
cat("\n===== summarise_short_track_by_species() =====\n")
print(short_track_by_species)
cat(paste(
  "\nEsperado: Golden-Eagle n=1 (50.0%, de T1), Steppe-Eagle n=1 (50.0%, de",
  "T4). T5 (Kestrel) fica de fora -- nao e' especie prioritaria, apesar de",
  "short_track_close=TRUE.\n"
))
