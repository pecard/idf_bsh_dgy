##
## Teste com dados simulados para R/bio_flight_metrics.R
##
## Objetivo: confirmar os filtros de qualidade partilhados por
## flight_metrics_base() (usada nas 2 tabelas de resumo E no grafico -- ao
## contrario dos 3 scripts originais, que tinham 3 bases ligeiramente
## diferentes entre si): nº minimo de pontos do track, velocidade dentro de
## limites, altura >= 0 (excluindo NA e negativos). E confirmar que
## flight_height_qa() conta as alturas negativas ANTES desse filtro, em vez
## de as descartar sem deixar rasto.
##
## Nao usa testthat -- dados sinteticos, resultado calculado a mao.
##
## Correr: source("tests/test_bio_flight_metrics.R")
##
## Depende de: data.table
##

source("R/bio_flight_metrics.R")

test_prioritysp <- c("Steppe-Eagle", "Golden-Eagle", "Egyptian-Vulture")

## T1 -- Steppe-Eagle, 5 pontos, tudo valido -- fica todo na base
## T2 -- Golden-Eagle, so' 3 pontos (< min_track_points=4) -- EXCLUIDO
##       inteiro, apesar dos valores individuais serem plausiveis
## T3 -- Egyptian-Vulture, 5 pontos: 1 com altura negativa (-5), 1 com
##       altura NA -- ficam so' 3 linhas validas
## T4 -- Egyptian-Vulture, 4 pontos: 1 com velocidade fora dos limites (150
##       m/s) -- fica so' 3 linhas validas
## T5 -- Kestrel, nao-prioritaria -- EXCLUIDA inteira (nem entra na base)

track_dt_test <- data.table(
  track_id = c(rep("T1", 5), rep("T2", 3), rep("T3", 5), rep("T4", 4), rep("T5", 5)),
  spec = c(
    rep("Steppe-Eagle", 5),
    rep("Golden-Eagle", 3),
    rep("Egyptian-Vulture", 5),
    rep("Egyptian-Vulture", 4),
    rep("Kestrel", 5)
  ),
  speed_ms = c(
    c(8, 8, 12, 12, 10),      # T1
    c(15, 14, 16),            # T2
    c(8, 9, 7, 10, 6),        # T3
    c(8, 150, 9, 7),          # T4 -- 150 fora dos limites (1-100)
    c(5, 5, 5, 5, 5)          # T5
  ),
  height = c(
    c(90, 90, 110, 110, 100), # T1
    c(80, 85, 90),            # T2
    c(-5, 60, 70, 65, NA),    # T3 -- -5 negativa, NA em falta
    c(50, 55, 60, 65),        # T4
    c(40, 40, 40, 40, 40)     # T5
  )
)

flight_base_dt <- flight_metrics_base(track_dt_test, test_prioritysp, min_track_points = 4)

cat("\n===== flight_metrics_base() =====\n")
print(flight_base_dt[, .(track_id, spec, speed_ms, height)])

n_by_track <- flight_base_dt[, .N, by = track_id]
cat("\nLinhas validas por track (obtido):\n")
print(n_by_track)
cat(paste(
  "\nEsperado: T1=5 (tudo valido), T2=0 (so 3 pontos, < min_track_points=4,",
  "EXCLUIDO por inteiro mesmo com valores plausiveis), T3=3 (exclui a linha",
  "com altura -5 e a linha com altura NA), T4=3 (exclui a linha com",
  "velocidade 150 m/s), T5=0 (Kestrel nao e' prioritaria). T2 e T5 nao",
  "devem aparecer em nenhuma linha da tabela acima.\n"
))

speed_summary_dt <- summarise_flight_speed(flight_base_dt)
cat("\n===== summarise_flight_speed() =====\n")
print(speed_summary_dt)
cat(paste(
  "\nEsperado para Steppe-Eagle (T1, valores redondos de proposito para",
  "conferir a olho): speed=[8,8,12,12,10] -> n=5, mean=10.00, median=10,",
  "sd=2.00 (variancia=4, soma dos quadrados dos desvios=16/4), min=8,",
  "max=12. Golden-Eagle NAO aparece (0 linhas validas). Egyptian-Vulture",
  "combina T3 (velocidades 9,7,10) + T4 (velocidades 8,9,7) = 6 valores --",
  "conferir a olho contra os valores impressos em flight_metrics_base()",
  "acima.\n"
))

height_summary_dt <- summarise_flight_height(flight_base_dt)
cat("\n===== summarise_flight_height() =====\n")
print(height_summary_dt)
cat(paste(
  "\nEsperado para Steppe-Eagle: height=[90,90,110,110,100] -> n=5,",
  "mean=100.0, median=100, sd=10.0 (variancia=100), min=90, max=110.",
  "Egyptian-Vulture combina T3 (alturas 60,70,65) + T4 (50,55->EXCLUIDA",
  "junto com a velocidade 150,60,65 -- ver nota abaixo) = os 3 valores de",
  "T3 mais os 3 valores de T4 que sobreviveram ao filtro de velocidade.\n"
))

height_qa_dt <- flight_height_qa(track_dt_test, test_prioritysp, min_track_points = 4)
cat("\n===== flight_height_qa() =====\n")
print(height_qa_dt)
cat(paste(
  "\nEsperado: Egyptian-Vulture (T3+T4, ANTES do filtro height>=0, so' exclui",
  "NA e velocidade fora dos limites): T3 contribui 4 linhas (exclui so' a",
  "linha com altura NA; a de altura -5 fica incluida aqui, ao contrario de",
  "flight_metrics_base()), T4 contribui 3 linhas (exclui a de velocidade",
  "150) -> n_total=7, n_negative_height=1 (a de -5), pct_negative=14.3%,",
  "min_height_m=-5.0. Steppe-Eagle: n_total=5, n_negative_height=0. Golden-Eagle",
  "NAO aparece (so' 3 pontos, < min_track_points, mesmo regra aqui).\n"
))
