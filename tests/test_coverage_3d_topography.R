##
## Teste com dados simulados para R/coverage_3d_topography.R
##
## Foca-se em .build_mesh_from_terrain() -- a logica pura (data.table, sem
## DEM/CRS) que decide ate onde a malha 3D se estende para baixo (z_floor) e
## classifica cada no como "terrain"/"air" e por risk_band. E' a parte que
## mudou (extensao da malha ate a cota mais baixa do relevo, em vez de parar
## sempre em z relativo = 0), e a unica testavel sem um DEM real -- o resto de
## build_terrain_mesh()/compute_mesh_coverage() faz crop/reproject/extract
## sobre um GeoTIFF real, dificil de simular com rigor aqui.
##
## Correr: source("tests/test_coverage_3d_topography.R")
##
## Depende de: data.table
##

source("R/coverage_3d_topography.R")


##
## Cenario 1: turbina numa zona com relevo mais baixo nalgumas direcoes --
## 4 colunas (x,y) com elevacao de terreno diferente, wtg_elev = 500:
##   A (0,0)     terrain=500 (na base da propria turbina)
##   B (100,0)   terrain=450 (50m abaixo da turbina)
##   C (-100,0)  terrain=530 (30m acima da turbina)
##   D (0,100)   terrain=380 (120m abaixo da turbina -- o caso mais critico,
##               o que motivou a correcao: uma ave a voar a 20m do solo aqui
##               esta a z_rel_turbine = -100, fora da malha antiga)
##
mesh_xy_1 <- data.table(
  x = c(0, 100, -100, 0),
  y = c(0, 0, 0, 100),
  terrain_elev = c(500, 450, 530, 380)
)
wtg_elev_1 <- 500
cyl_height <- 1000
step_z <- 50

built_1 <- .build_mesh_from_terrain(mesh_xy_1, wtg_elev_1, cyl_height, step_z)

## z_floor esperado: min(0, floor((min(terrain_elev) - wtg_elev)/step_z)*step_z)
##   = min(0, floor((380-500)/50)*50) = min(0, floor(-2.4)*50) = min(0, -150) = -150
cat("\n===== z_floor (cenario 1: relevo com zonas abaixo da turbina) =====\n")
cat(sprintf("Obtido: %.0f | Esperado: -150 -- %s\n",
           built_1$z_floor, if (built_1$z_floor == -150) "OK" else "FALHOU"))

## zs = seq(-150, 1000, by=50) -> 24 niveis * 4 colunas = 96 nos
cat(sprintf("nrow(mesh): %d | Esperado: 96 -- %s\n",
           nrow(built_1$mesh), if (nrow(built_1$mesh) == 96) "OK" else "FALHOU"))

## Spot-checks de medium ("terrain"/"air") e risk_band, calculados a mao:
## clearance = (wtg_elev + z_rel_turbine) - terrain_elev(coluna); "terrain" se <= 0
checks_1 <- data.table(
  x = c(0, 0, 0, 100, 100, -100, -100, -100, 0, 0, 0, 0),
  y = c(0, 0, 0, 0, 0, 0, 0, 0, 100, 100, 100, 0),
  z_rel_turbine = c(-150, 0, 50, -50, 0, 0, 50, 100, -150, -100, -50, 250),
  expected_medium = c("terrain", "terrain", "air", "terrain", "air", "terrain", "air", "air",
                      "terrain", "air", "air", "air"),
  expected_risk_band = c("at risk", "at risk", "at risk", "at risk", "at risk", "at risk",
                         "at risk", "at risk", "at risk", "at risk", "at risk", "above")
)
## nota: todos os pontos < 200m => "at risk"; o ultimo (z_rel=250, coluna A) testa
## que a banda "above" continua correta depois de baixarmos o limite inferior do cut()

check_1 <- merge(built_1$mesh, checks_1, by = c("x", "y", "z_rel_turbine"))
check_1[, `:=`(
  medium_ok    = medium == expected_medium,
  risk_band_ok = as.character(risk_band) == expected_risk_band
)]

cat("\n===== Spot-checks medium/risk_band (cenario 1) =====\n")
print(check_1[, .(x, y, z_rel_turbine, medium, expected_medium, medium_ok,
                  risk_band, expected_risk_band, risk_band_ok)])

n_ok_1 <- sum(check_1$medium_ok & check_1$risk_band_ok)
cat(sprintf("\nResultado: %d/%d casos corretos (%d esperados, incl. o caso critico D em z=-100: 'air').\n",
           n_ok_1, nrow(check_1), nrow(checks_1)))

## O caso central desta correcao: o no (0,100,-100) tem de existir e ser "air"
## -- na malha antiga (zs a comecar sempre em 0) este no simplesmente nao existia
d_air_node <- built_1$mesh[x == 0 & y == 100 & z_rel_turbine == -100]
cat(sprintf(
  "\nNo critico (0,100,z_rel=-100): existe=%s, medium=%s (esperado: existe=TRUE, medium='air')\n",
  nrow(d_air_node) == 1, if (nrow(d_air_node) == 1) d_air_node$medium else NA
))


##
## Cenario 2: turbina em zona plana/elevada (todo o terreno >= wtg_elev) --
## comportamento deve ficar EXATAMENTE como antes da correcao (z_floor = 0)
##
mesh_xy_2 <- data.table(
  x = c(0, 100, -100),
  y = c(0, 0, 0),
  terrain_elev = c(500, 510, 600)
)
built_2 <- .build_mesh_from_terrain(mesh_xy_2, wtg_elev_1, cyl_height, step_z)

cat("\n===== z_floor (cenario 2: sem relevo abaixo da turbina -- compatibilidade) =====\n")
cat(sprintf("Obtido: %.0f | Esperado: 0 -- %s\n",
           built_2$z_floor, if (built_2$z_floor == 0) "OK" else "FALHOU"))
cat(sprintf("min(z_rel_turbine) na malha: %.0f | Esperado: 0 -- %s\n",
           min(built_2$mesh$z_rel_turbine), if (min(built_2$mesh$z_rel_turbine) == 0) "OK" else "FALHOU"))


##
## low_sample: formula usada em compute_mesh_coverage() (n_records < min_sample_records)
## -- so a formula em si, nao testavel end-to-end aqui sem um DEM real (ver nota no topo)
##
cat("\n===== low_sample (so a formula -- compute_mesh_coverage() precisa de DEM real) =====\n")
cat(sprintf("500000 registos, threshold 500000 -> low_sample = %s (esperado FALSE)\n", 500000 < 500000))
cat(sprintf("499999 registos, threshold 500000 -> low_sample = %s (esperado TRUE)\n", 499999 < 500000))
