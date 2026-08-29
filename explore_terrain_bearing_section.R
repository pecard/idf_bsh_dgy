##
## Teste final, em consola, da futura secção "Terrain & Spatial Use
## Patterns" do relatorio anual (BSH/DGY) -- NAO faz parte do pipeline de
## producao (IDF_analysis.R nunca o chama, nao escreve nada em outputs/).
##
## Junta os 3 blocos construidos em 2026-08 a pedido do Paulo:
##   R/turbine_terrain_classification.R -- classe de terreno por turbina (DEM)
##   R/curtailment_bearing_sectors.R    -- distancia/velocidade/altura do
##                                          disparo de curtailment, por setor
##                                          de bussola, classe de terreno e
##                                          especie
##   R/track_terrain_temporal.R         -- padrao semanal de uso do espaco
##                                          (TODOS os tracks) por classe de
##                                          terreno
##
## So' os graficos combinados abaixo (2 da parte de curtailments/especies +
## 2 da parte temporal) sao os que se propoe levar para o relatorio -- ver
## mensagem ao Paulo para a estrutura da secção proposta antes de mexer em
## report_template.rmd/IDF_analysis.R.
##
## Pre-requisitos (correr isto DEPOIS de uma corrida normal de
## IDF_analysis.R, na mesma sessao) -- objetos ja tem de existir:
##   wtg, dem_file, curtl_dt, track_dt (CONFIRMAR o range de datas que
##   queres antes de correr -- range(track_dt$timestamp) -- se quiseres o
##   historico completo em vez do periodo do relatorio, usa track_dt_unfilt)
##
## Correr: source("explore_terrain_bearing_section.R")
##

source("R/turbine_terrain_classification.R")
source("R/curtailment_bearing_sectors.R")
source("R/track_terrain_temporal.R")


## 0. Classificacao do terreno (so' precisa de correr 1x por parque/DEM --
## reutilizar terrain_dt se ja o tiveres em memoria de uma corrida anterior) -

terrain_dt <- compute_turbine_terrain_metrics(wtg, dem_file, radius_inner_m = 250, radius_outer_m = 500)
terrain_dt <- classify_terrain(terrain_dt) # limiares por quantil, calibrados a este parque

cat("\n===== Turbinas por classe de terreno =====\n")
print(table(terrain_dt$terrain_class))

# confirmar visualmente antes de confiar na classificacao -- ajustar
# ridge_relelev_q/complex_slope_q em classify_terrain() se nao bater certo
# com o que conheces do terreno
print(plot_turbine_terrain_map(wtg, terrain_dt))


## 1. Distancia/velocidade/altura do disparo, por setor + classe de terreno +
## especie (Egyptian-Vulture, Steppe-Eagle, Saker-Falcon para ja') ----------

bearing_dt <- compute_curtailment_bearing(curtl_dt, track_dt, wtg, turbine_terrain_dt = terrain_dt)

priority_species <- c("Egyptian-Vulture", "Steppe-Eagle", "Saker-Falcon")
bearing_dt_sp <- bearing_dt[species %in% priority_species]

cat("\n===== Eventos de curtailment por especie prioritaria =====\n")
print(bearing_dt_sp[, .N, by = species])

# tabela de apoio (nao vai para o relatorio, so' para conferir os numeros
# antes de confiar nos graficos)
summary_bearing_sp <- summarise_bearing_sectors(bearing_dt_sp, group_cols = c("species", "terrain_class"))
cat("\n===== Resumo (quantis) por especie x classe de terreno =====\n")
print(summary_bearing_sp[, .(species, terrain_class, n, dist_median, speed_median, height_median)])

## Os 2 graficos propostos para o relatorio (secção "Curtailment Distance,
## Speed and Height by Terrain Class and Species") -- repetir para
## metric = "avg_speed_ms" / "trigger_height_m" conforme o que quiseres ver
p_box_species   <- plot_bearing_boxplot(bearing_dt_sp, metric = "trigger_dist_m")
p_terrain_species <- plot_terrain_class_hist(bearing_dt_sp, metric = "trigger_dist_m")

print(p_box_species)
print(p_terrain_species)

# ggsave(file.path(folder_output, "bearing_boxplot_by_species.png"), p_box_species, width = 8, height = 8, dpi = 300, bg = "white")
# ggsave(file.path(folder_output, "terrain_hist_by_species.png"), p_terrain_species, width = 9, height = 8, dpi = 300, bg = "white")


## 2. Padrao semanal de uso do espaco por classe de terreno (TODOS os tracks) -

track_terrain_dt <- assign_track_terrain_class(track_dt, wtg, terrain_dt)
weekly_dt <- summarise_tracks_by_week_terrain(track_terrain_dt, terrain_dt)

cat("\n===== weekly_dt (ultimas linhas) =====\n")
print(tail(weekly_dt))

## Os 2 graficos propostos para o relatorio (secção "Weekly Space-Use
## Pattern by Terrain Class") -- eixo X semanal e vertical ja' embutido em
## plot_tracks_by_week_terrain()
p_mean_sd <- plot_tracks_by_week_terrain(weekly_dt, metric = "mean_tracks_per_turbine")
p_active  <- plot_tracks_by_week_terrain(weekly_dt, metric = "pct_turbines_active")

print(p_mean_sd)
print(p_active)

# ggsave(file.path(folder_output, "weekly_mean_tracks_per_turbine.png"), p_mean_sd, width = 12, height = 6, dpi = 300, bg = "white")
# ggsave(file.path(folder_output, "weekly_pct_turbines_active.png"), p_active, width = 12, height = 6, dpi = 300, bg = "white")
