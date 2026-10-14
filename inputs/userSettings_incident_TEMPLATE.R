##
## MODELO -- settings do INCIDENTE (qualquer parque)
##
## Copiar para inputs/userSettings_incident_<FARM>_<turbina>_<AAAAMMDD>.R,
## preencher, e apontar incident_settings_file para ele no lancador
## (modelo: run_incident_TEMPLATE.R). E' lido DEPOIS do settings do parque
## (project_settings_file, ex: userSettings_BSH.R) e sobrepoe-se-lhe: aqui
## so' vao os valores que mudam de incidente para incidente. Tudo o que
## define o PARQUE (pastas dos 4 datasets do portal IDF, padroes de nome,
## shapefiles de turbinas/IDF, DEM, farm_code, fuso, coordenadas) fica no
## settings do parque.
##
## O parque e' identificado por farm_code (+ farm_pattern); o incidente por
## farm_code + turbina + data -- e' isto que da' nome a pasta e aos
## ficheiros de output (R/output_paths.R).
##

## -- Janela de dados lida/filtrada (mais larga que a janela do incidente) --
ini <- as.POSIXct('2026-01-01 00:00:00', tz = proj_timezone)
end <- as.POSIXct('2026-01-31 23:59:59', tz = proj_timezone)
scada_ini <- ini
scada_end <- end

## -- Turbina do incidente (rotulo como em track_dt$turbine / SCADA) --
turbinas_scada <- c("T00")
wtg_3d_coverage <- c("T00")

## -- Unidades IDF de interesse: codigo BRUTO do heartbeat (valores) e
## rotulo "IDF<NN>" (names) -- ver nota em userSettings_ZRF_T94_20261001.R --
heartbeat_idf_units <- c("XX00-00")
names(heartbeat_idf_units) <- c("IDF00")

## -- Colunas de ID nos shapefiles (se diferirem do settings do parque) --
# wtg_source_id_col <- "ID"
# idf_source_id_col <- "Name"

## -- Raio do rotor (m) da turbina/modelo do incidente: velocidade na ponta
## da pa na tabela "Top Candidate Tracks". Sem ele, a coluna fica NA --
# rotor_radius_m <- 77.5

## -- Limiares (omitir para usar os do settings do parque, quando existirem) --
# track_proximity_threshold_m  <- 100
# curtailment_trigger_height_m <- 300   # m AGL; sem isto a proximidade e' so' horizontal
# id_transition_late_time_sec  <- 50
# shorttrack_min_points        <- 6
# shorttrack_eval_range        <- 300

## -- O incidente em si --
fatality_incidents <- data.table::data.table(
  incident_id   = "FARM_Mon012026",
  turbine       = "T00",
  species       = "Egyptian-Vulture",
  incident_date = as.Date("2026-01-31"),   # data do ACHADO, nao da colisao
  days_before   = 15
)
