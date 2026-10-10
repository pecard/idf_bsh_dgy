##
## Settings do regional assessment (ZRF + BSH + DGY) -- run_regional_assessment.R
##
## Ficheiro SEPARADO dos settings de incidente/anual/mensal (userSettings_ZRF*.R,
## userSettings_BSH.R, ...) de proposito: so' tem o que e' preciso para LER os
## tracks dos 3 parques, a janela temporal, as 2 especies, e os criterios de
## bin/separacao entre tracks. Nao le curtailments, SCADA nem heartbeats.
## Os parametros da analise (epocas, lags, pulsos, esforco de monitorizacao)
## ficam no bloco "PARAMETROS DA ANALISE" de run_regional_assessment.R.
##
## Requer data.table carregado (o script faz library(data.table) antes).
##


##
## Fuso horario
##
## Mesmo fuso nos 3 parques (Uzbequistao, sem DST). Todas as datas/dias do
## regional assessment usam este fuso, nunca UTC.
##

proj_timezone <- "Asia/Samarkand"


##
## Raw databases (so' tracks)
##
## databases_dir e' a pasta partilhada "data-raw" (mesma dos outros projetos);
## databases_dir_alt e' a pasta de rede com os mesmos ficheiros, por parque
## (se o mesmo nome existir nas 2 pastas, fica o da 1a, databases_dir -- ver
## list_files_multi_dir(), R/read_utils.R). NA = sem pasta alternativa.
##
## TODO(Paulo): confirmar a pasta de rede de ZRF (ex: .../IDF_PortalData/ZRF)
##

databases_dir <- "G:/O meu disco/Programacao/r/Bsh_Dgy_WPP/data-raw"

## Nomes dos ficheiros consistentes nos 3 parques: "TrackReport_Default" +
## codigo do parque (ex: TrackReport_Default_ZRF_20260901_....csv).
## ATENCAO: o padrao e' uma expressao regular -- "TrackReport_Default+ZRF"
## NAO serve (o "+" aplica-se ao "t" anterior); ".*" e' o "qualquer coisa
## entre os dois" certo. O padrao final de cada parque aparece na coluna
## trackreport_pattern de regional_farms (e e' escrito na consola ao ler).
trackreport_prefix <- "TrackReport_Default"

## farm = codigo do parque (ordem: norte, oeste, este); farm_pattern = 2a
## camada de filtro por substring no nome do ficheiro (igual ao codigo);
## cache propria do regional (regional_cache_dir) -- nao mistura com as caches
## dos outros relatorios, que usam outros padroes de ficheiro.
regional_farms <- data.table::data.table(
  farm                = c("ZRF", "BSH", "DGY"),
  databases_dir_alt   = c(NA_character_,
                          "//192.168.1.11/DadosBrutos(T2)/Lisboa/08_Tecnica/2025/T05-2025_BSH_DGY/IDF_PortalData/BSH",
                          "//192.168.1.11/DadosBrutos(T2)/Lisboa/08_Tecnica/2025/T05-2025_BSH_DGY/IDF_PortalData/DGY")
)
regional_farms[, `:=`(
  trackreport_pattern = paste0(trackreport_prefix, ".*", farm),
  farm_pattern        = farm
)]

regional_cache_dir <- file.path("cache", "regional")


##
## Timeframe for analysis/reporting period
##
## Janela unica para os 3 parques (a comparacao so' faz sentido na mesma
## janela). Tracks fora de ini/end sao descartados depois de lidos (a cache
## guarda sempre todos os tracks lidos -- mudar ini/end nao exige reler).
## Fora do periodo com dados de cada parque, as series ficam NA (nao 0).
##

ini <- as.POSIXct('2025-01-01 00:00:00', tz = proj_timezone)
end <- as.POSIXct('2026-10-01 23:59:59', tz = proj_timezone)


##
## Especies
##
## So' estas 2 -- movimentos migratorios pos-nupciais (outono, para sul) e
## pre-nupciais (primavera, para norte).
##

species_regional <- c("Egyptian-Vulture", "Steppe-Eagle")


##
## Bins e criterios de separacao entre tracks (evitar contar o mesmo individuo
## varias vezes) -- ver R/track_min_individuals.R
##
## Separacao TEMPORAL: os pontos de track sao postos em bins de bin_min
## minutos; so' tracks no MESMO bin podem ser o mesmo individuo.
## Separacao ESPACIAL: dentro de um bin, 2 tracks (mesmo de unidades IDF
## diferentes) contam como UM individuo se a distancia minima ponto-a-ponto
## entre eles for < merge_dist_m (cadeias de proximidade A-B-C colapsam para 1).
## O numero de individuos do bin e' por isso um minimo (lower bound).
## Mesmos valores dos relatorios anual/mensal/incidente.
##

bin_min <- 2          # minutos
merge_dist_m <- 200   # metros
