## ----------- HEADER -----------
##**********************************************
##
##   Identiflight (IDF) data analysis -- MONTHLY REPORT
##
##**********************************************
##
## Reusa os mesmos modulos R/*.R de IDF_analysis.R, mas restringe a analise
## a 1 mes de calendario (report_month, abaixo) e a um subconjunto de
## seccoes -- ver inputs/monthlyReportSettings.R para a lista completa e a
## justificacao de cada exclusao (fatality investigation, coverage 3D,
## curtailment removal risk, turbine clustering, etc. ficam fora deste
## relatorio).
##
## Le os MESMOS 4 datasets em cache (cache/*.fst) que IDF_analysis.R --
## correr IDF_analysis.R pelo menos 1 vez antes (ou definir
## force_reread_cache_monthly = TRUE abaixo depois de descarregar dados
## novos) para a cache existir.
##
## Property of Bioinsight, Lda.
## Reproduction or sharing of any part of this
## script is prohibited without explicit permission.
##
##**********************************************


#coment this line for debug!!!
options(error = function() message("Skipping failed step"))

##
## PACKAGES ----
##

## Subconjunto do IDF_analysis.R -- este script nao usa terra/RANN/plotly/
## cluster (coverage 3D e turbine clustering ficam fora do relatorio
## mensal); sf e' usado so' para localizar as turbinas no plot espacial de
## disponibilidade (secção 1). NAO usa officedown/officer -- uma tentativa
## anterior de tornar a tabela 7.1 landscape via officedown::block_section
## saiu com um bug (aplicava landscape a TODO o conteudo anterior, nao so' a
## 7.1); revertido para rmarkdown::word_document simples, documento inteiro
## em A4 vertical, tabelas largas resolvidas por colunas mais estreitas em
## vez de mudar a orientacao da pagina (ver report/monthly_report_template.rmd).
packages <- c('purrr', 'rstudioapi',
              'tidyverse', 'lubridate', 'ggplot2',
              'scales', 'readxl', 'janitor', 'sf',
              'flextable', 'systemfonts',
              'openxlsx', 'writexl', 'rmarkdown',
              'data.table', 'suncalc',
              'fst')

for (p in packages) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}


##
## SETTINGS ----
##

## report_month: mes a processar (formato "YYYY-MM"). Editar o valor por
## omissao abaixo antes de correr (Source) -- OU definir report_month antes
## de dar source a este ficheiro (consola, ou run_monthly_report.R na raiz
## do projeto), o que tem sempre prioridade sobre o valor por omissao.
##
## NUNCA colocar aqui "source(\"IDF_monthly_report.R\")" -- isso faz este
## ficheiro chamar-se A SI PROPRIO a meio da sua propria execucao, o que
## recursa infinitamente ate um erro confuso tipo "node stack overflow"
## (ja aconteceu 2x, 2026-08). Quem quer que tenha invocado este ficheiro
## (consola, botao "Source" do RStudio, run_monthly_report.R, etc.) ja
## continua sozinho para as linhas seguintes depois desta -- nunca e'
## preciso nem seguro dar source() a este ficheiro a partir de dentro dele
## proprio.
if (!exists("report_month")) {
  report_month <- "2026-07"  ## <<< editar aqui o mes a processar, ex: "2026-08"
}

## rstudioapi::getActiveDocumentContext()$path so' resolve de forma fiavel
## quando o codigo corre via "Source" ou um source("...") explicito escrito
## na consola -- selecionar TODO o ficheiro (Ctrl+A) e correr como bloco
## envia o texto para a consola sem passar por source(), e o path devolvido
## pode vir vazio. dirname("") ainda daria "." (inofensivo), mas nalguns
## casos o path vem mesmo NULL/vazio de outra forma e faz o setwd() abaixo
## falhar com "cannot change working directory" -- como esse erro acontece
## ja dentro do source() que carrega este ficheiro, e options(error=...)
## acima nao intercepta localmente, um erro aqui aborta o resto do script
## INTEIRO (nao so' esta linha) -- por isso o try/aviso, em vez de deixar
## falhar: correr via source("IDF_monthly_report.R") escrito na consola
## continua a ser a forma recomendada (ver comentario acima), mas um Ctrl+A
## acidental agora so' avisa e continua com a working directory atual, em
## vez de abortar tudo silenciosamente.
tryCatch({
  active_doc_path <- rstudioapi::getActiveDocumentContext()$path
  if (isTRUE(nzchar(active_doc_path))) setwd(dirname(active_doc_path))
}, error = function(e) {
  message(sprintf(
    "Aviso: nao foi possivel mudar para a pasta do script via rstudioapi (%s) -- a usar a working directory atual: %s",
    conditionMessage(e), getwd()
  ))
})

folder_input  <- "inputs"
folder_output <- file.path("outputs", "monthly", report_month)

dir.create(folder_output, showWarnings = FALSE, recursive = TRUE)
dir.create(folder_input,  showWarnings = FALSE, recursive = TRUE)

folder_script <- "scripts\\"
Rfiles <- list.files(folder_script, pattern = '.R', full.names = TRUE)
lapply(Rfiles, function(x) source(x))

source(file.path(folder_input, "monthlyReportSettings.R"))


##
## QUALITY STANDARDS ----
##

username <- check_username()

## Versao do codigo que gerou este relatorio especifico -- rastreabilidade:
## se num mes seguinte ajustarmos conteudo/formula, isto identifica
## exatamente que commit (e se havia alteracoes locais nao commitadas)
## produziu CADA relatorio ja emitido. "unknown" so' se este nao for um
## checkout git ou o git nao estiver disponivel (ex: maquina sem git
## instalado) -- nunca aborta o relatorio por causa disto.
get_code_version <- function() {
  run_git <- function(args) {
    tryCatch(system2("git", args, stdout = TRUE, stderr = FALSE), error = function(e) character(0))
  }
  # --format=%h (%ci) num so' argumento (espaco + parenteses) e' interpretado
  # pela shell como 2 argumentos separados em alguns sistemas (ex: Windows),
  # partindo o argumento a meio e devolvendo status 128 do git -- 2 chamadas
  # com formatos de 1 so' token (sem espacos/parenteses) evita o problema
  hash <- run_git(c("log", "-1", "--format=%h"))
  if (length(hash) == 0L || !nzchar(hash[1])) {
    return("unknown (not a git checkout, or git unavailable)")
  }
  commit_date <- run_git(c("log", "-1", "--format=%ci"))
  dirty <- length(run_git(c("status", "--porcelain"))) > 0L
  out <- if (length(commit_date) > 0L && nzchar(commit_date[1])) {
    sprintf("%s (%s)", hash[1], commit_date[1])
  } else {
    hash[1]
  }
  if (dirty) paste0(out, " + uncommitted local changes") else out
}
code_version <- get_code_version()

sink(file.path(folder_output, "R_analysis_info.txt"))
cat(paste0('Analysis technician: ', username, '\n'))
cat(paste0('Analysis date: ', format(Sys.time(), "%Y-%m-%d"), '\n'))
cat(paste0('Report month: ', report_month, '\n'))
cat(paste0('Code version: ', code_version, '\n'))
sink()


##
## ANALYSIS ----
##

##
## 0. Import data (le a MESMA cache que IDF_analysis.R) ----
##

source("R/write_utils.R")
source("R/read_utils.R")
source("R/read_tracks.R")
source("R/read_curtailments.R")
source("R/read_scada.R")
source("R/read_heartbeats.R")
source("R/data_cache.R")

databases_dirs <- unique(c(databases_dir, if (exists("databases_dir_alt")) databases_dir_alt))

folder_cache <- "cache"

## force_reread_cache_monthly: FALSE (por omissao) reutiliza a cache
## existente sem reler os ficheiros brutos -- rapido, mas NAO deteta
## ficheiros novos (ex: SCADA de turbinas recem-descarregadas) sozinho,
## porque so' olha para o cache_file existir ou nao (ver
## load_or_read_cache(), R/data_cache.R), nunca para o conteudo da pasta de
## dados. Definir TRUE (aqui, na consola, ou em run_monthly_report.R --
## tem sempre prioridade sobre este valor por omissao) so' na 1a corrida
## depois de descarregar dados novos, para forcar a releitura e regravar a
## cache com o conteudo atualizado -- a cache e' partilhada com
## IDF_analysis.R, por isso essa releitura tambem beneficia esse script.
if (!exists("force_reread_cache_monthly")) force_reread_cache_monthly <- FALSE

## reuse_or_load_cache() (R/data_cache.R): se estes objetos ja estiverem em
## memoria de uma corrida anterior NA MESMA sessao R (ex: gerar o relatorio
## para varios meses seguidos sem reiniciar o R), reutiliza-os sem tocar no
## disco -- nao dependem do report_month, so' a filtragem por mes (abaixo)
## e' que muda a cada corrida. force_reread_cache_monthly = TRUE ignora
## sempre a memoria e vai ao disco/ficheiros brutos.
track_dt_unfilt <- reuse_or_load_cache(
  if (exists("track_dt_unfilt")) track_dt_unfilt else NULL,
  "track_dt_unfilt", file.path(folder_cache, "track_dt_unfilt.fst"),
  function() read_tracks_data(databases_dirs, trackreport_pattern, tz = proj_timezone),
  force_reread = force_reread_cache_monthly, tz = proj_timezone
)
curtl_dt_unfilt <- reuse_or_load_cache(
  if (exists("curtl_dt_unfilt")) curtl_dt_unfilt else NULL,
  "curtl_dt_unfilt", file.path(folder_cache, "curtl_dt_unfilt.fst"),
  function() read_curtailments_data(databases_dirs, curtailments_pattern, tz = proj_timezone),
  force_reread = force_reread_cache_monthly, tz = proj_timezone
)
scada_dt_unfilt <- reuse_or_load_cache(
  if (exists("scada_dt_unfilt")) scada_dt_unfilt else NULL,
  "scada_dt_unfilt", file.path(folder_cache, "scada_dt_unfilt.fst"),
  function() read_scada_data(databases_dirs, scada_pattern, tz = proj_timezone),
  force_reread = force_reread_cache_monthly, tz = proj_timezone
)
heartb_dt_unfilt <- reuse_or_load_cache(
  if (exists("heartb_dt_unfilt")) heartb_dt_unfilt else NULL,
  "heartb_dt_unfilt", file.path(folder_cache, "heartb_dt_unfilt.fst"),
  function() read_heartbeats_data(databases_dirs, heartbeats_pattern, tz = proj_timezone),
  force_reread = force_reread_cache_monthly, tz = proj_timezone
)

## Localizacao das turbinas + matriz manual turbina<->IDF -- so' para o plot
## espacial de disponibilidade (secção 1); nenhuma outra seccao do relatorio
## mensal precisa do shapefile ou da matriz
wtg <- sf::read_sf(file.path(folder_input, wtg_filename))
wtg <- sf::st_transform(wtg, crs_projection_plannar)

## IDs de turbina no shapefile (InternalNa) nao tem zero a esquerda para 1-9
## ("BSH1".."BSH9"), enquanto a matriz manual turbina<->IDF (e todos os
## outros datasets do projeto -- curtailments, SCADA, tracks) usam sempre 2
## digitos ("BSH01".."BSH09") -- por isso essas 9 turbinas nunca faziam
## match em join_availability_to_turbine() (R/availability_daylight.R),
## ficando "sem unidade IDF primaria" no aviso/plot espacial quando na
## realidade tem, so' que o merge falhava pela diferenca de formato
## (confirmado pelo Paulo comparando sort(unique(wtg$InternalNa)) com
## sort(unique(turbine_idf_manual_dt[["Turbine ID"]])), 2026-08). Normaliza
## so' aqui -- InternalNa nao e' usado em mais nenhuma secção do relatorio
## mensal (ver comentario acima).
wtg$InternalNa <- {
  m <- regmatches(wtg$InternalNa, regexec("^([A-Za-z]+)([0-9]+)$", wtg$InternalNa))
  vapply(seq_along(wtg$InternalNa), function(i) {
    g <- m[[i]]
    if (length(g) < 3) return(wtg$InternalNa[i])
    paste0(g[2], sprintf("%02d", as.integer(g[3])))
  }, character(1))
}

turbine_idf_matrix_file <- file.path(folder_input, turbine_idf_matrix_filename)
if (file.exists(turbine_idf_matrix_file)) {
  turbine_idf_manual_dt <- readxl::read_xlsx(turbine_idf_matrix_file)
} else {
  message("Matriz turbina<->IDF nao disponivel -- plot espacial de disponibilidade (secção 1) sera saltado.")
  turbine_idf_manual_dt <- NULL
}


##
## 0. Filter data for the report month (ini/end vem de monthlyReportSettings.R) ----
##

report_start <- as.Date(ini)
report_end   <- as.Date(end)

scada_dt <- scada_dt_unfilt # NAO FILTRAR -- mesma logica de IDF_analysis.R

## Todos os filtros de data abaixo usam a MESMA sintaxe (data.table `[...]`,
## nao dplyr::filter()) e as MESMAS 2 variaveis auxiliares (filter_ini/
## filter_end), nunca `ini`/`end` diretamente dentro de um `DT[...]` --
## curtl_dt_unfilt TEM uma coluna chamada "end" (fim do proprio curtailment,
## ver R/read_curtailments.R), e dentro de DT[...] nomes de coluna tomam
## precedencia sobre variaveis do ambiente com o mesmo nome. Usar `ini`/`end`
## bare dentro do filtro de curtl_dt_unfilt fazia "start <= end" comparar
## com a COLUNA end (sempre >= o seu proprio start -- quase sempre TRUE),
## nao com o limite do periodo do relatorio -- bug real, encontrado 2026-08
## via monthly_data_summary_dt a mostrar Curtailments date_to muito alem do
## mes do relatorio. filter_ini/filter_end nao colidem com nenhuma coluna
## de curtl_dt_unfilt/track_dt_unfilt/heartb_dt_unfilt, nem com
## report_start/report_end (Date, definidos acima, usados em nomes de
## ficheiro) -- usados por omissao em TODOS os filtros, mesmo nos que hoje
## nao tem risco de colisao, para nao depender de confirmar caso a caso.
filter_ini <- ini
filter_end <- end

curtl_dt <- as.data.table(curtl_dt_unfilt)[start >= filter_ini & start <= filter_end]

track_dt <- as.data.table(track_dt_unfilt)[timestamp >= filter_ini & timestamp <= filter_end]
# count (nº de pontos por track) recalculado so' para o mes do relatorio --
# mesma correcao de IDF_analysis.R (0. Filter data), ver R/read_tracks.R
track_dt[, count := .N, by = track_id]

## Sem filtro por unidade IDF -- ao contrario de IDF_analysis.R
## (heartbeat_idf_units, um subconjunto historico com cobertura parcial), o
## relatorio mensal quer a disponibilidade de TODAS as unidades (heartbeats
## disponiveis para as 79 turbinas)
heartb_dt <- as.data.table(heartb_dt_unfilt)[timestamp >= filter_ini & timestamp <= filter_end]

# janela de SCADA disponivel para este mes -- interseccao entre a janela fixa
# de disponibilidade de SCADA (scada_ini/scada_end, monthlyReportSettings.R)
# e o mes do relatorio, para nao incluir historico fora do mes nem assumir
# SCADA antes de scada_ini
scada_ini_monthly <- max(scada_ini, ini)
scada_end_monthly <- min(scada_end, end)


##
## 0. Data extent / quantities table (tabela inicial de quantidades e janela temporal) ----
##

source("R/dataset_summary.R")

monthly_data_summary_dt <- summarise_dataset_extent(list(
  Tracks       = list(dt = track_dt,  date_col = "timestamp"),
  Curtailments = list(dt = curtl_dt,  date_col = "start"),
  SCADA        = list(dt = scada_dt[datetime >= scada_ini_monthly & datetime <= scada_end_monthly], date_col = "datetime"),
  Heartbeats   = list(dt = heartb_dt, date_col = "timestamp")
))

write_xlsx_local(
  list(Monthly_data_summary = monthly_data_summary_dt),
  file.path(folder_output, sprintf("data_summary_%s.xlsx", report_month))
)


##
## 0b. Temporal coverage of the 4 main datasets (gaps WITHIN the month) ----
##
## Complementa a tabela acima -- essa so' da min/max/n_rows (nao mostra SE ha
## dias sem dados dentro do periodo); este plot torna as lacunas visiveis
## dia a dia. Ver R/data_coverage.R (ja usado, de forma mais detalhada por
## turbina/unidade IDF, em IDF_analysis.R).
##

source("R/data_coverage.R")

# scada_dt NAO e' filtrado por ini/end (ver "0. Filter data", acima) -- copia
# local so' para este plot, na mesma janela SCADA-disponivel-x-mes usada na
# tabela de quantidades acima (scada_ini_monthly/scada_end_monthly)
scada_dt_month <- scada_dt[datetime >= scada_ini_monthly & datetime <= scada_end_monthly]

monthly_coverage_summary_dt <- data_coverage_summary(track_dt, curtl_dt, scada_dt_month, heartb_dt)

write_xlsx_local(
  list(Coverage_summary = monthly_coverage_summary_dt),
  file.path(folder_output, sprintf("data_coverage_%s.xlsx", report_month))
)

monthly_coverage_calendar_dt <- data_coverage_calendar(track_dt, curtl_dt, scada_dt_month, heartb_dt)

# breaks finos (o periodo e' so' 1 mes, "1 month" -- o default da funcao,
# pensado para o historico completo -- deixaria o eixo x com 1 unica marca)
p_monthly_coverage <- plot_data_coverage(monthly_coverage_calendar_dt, date_breaks = "2 days", date_labels = "%d %b")
ggsave(
  file.path(folder_output, sprintf("data_coverage_%s.png", report_month)),
  plot = p_monthly_coverage, width = 15, height = 8, units = "cm", dpi = 300, bg = "white"
)

## Cobertura combinada Curtailments x SCADA, POR TURBINA -- reusa as mesmas
## funcoes ja usadas (de forma identica) em IDF_analysis.R, secção "Data
## coverage: Curtailments vs SCADA". So' vai para outputs/ (annex) -- nao
## e' embutida no corpo do docx (tantas turbinas quantas turbinas_scada
## resolver, facet_wrap ncol=1 fica demasiado alta para uma pagina). O
## texto da secção 1.2 identifica so' as turbinas com sobreposicao
## incompleta (overlap_summary_by_turbine() abaixo).
##
## Restringido as turbinas com leituras SCADA NESTE MES (scada_dt_month) --
## de propósito DIFERENTE de turbinas_scada_resolved (secção 5, "alguma vez
## equipadas com SCADA", em TODO o historico do projeto). Essa resolucao
## "sempre" e' o comportamento certo para decidir que curtailments tentar
## avaliar (secções 5-7, onde queremos ser inclusivos mesmo com leituras
## esparsas nalguns meses) -- mas para ESTE plot mensal, uma turbina cujo
## unico registo SCADA de sempre veio de outro mes (ex: um download pontual
## feito no passado para uma investigacao de incidente especifica, sem
## relacao com este relatorio) so' apareceria aqui com uma faixa SCADA
## inteiramente em branco, sem informacao nenhuma -- ruido, nao sinal (bug
## reportado pelo Paulo com a BSH14, 2026-08).
turbinas_scada_this_month <- sort(unique(scada_dt_month$turbinelabel))

monthly_coverage_overlap_dt <- daily_overlap_by_turbine(
  curtl_dt[turbine %in% turbinas_scada_this_month], "start", "turbine", "Curtailments",
  scada_dt_month, "datetime", "turbinelabel", "SCADA"
)
monthly_coverage_overlap_summary_dt <- overlap_summary_by_turbine(monthly_coverage_overlap_dt, "Curtailments", "SCADA")

write_xlsx_local(
  list(Overlap = monthly_coverage_overlap_dt, Overlap_summary = monthly_coverage_overlap_summary_dt),
  file.path(folder_output, sprintf("data_coverage_turbine_overlap_%s.xlsx", report_month))
)

p_monthly_coverage_overlap <- plot_daily_overlap_by_turbine(
  monthly_coverage_overlap_dt, "Curtailments", "SCADA",
  date_breaks = "2 days", date_labels = "%Y-%m-%d"
)
ggsave(
  file.path(folder_output, sprintf("data_coverage_turbine_overlap_%s.png", report_month)),
  plot = p_monthly_coverage_overlap,
  width = 250, height = max(60, data.table::uniqueN(monthly_coverage_overlap_dt$turbine) * 25),
  units = "mm", dpi = 300, bg = "white", limitsize = FALSE
)

# "Consideradas para analise" = turbinas com ALGUMA leitura de AMBOS os
# lados neste mes (days_a > 0 & days_b > 0) -- excluir aqui as turbinas so'
# com curtailments e ZERO SCADA (a maioria da quinta, turbinas sem SCADA
# instalado) evita a tabela/texto ficarem cheios de 0%/NaN% sem
# significado (pedido do Paulo, 2026-08)
monthly_coverage_considered_dt <- monthly_coverage_overlap_summary_dt[days_a > 0 & days_b > 0]

coverage_considered_turbines_text <- if (nrow(monthly_coverage_considered_dt) == 0L) {
  "none"
} else {
  paste(sort(monthly_coverage_considered_dt$turbine), collapse = ", ")
}

# dentro do universo "considerado" (acima), turbinas com sobreposicao
# incompleta (< 100% num dos dois lados) -- formatado para o texto da
# secção 1.2 do Rmd
monthly_coverage_incomplete_dt <- monthly_coverage_considered_dt[
  pct_a_with_overlap < 100 | pct_b_with_overlap < 100
]
coverage_incomplete_turbines_text <- if (nrow(monthly_coverage_incomplete_dt) == 0L) {
  "none"
} else {
  paste(sprintf(
    "%s (Curtailments %.0f%%, SCADA %.0f%%)",
    monthly_coverage_incomplete_dt$turbine,
    monthly_coverage_incomplete_dt$pct_a_with_overlap,
    monthly_coverage_incomplete_dt$pct_b_with_overlap
  ), collapse = "; ")
}


##
## 1. System availability (heartbeats) ----
##

if (exists("heartb_dt") && isTRUE(run_sections_monthly$system_availability)) {

  source("R/availability_daylight.R")

  daylight_cal <- build_daylight_calendar(ini, end, proj_lat, proj_lon, proj_timezone)

  idf_availability_dt <- daylight_availability(
    heartb_dt, daylight_cal, proj_timezone,
    offline_gap_min  = heartbeat_offline_gap_min,
    online_grace_min = heartbeat_interval_min
  )

  idf_availability_summary <- summarise_availability(idf_availability_dt)

  idf_sel <- idf_availability_summary$by_idf[
    order(-offline_mins_total)][seq_len(min(idf_availability_top_n, .N)), idf]

  p_availability_cal <- plot_availability_calendar(
    idf_availability_dt, idf_availability_summary$by_idf,
    idf_sel = idf_sel, top_n = idf_availability_top_n
  )
  ggsave(
    file.path(folder_output, sprintf("idf_availability_calendar_%s.png", report_month)),
    plot = p_availability_cal, width = 16, height = 16, units = "cm", dpi = 300, bg = "white"
  )

  p_availability_freq <- plot_availability_frequency(idf_availability_summary$by_idf)
  ggsave(
    file.path(folder_output, sprintf("idf_availability_frequency_%s.png", report_month)),
    plot = p_availability_freq, width = 8, height = 8, units = "cm", dpi = 300, bg = "white"
  )

  write_xlsx_local(
    list(By_idf = idf_availability_summary$by_idf, By_month = idf_availability_summary$by_month),
    file.path(folder_output, sprintf("idf_availability_%s.xlsx", report_month))
  )

  ## 1b. Spatial unavailability -- 1 ponto por turbina (localizacao real),
  ## tamanho/cor = % offline da sua unidade IDF primaria neste mes (ver
  ## R/availability_daylight.R, join_availability_to_turbine()/
  ## plot_availability_spatial()). So' corre se a matriz manual
  ## turbina<->IDF estiver disponivel (ver "0. Import data").
  if (!is.null(turbine_idf_manual_dt)) {

    turbine_availability_dt <- join_availability_to_turbine(
      idf_availability_summary$by_idf, wtg, turbine_idf_manual_dt, wtg_id_col = "InternalNa"
    )

    write_xlsx_local(
      list(Turbine_availability = turbine_availability_dt),
      file.path(folder_output, sprintf("idf_availability_spatial_%s.xlsx", report_month))
    )

    p_availability_spatial <- plot_availability_spatial(turbine_availability_dt)
    ggsave(
      file.path(folder_output, sprintf("idf_availability_spatial_%s.png", report_month)),
      plot = p_availability_spatial, width = 16, height = 12, units = "cm", dpi = 300, bg = "white"
    )

  } else {message("Matriz turbina<->IDF nao disponivel -- 1b (spatial unavailability) saltada nesta ronda.")}

} else {message("heartb_dt nao disponivel ou run_sections_monthly$system_availability = FALSE -- 1 (system availability) saltada nesta ronda.")}


##
## 2. Observed species (riqueza de especies observadas) ----
##

if (exists("track_dt") && isTRUE(run_sections_monthly$observed_species)) {

  source("R/id_transitions.R")

  monthly_richness_dt <- track_species_summary(track_dt)
  monthly_richness_summary <- summarise_species_richness(monthly_richness_dt)

  write_xlsx_local(
    list(
      Track_species_richness = monthly_richness_dt,
      Richness_by_n_species  = monthly_richness_summary$by_n_species,
      Richness_rate          = monthly_richness_summary$rate,
      Richness_entropy       = monthly_richness_summary$entropy
    ),
    file.path(folder_output, sprintf("observed_species_%s.xlsx", report_month))
  )

  p_monthly_richness <- plot_species_richness_hist(monthly_richness_dt)
  ggsave(
    file.path(folder_output, sprintf("observed_species_richness_hist_%s.png", report_month)),
    plot = p_monthly_richness, width = 8, height = 8, units = "cm", dpi = 300, bg = "white"
  )

  p_monthly_entropy <- plot_entropy_hist(monthly_richness_dt)
  ggsave(
    file.path(folder_output, sprintf("observed_species_entropy_hist_%s.png", report_month)),
    plot = p_monthly_entropy, width = 16, height = 10, units = "cm", dpi = 300, bg = "white"
  )

} else {message("track_dt nao disponivel ou run_sections_monthly$observed_species = FALSE -- 2 (observed species) saltada nesta ronda.")}


##
## 3. Curtailments by species ----
##

if (exists("curtl_dt") && isTRUE(run_sections_monthly$curtailment_by_species)) {

  source("R/curtailment_species.R")

  monthly_species_curt_dt <- classify_curtailment_species(curtl_dt, prioritysp, nonprioritysp, othersp)
  monthly_species_curt_by_species_dt <- summarise_curtailment_species(monthly_species_curt_dt)
  monthly_species_curt_by_group_dt   <- summarise_curtailment_species_group(monthly_species_curt_dt)

  if (monthly_species_curt_by_group_dt[species_group == "uncategorized", .N] > 0) {
    message(sprintf(
      "Aviso: %d curtailments com species fora de prioritysp/nonprioritysp/othersp neste mes.",
      monthly_species_curt_by_group_dt[species_group == "uncategorized", n]
    ))
  }

  write_xlsx_local(
    list(
      Curtailments_by_species = monthly_species_curt_by_species_dt,
      Curtailments_by_group   = monthly_species_curt_by_group_dt
    ),
    file.path(folder_output, sprintf("curtailment_species_%s.xlsx", report_month))
  )

} else {message("curtl_dt nao disponivel ou run_sections_monthly$curtailment_by_species = FALSE -- 3 (curtailments by species) saltada nesta ronda.")}


##
## 4. Short-track curtailments ----
##

if (exists("track_dt") && exists("curtl_dt") && isTRUE(run_sections_monthly$short_track_curtailment)) {

  source("R/curtailment_short_track.R")

  monthly_short_track_dt <- classify_short_track_curtailments(
    track_dt, curtl_dt, min_points = shorttrack_min_points, eval_range_m = shorttrack_eval_range
  )
  monthly_short_track_summary_dt <- summarise_short_track_curtailments(track_dt, monthly_short_track_dt, shorttrack_min_points)
  monthly_short_track_by_species_dt <- summarise_short_track_by_species(monthly_short_track_dt, prioritysp)

  write_xlsx_local(
    list(
      Short_track_summary    = monthly_short_track_summary_dt,
      Short_track_by_species = monthly_short_track_by_species_dt,
      Short_track_detail     = monthly_short_track_dt
    ),
    file.path(folder_output, sprintf("curtailment_short_track_%s.xlsx", report_month))
  )

} else {message("track_dt/curtl_dt nao disponiveis ou run_sections_monthly$short_track_curtailment = FALSE -- 4 (short-track curtailment) saltada nesta ronda.")}


##
## 5. Curtailment response, shutdown time and safe distance (delays) ----
##

if (exists("scada_dt") && isTRUE(run_sections_monthly$curtailment_response_delays)) {

  source("R/curtailment_response.R")
  source("R/curtailment_shutdown_time.R")
  source("R/curtailment_safe_distance.R")

  # turbinas_scada = "all" (monthlyReportSettings.R) e' resolvido aqui contra
  # TODO o scada_dt (nao so' o mes do relatorio -- ver correcao 2026-08 em
  # resolve_turbinas_scada(), R/monthly_report_utils.R: filtrar so' pelo mes
  # excluia turbinas com SCADA instalado mas sem leituras nesse mes
  # especifico). scada_dt aqui ja nao e' filtrado por ini/end (ver "0.
  # Filter data", acima). Um vetor explicito passa tal e qual.
  turbinas_scada_resolved <- resolve_turbinas_scada(turbinas_scada, scada_dt)
  message(sprintf(
    "Turbinas SCADA usadas nesta ronda (%d): %s",
    length(turbinas_scada_resolved), paste(turbinas_scada_resolved, collapse = ", ")
  ))

  curtl_scada_dt <- curtl_dt[
    turbine %in% turbinas_scada_resolved & start >= scada_ini_monthly & start <= scada_end_monthly
  ]

  ### 5.1 Avaliacao principal (baseline apertado + resposta imediata + delta start->end)
  assess_dt <- assess_curtailment_response(
    curtl_scada_dt, scada_dt,
    start_end_gap_sec = curtailment_start_end_gap_sec, max_next_gap_sec = curtailment_max_next_gap_sec,
    drop_pct_threshold = curtailment_drop_pct_threshold, rpm_threshold = safe_shutdown_rpm
  )
  summary_assess <- summarise_curtailment_assessment(assess_dt)

  write_xlsx_local(
    list(
      Assessment   = assess_dt,
      By_status    = summary_assess$by_status,
      By_immediate = summary_assess$by_immediate,
      By_turbine   = summary_assess$by_turbine
    ),
    file.path(folder_output, sprintf("curtailment_response_assessment_%s.xlsx", report_month))
  )

  ### 5.2 Tempo ate atingir limiares de RPM (2, 1, 0), por curtailment
  tt_dt <- time_to_rpm_thresholds(
    curtl_scada_dt, scada_dt, thresholds = shutdown_time_thresholds,
    start_end_gap_sec = curtailment_start_end_gap_sec
  )
  summary_tt_by_turbine <- summarise_time_to_threshold(tt_dt)
  summary_tt_bands      <- summarise_time_to_threshold_bands(
    tt_dt, low_cut = shutdown_time_low_cut, high_cut = shutdown_time_high_cut
  )

  write_xlsx_local(
    list(
      Time_to_threshold = tt_dt,
      By_turbine        = summary_tt_by_turbine,
      Bands_40_50s      = summary_tt_bands
    ),
    file.path(folder_output, sprintf("curtailment_shutdown_time_%s.xlsx", report_month))
  )

  p_shutdown_time <- plot_time_to_threshold(tt_dt)
  ggsave(
    file.path(folder_output, sprintf("curtailment_shutdown_time_hist_%s.png", report_month)),
    plot = p_shutdown_time, width = 180, height = 200, units = "mm", dpi = 300, bg = "white"
  )

  ### 5.3 Safe distance (metodologia KNE)
  safe_dist_dt <- compute_safe_distance(
    curtl_scada_dt, scada_dt, track_dt,
    start_end_gap_sec = curtailment_start_end_gap_sec,
    rpm_threshold = safe_dist_rpm_threshold,
    speed_trim_q = safe_dist_speed_trim_q,
    already_slowing_rpm_threshold = safe_dist_already_slowing_rpm
  )
  summary_safe_dist <- summarise_safe_distance(safe_dist_dt, prioritysp)

  write_xlsx_local(
    list(
      Safe_distance = safe_dist_dt,
      Overall       = summary_safe_dist$overall,
      By_species    = summary_safe_dist$by_species
    ),
    file.path(folder_output, sprintf("curtailment_safe_distance_%s.xlsx", report_month))
  )

  p_safe_dist_hist <- plot_safe_distance_hist(
    safe_dist_dt, species_sel = prioritysp, ref_line_m = safe_dist_reference_line_m, facet = TRUE
  )

  ## altura dinamica -- plot_safe_distance_hist() usa facet_wrap(~species,
  ## ncol=3), por isso o numero de linhas da grelha varia com quantas
  ## especies (de prioritysp) tem registos este mes (de poucas ate' 13) --
  ## uma altura fixa ou fica cortada com muitas especies ou desperdica espaco
  ## com poucas. ~3.5cm por linha (pedido do Paulo) + 2.5cm fixos para
  ## titulo/eixo-x, minimo 10cm (1 linha).
  safe_dist_n_species <- data.table::uniqueN(
    safe_dist_dt[!is.na(min_safe_dist_m) & species %in% prioritysp]$species
  )
  safe_dist_plot_height_cm <- max(10, ceiling(safe_dist_n_species / 3) * 3.5 + 2.5)

  ggsave(
    file.path(folder_output, sprintf("curtailment_safe_distance_hist_%s.png", report_month)),
    plot = p_safe_dist_hist, width = 16, height = safe_dist_plot_height_cm, units = "cm", dpi = 300, bg = "white"
  )

} else {message("scada_dt nao disponivel ou run_sections_monthly$curtailment_response_delays = FALSE -- 5 (response/shutdown/safe distance) saltada nesta ronda.")}


##
## 6. Bi-directional ID transitions (P<->NP), global (todas as especies) ----
##
## So' o risco global de transicao P<->NP (direcao, atraso, numeros gerais) --
## NAO inclui matriz de confusao por especie (essa e' a secção 6b, abaixo,
## desligada por omissao). O metodo e' o mesmo usado na analise de remocao
## do Kestrel (R/curtailment_removal_risk.R), aqui aplicado ao conjunto
## completo de especies nao-prioritarias, nao a uma unica especie.
##

if (exists("track_dt") && exists("curtl_dt") && isTRUE(run_sections_monthly$id_transitions)) {

  source("R/id_transitions.R")

  ## reutiliza monthly_richness_dt calculado na secção 2 (mesmo track_dt);
  ## recalcula aqui de forma defensiva, caso a secção 2 tenha sido saltada
  if (!exists("monthly_richness_dt")) monthly_richness_dt <- track_species_summary(track_dt)

  monthly_id_risk_dt <- classify_id_transition_risk(
    monthly_richness_dt, track_dt, curtl_dt, prioritysp,
    late_time_threshold_sec = id_transition_late_time_sec,
    late_dist_threshold_m = track_proximity_threshold_m
  )
  monthly_id_risk_summary <- summarise_id_transition_risk(monthly_id_risk_dt, curtl_dt)
  monthly_id_late_cases_dt <- id_transition_late_cases(monthly_id_risk_dt, curtl_dt)

  write_xlsx_local(
    list(
      ID_transition_risk    = monthly_id_risk_dt,
      Risk_by_direction     = monthly_id_risk_summary$by_direction,
      Risk_PNP_curtailments = monthly_id_risk_summary$pnp_curtailments,
      Late_criteria_compare = monthly_id_risk_summary$late_criteria_compare,
      Late_cases_detail     = monthly_id_late_cases_dt
    ),
    file.path(folder_output, sprintf("id_transitions_%s.xlsx", report_month))
  )

  p_monthly_late_time <- plot_late_time_distribution(monthly_id_risk_dt, threshold_sec = id_transition_late_time_sec)
  ggsave(
    file.path(folder_output, sprintf("id_transition_late_time_dist_%s.png", report_month)),
    plot = p_monthly_late_time, width = 7, height = 4, dpi = 300, bg = "white"
  )

  p_monthly_late_dist <- plot_late_dist_distribution(monthly_id_risk_dt, threshold_m = track_proximity_threshold_m)
  ggsave(
    file.path(folder_output, sprintf("id_transition_late_dist_dist_%s.png", report_month)),
    plot = p_monthly_late_dist, width = 7, height = 4, dpi = 300, bg = "white"
  )

} else {message("track_dt/curtl_dt nao disponiveis ou run_sections_monthly$id_transitions = FALSE -- 6 (ID transitions) saltada nesta ronda.")}


##
## 6b. Species confusion matrix (opcional, DESLIGADA por omissao) ----
##
## Que outras especies aparecem no mesmo track que qualquer uma das especies
## em id_confusion_species_of_interest (ver inputs/monthlyReportSettings.R --
## aceita 1 ou varias) -- analise pontual (ex: Egyptian-Vulture/Steppe-Eagle
## por omissao, as especies dos incidentes de fatalidade conhecidos), nao uma
## metrica mensal de rotina. Ligar via run_sections_monthly$id_confusion =
## TRUE e ajustar id_confusion_species_of_interest quando for preciso.
##

## Limpar estado de uma corrida anterior NA MESMA sessao R -- source() nao
## remove variaveis nao tocadas pela corrida atual. Sem isto, se esta
## secção tivesse corrido com run_sections_monthly$id_confusion = TRUE numa
## corrida anterior (mesma sessao) e agora estiver FALSE,
## monthly_id_confusion_summary continuaria a "existir" (desatualizado, de
## outra corrida/mes) -- o relatorio mostraria dados errados na secção 9.2
## em vez de a saltar, e id_confusion_species_label (so' criada dentro do
## bloco abaixo) ficaria em falta, causando "object not found".
rm(list = intersect(c("monthly_id_confusion_summary", "id_confusion_species_label"), ls()))

if (exists("track_dt") && exists("curtl_dt") && isTRUE(run_sections_monthly$id_confusion)) {

  source("R/id_transitions.R")

  if (!exists("monthly_richness_dt")) monthly_richness_dt <- track_species_summary(track_dt)

  monthly_id_confusion_summary <- summarise_species_confusion(
    track_dt, monthly_richness_dt, curtl_dt, id_confusion_species_of_interest
  )

  # id_confusion_species_of_interest pode ter varias especies -- juntar num
  # unico texto/nome de ficheiro (sprintf com um vetor de tamanho >1
  # devolveria varios ficheiros/titulos, nao 1)
  id_confusion_species_label <- paste(id_confusion_species_of_interest, collapse = " & ")

  write_xlsx_local(
    list(
      Confusion_rate_compare = monthly_id_confusion_summary$rate_compare,
      Confusion_general      = monthly_id_confusion_summary$confusion_general,
      Confusion_curtailments = monthly_id_confusion_summary$confusion_curtailments
    ),
    file.path(folder_output, sprintf(
      "id_confusion_%s_%s.xlsx", paste(id_confusion_species_of_interest, collapse = "_"), report_month
    ))
  )

} else {message("run_sections_monthly$id_confusion = FALSE (por omissao) -- 6b (species confusion matrix) saltada nesta ronda.")}


##
## 7. Biological flight metrics (velocidade/altura de voo por especie) ----
##

if (exists("track_dt") && isTRUE(run_sections_monthly$bio_flight_metrics)) {

  source("R/bio_flight_metrics.R")

  monthly_flight_base_dt <- flight_metrics_base(
    track_dt, prioritysp, min_track_points = flight_min_track_points,
    speed_ms_min = flight_speed_ms_min, speed_ms_max = flight_speed_ms_max
  )
  monthly_flight_speed_summary_dt  <- summarise_flight_speed(monthly_flight_base_dt)
  monthly_flight_height_summary_dt <- summarise_flight_height(
    monthly_flight_base_dt, risk_height_lower = riskHeight_lower, risk_height_upper = riskHeight_upper
  )

  write_xlsx_local(
    list(
      Flight_speed_by_species  = monthly_flight_speed_summary_dt,
      Flight_height_by_species = monthly_flight_height_summary_dt
    ),
    file.path(folder_output, sprintf("bio_flight_metrics_%s.xlsx", report_month))
  )

  p_monthly_flight_metrics <- plot_flight_metrics_distribution(monthly_flight_base_dt, riskHeight_lower, riskHeight_upper)
  ggsave(
    file.path(folder_output, sprintf("bio_flight_metrics_distribution_%s.png", report_month)),
    plot = p_monthly_flight_metrics, width = 16, height = 15, units = "cm", dpi = 300, bg = "white"
  )

} else {message("track_dt nao disponivel ou run_sections_monthly$bio_flight_metrics = FALSE -- 7 (bio flight metrics) saltada nesta ronda.")}


##
## 8. Minimum individuals per time bin ----
##

if (exists("track_dt") && isTRUE(run_sections_monthly$min_individuals)) {

  source("R/track_min_individuals.R")

  monthly_min_indiv_bins_dt <- count_min_individuals_per_bin(
    track_dt, species = prioritysp,
    bin_min = min_individuals_bin_min, merge_dist_m = min_individuals_merge_dist_m
  )
  monthly_min_indiv_summary_dt <- summarise_min_individuals(monthly_min_indiv_bins_dt)
  monthly_min_indiv_daily_dt   <- summarise_daily_max_individuals(monthly_min_indiv_bins_dt)

  write_xlsx_local(
    list(
      Min_individuals_bins    = monthly_min_indiv_bins_dt,
      Min_individuals_summary = monthly_min_indiv_summary_dt,
      Min_individuals_daily   = monthly_min_indiv_daily_dt
    ),
    file.path(folder_output, sprintf("min_individuals_per_bin_%s.xlsx", report_month))
  )

  # species_sel = prioritysp (nao o default da funcao, so' 2 especies) --
  # mostrar todas as especies prioritarias, 1 painel por especie -- exceto
  # "Protected" (categoria generica, nao 1 especie concreta), removida deste
  # plot a pedido do Paulo (2026-08)
  p_monthly_min_indiv_daily <- plot_daily_max_individuals(
    monthly_min_indiv_daily_dt, species_sel = setdiff(prioritysp, "Protected"),
    date_breaks = "1 week", date_labels = "%d %b"
  )
  ggsave(
    file.path(folder_output, sprintf("min_individuals_daily_max_%s.png", report_month)),
    plot = p_monthly_min_indiv_daily, width = 16, height = 22, units = "cm", dpi = 300, bg = "white"
  )

} else {message("track_dt nao disponivel ou run_sections_monthly$min_individuals = FALSE -- 8 (min individuals) saltada nesta ronda.")}


##
## Export monthly report (Word, via Rmd) ----
##

source("R/report.R")

monthly_report_params <- list(
  title         = report_title_monthly,
  project_ref   = project_ref,
  report_month  = report_month,
  report_start  = as.character(report_start),
  report_end    = as.character(report_end),
  analysis_date = format(Sys.time(), "%Y-%m-%d"),
  username      = username,
  code_version  = code_version,

  data_summary = if (exists("monthly_data_summary_dt")) monthly_data_summary_dt else NULL,
  coverage_summary = if (exists("monthly_coverage_summary_dt")) monthly_coverage_summary_dt else NULL,
  coverage_plot    = if (exists("p_monthly_coverage")) p_monthly_coverage else NULL,
  coverage_considered_turbines_text = if (exists("coverage_considered_turbines_text")) coverage_considered_turbines_text else NULL,
  coverage_incomplete_turbines_text = if (exists("coverage_incomplete_turbines_text")) coverage_incomplete_turbines_text else NULL,

  availability_by_idf   = if (exists("idf_availability_summary")) idf_availability_summary$by_idf else NULL,
  availability_plot_cal = if (exists("p_availability_cal")) p_availability_cal else NULL,
  availability_plot_freq = if (exists("p_availability_freq")) p_availability_freq else NULL,
  availability_plot_spatial = if (exists("p_availability_spatial")) p_availability_spatial else NULL,

  richness_dt         = if (exists("monthly_richness_summary")) monthly_richness_summary$by_n_species else NULL,
  entropy_plot        = if (exists("p_monthly_entropy")) p_monthly_entropy else NULL,
  # richness_plot (histograma de n_species por track) removida do docx por
  # pedido -- p_monthly_richness continua a ser calculado e gravado em
  # outputs/ (ver secção 2, acima), so' deixou de ser passado ao Rmd

  curtl_by_species_dt = if (exists("monthly_species_curt_by_species_dt")) monthly_species_curt_by_species_dt else NULL,
  curtl_by_group_dt   = if (exists("monthly_species_curt_by_group_dt")) monthly_species_curt_by_group_dt else NULL,

  short_track_summary_dt = if (exists("monthly_short_track_summary_dt")) monthly_short_track_summary_dt else NULL,

  assess_by_status  = if (exists("summary_assess")) summary_assess$by_status else NULL,
  assess_by_turbine = if (exists("summary_assess")) summary_assess$by_turbine else NULL,

  shutdown_by_turbine = if (exists("summary_tt_by_turbine")) summary_tt_by_turbine else NULL,
  shutdown_bands      = if (exists("summary_tt_bands")) summary_tt_bands else NULL,
  shutdown_plot       = if (exists("p_shutdown_time")) p_shutdown_time else NULL,

  safe_dist_overall     = if (exists("summary_safe_dist")) summary_safe_dist$overall else NULL,
  safe_dist_by_species  = if (exists("summary_safe_dist")) summary_safe_dist$by_species else NULL,
  safe_dist_plot        = if (exists("p_safe_dist_hist")) p_safe_dist_hist else NULL,
  # fallback (3.94in = 10cm, o valor fixo antigo) so' usado se a secção 5 nao
  # correr -- nesse caso safe_dist_plot tambem e' NULL e nada e' desenhado,
  # mas o chunk do Rmd ainda precisa de um fig.height numerico valido
  safe_dist_plot_height_in = if (exists("safe_dist_plot_height_cm")) safe_dist_plot_height_cm / 2.54 else 3.94,

  id_risk_by_direction = if (exists("monthly_id_risk_summary")) monthly_id_risk_summary$by_direction else NULL,
  id_confusion_species = if (exists("monthly_id_confusion_summary")) id_confusion_species_label else NULL,
  id_confusion_general = if (exists("monthly_id_confusion_summary")) monthly_id_confusion_summary$confusion_general else NULL,

  flight_speed_by_species  = if (exists("monthly_flight_speed_summary_dt")) monthly_flight_speed_summary_dt else NULL,
  flight_height_by_species = if (exists("monthly_flight_height_summary_dt")) monthly_flight_height_summary_dt else NULL,
  flight_metrics_plot      = if (exists("p_monthly_flight_metrics")) p_monthly_flight_metrics else NULL,

  min_indiv_summary = if (exists("monthly_min_indiv_summary_dt")) monthly_min_indiv_summary_dt else NULL,
  min_indiv_daily_plot = if (exists("p_monthly_min_indiv_daily")) p_monthly_min_indiv_daily else NULL,

  ## Literais de configuracao (monthlyReportSettings.R) -- so' para texto
  ## descritivo no Rmd (ver report/monthly_report_template.rmd), NAO
  ## controlam nenhum calculo aqui. Sempre definidos independentemente dos
  ## switches de run_sections_monthly (sao literais de settings, nao
  ## objetos calculados).
  heartbeat_interval_min    = heartbeat_interval_min,
  heartbeat_offline_gap_min = heartbeat_offline_gap_min,
  idf_availability_top_n    = idf_availability_top_n,

  shorttrack_min_points   = shorttrack_min_points,
  shorttrack_eval_range_m = shorttrack_eval_range,

  curtailment_start_end_gap_sec  = curtailment_start_end_gap_sec,
  curtailment_max_next_gap_sec   = curtailment_max_next_gap_sec,
  curtailment_drop_pct_threshold = curtailment_drop_pct_threshold,
  safe_shutdown_rpm               = safe_shutdown_rpm,
  turbinas_scada_used = if (exists("turbinas_scada_resolved")) paste(turbinas_scada_resolved, collapse = ", ") else NULL,

  shutdown_time_thresholds = shutdown_time_thresholds,
  shutdown_time_low_cut  = shutdown_time_low_cut,
  shutdown_time_high_cut = shutdown_time_high_cut,

  safe_dist_reference_line_m    = safe_dist_reference_line_m,
  safe_dist_rpm_threshold        = safe_dist_rpm_threshold,
  safe_dist_already_slowing_rpm  = safe_dist_already_slowing_rpm,

  id_transition_late_time_sec = id_transition_late_time_sec,
  track_proximity_threshold_m = track_proximity_threshold_m,

  flight_min_track_points = flight_min_track_points,
  flight_speed_ms_min     = flight_speed_ms_min,
  flight_speed_ms_max     = flight_speed_ms_max,
  riskHeight_lower         = riskHeight_lower,
  riskHeight_upper         = riskHeight_upper,

  min_individuals_bin_min       = min_individuals_bin_min,
  min_individuals_merge_dist_m  = min_individuals_merge_dist_m
)

## Reutiliza o template Word da empresa (estilos, cabecalho/rodape com
## numeracao de pagina) -- ver comentario em R/report.R (build_idf_report).
build_idf_report(
  output_file    = file.path(folder_output, sprintf("IDF_monthly_report_%s.docx", report_month)),
  report_params  = monthly_report_params,
  template       = "report/monthly_report_template.rmd",
  reference_docx = file.path(folder_input, "Mod.001.05_template_documentos_gerais.docx")
)
