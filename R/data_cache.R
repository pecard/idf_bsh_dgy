##
## Cache local (fst) para os datasets grandes (tracks, curtailments, SCADA,
## heartbeats) -- evita reler todos os ficheiros brutos via read_*_data()
## (pode demorar muito, com datasets na ordem dos milhoes de linhas, que so
## tendem a crescer ao longo do projeto) sempre que se corre o script.
##
## fst e' usado por ser o formato mais rapido para gravar/ler data.table
## "planos" (sem colunas de lista/geometria) -- e o caso dos 4 datasets.
##
## load_or_read_cache(cache_file, read_fn, force_reread = FALSE, tz = NULL):
##   se cache_file existir e force_reread = FALSE, le e devolve a cache;
##   senao corre read_fn(), grava o resultado em cache_file e devolve-o.
##
## tz: fst NAO preserva o atributo tzone das colunas POSIXct no round-trip
## (so preserva o instante em si -- POSIXct e sempre segundos desde epoch em
## UTC internamente, o tzone e so metadata de apresentacao/parsing de datas).
## Sem isto, depois de carregar da cache os timestamps voltam sem a tzone
## local (proj_timezone) que read_*_data() tinha aplicado -- reaplicar o
## atributo aqui NAO altera o instante, so corrige a "etiqueta" perdida.
## Passa tz = proj_timezone sempre que o objeto tiver colunas POSIXct.
##
## Depende de: fst, data.table
##
## Uso:
##   source("R/data_cache.R")
##   track_dt_unfilt <- load_or_read_cache(
##     file.path(folder_cache, "track_dt_unfilt.fst"),
##     function() read_tracks_data(databases_dirs, trackreport_pattern, tz = proj_timezone),
##     force_reread = force_reread_cache,
##     tz = proj_timezone
##   )
##


load_or_read_cache <- function(cache_file, read_fn, force_reread = FALSE, tz = NULL) {

  if (!force_reread && file.exists(cache_file)) {
    dt <- fst::read_fst(cache_file, as.data.table = TRUE)
    if (!is.null(tz)) {
      posix_cols <- names(dt)[vapply(dt, inherits, logical(1), what = "POSIXct")]
      for (col in posix_cols) {
        x <- dt[[col]]
        attr(x, "tzone") <- tz
        data.table::set(dt, j = col, value = x)
      }
    }
    cat(sprintf("Cache: '%s' carregada (%d linhas).\n", cache_file, nrow(dt)))
    return(dt)
  }

  dt <- read_fn()

  dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
  fst::write_fst(dt, cache_file)
  cat(sprintf("Cache: '%s' gravada (%d linhas).\n", cache_file, nrow(dt)))

  dt
}


## Evita reler a cache do disco (fst::read_fst(), varios milhoes de linhas
## nos datasets maiores) quando o objeto ja esta em memoria de uma corrida
## anterior NA MESMA sessao R -- ex: gerar o relatorio mensal para varios
## meses seguidos sem reiniciar o R, ou correr IDF_analysis.R e depois
## IDF_monthly_report.R na mesma sessao (os 2 usam os MESMOS ficheiros de
## cache/objetos "_unfilt", que nao dependem do mes do relatorio -- so' a
## filtragem por mes, feita depois, e' que muda). Chamar com
## `current = if (exists("nome_do_objeto")) nome_do_objeto else NULL`.
##
## force_reread = TRUE ignora sempre o que estiver em memoria e vai ao
## disco (ou aos ficheiros brutos, conforme load_or_read_cache()) -- e'
## o caso de "acabei de descarregar dados novos", onde o objeto em
## memoria pode estar desatualizado.
##
## Uso:
##   track_dt_unfilt <- reuse_or_load_cache(
##     if (exists("track_dt_unfilt")) track_dt_unfilt else NULL,
##     "track_dt_unfilt", file.path(folder_cache, "track_dt_unfilt.fst"),
##     function() read_tracks_data(databases_dirs, trackreport_pattern, tz = proj_timezone),
##     force_reread = force_reread_cache_monthly, tz = proj_timezone
##   )

reuse_or_load_cache <- function(current, name, cache_file, read_fn, force_reread = FALSE, tz = NULL) {

  if (!force_reread && !is.null(current)) {
    cat(sprintf("Cache: '%s' ja em memoria (%d linhas) -- reutilizada sem tocar no disco.\n", name, nrow(current)))
    return(current)
  }

  load_or_read_cache(cache_file, read_fn, force_reread = force_reread, tz = tz)
}
