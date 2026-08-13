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
