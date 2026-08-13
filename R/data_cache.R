##
## Cache local (fst) para os datasets grandes (tracks, curtailments, SCADA,
## heartbeats) -- evita reler todos os ficheiros brutos via read_*_data()
## (pode demorar muito, com datasets na ordem dos milhoes de linhas, que so
## tendem a crescer ao longo do projeto) sempre que se corre o script.
##
## fst e' usado por ser o formato mais rapido para gravar/ler data.table
## "planos" (sem colunas de lista/geometria) -- e o caso dos 4 datasets.
##
## load_or_read_cache(cache_file, read_fn, force_reread = FALSE):
##   se cache_file existir e force_reread = FALSE, le e devolve a cache;
##   senao corre read_fn(), grava o resultado em cache_file e devolve-o.
##
## Depende de: fst, data.table
##
## Uso:
##   source("R/data_cache.R")
##   track_dt_unfilt <- load_or_read_cache(
##     file.path(folder_cache, "track_dt_unfilt.fst"),
##     function() read_tracks_data(databases_dirs, trackreport_pattern, tz = proj_timezone),
##     force_reread = force_reread_cache
##   )
##


load_or_read_cache <- function(cache_file, read_fn, force_reread = FALSE) {

  if (!force_reread && file.exists(cache_file)) {
    dt <- fst::read_fst(cache_file, as.data.table = TRUE)
    cat(sprintf("Cache: '%s' carregada (%d linhas).\n", cache_file, nrow(dt)))
    return(dt)
  }

  dt <- read_fn()

  dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
  fst::write_fst(dt, cache_file)
  cat(sprintf("Cache: '%s' gravada (%d linhas).\n", cache_file, nrow(dt)))

  dt
}
