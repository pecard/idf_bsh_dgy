##
## Utilitarios partilhados pelos read_*.R (tracks, curtailments, SCADA, heartbeats)
##
## Depende de: (nenhuma alem de base R)
##


## Lista ficheiros que correspondem a um padrao, em 1 ou varios diretorios,
## sem duplicar pelo nome do ficheiro. Quando o mesmo nome de ficheiro existe
## em mais do que um diretorio, ganha o PRIMEIRO diretorio da lista -- a
## ordem em databases_dirs define a precedencia.
list_files_multi_dir <- function(databases_dirs, pattern) {
  files <- unlist(lapply(databases_dirs, list.files, pattern = pattern, full.names = TRUE))
  files[!duplicated(basename(files))]
}
