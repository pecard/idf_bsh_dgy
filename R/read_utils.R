##
## Utilitarios partilhados pelos read_*.R (tracks, curtailments, SCADA, heartbeats)
##
## Depende de: (nenhuma alem de base R)
##


## Lista ficheiros que correspondem a um padrao, em 1 ou varios diretorios,
## sem duplicar pelo nome do ficheiro. Quando o mesmo nome de ficheiro existe
## em mais do que um diretorio, ganha o PRIMEIRO diretorio da lista -- a
## ordem em databases_dirs define a precedencia.
##
## Reporta (message(), nao afeta o valor devolvido) quantos ficheiros o
## padrao apanhou em CADA diretorio -- unica forma de confirmar, a olho na
## consola, que um databases_dir_alt (ex: a pasta de rede do BSH) esta mesmo
## a ser lido, e nao so' o databases_dir local (pedido do Paulo, 2026-08).
list_files_multi_dir <- function(databases_dirs, pattern) {
  per_dir <- lapply(databases_dirs, function(d) list.files(d, pattern = pattern, full.names = TRUE))
  for (i in seq_along(databases_dirs)) {
    message(sprintf("list_files_multi_dir: %d ficheiro(s) '%s' em '%s'", length(per_dir[[i]]), pattern, databases_dirs[i]))
  }

  files <- unlist(per_dir, use.names = FALSE)
  out <- files[!duplicated(basename(files))]

  n_dupes <- length(files) - length(out)
  if (n_dupes > 0) {
    message(sprintf(
      "list_files_multi_dir: %d ficheiro(s) com o mesmo nome em mais de 1 diretorio -- mantido o do 1o diretorio da lista (precedencia por ordem em databases_dirs).",
      n_dupes
    ))
  }

  out
}
