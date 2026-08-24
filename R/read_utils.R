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
##
## farm_pattern (opcional): databases_dir e' partilhado entre projetos (BSH
## e DGY apontam para a MESMA pasta local "data-raw"), e os 2 parques usam o
## mesmo pattern por tipo de dataset (ex: scada_pattern = "SCADA_.+csv" nos
## 2) -- sem discriminar por parque, correr o projeto BSH apanharia tambem
## ficheiros SCADA do DGY que estejam nessa pasta partilhada, e vice-versa.
## O codigo do parque ("BSH"/"DGY") aparece no nome do ficheiro, mas NAO
## necessariamente na mesma posicao entre datasets/convencoes de nome (ex:
## SCADA_20260801_20260815_BSH_T014.csv vs SCADA_BSH014_20260801_20260815_BSH.csv)
## -- por isso farm_pattern e' aplicado como um filtro SEPARADO (2a
## passagem, por substring em qualquer posicao), em vez de tentar embutir a
## posicao exata do codigo num unico regex combinado com `pattern`.
list_files_multi_dir <- function(databases_dirs, pattern, farm_pattern = NULL) {
  per_dir <- lapply(databases_dirs, function(d) list.files(d, pattern = pattern, full.names = TRUE))
  for (i in seq_along(databases_dirs)) {
    message(sprintf("list_files_multi_dir: %d ficheiro(s) '%s' em '%s'", length(per_dir[[i]]), pattern, databases_dirs[i]))
  }

  files <- unlist(per_dir, use.names = FALSE)

  if (!is.null(farm_pattern)) {
    n_before <- length(files)
    files <- files[grepl(farm_pattern, basename(files))]
    message(sprintf(
      "list_files_multi_dir: %d de %d ficheiro(s) mantidos apos filtrar por farm_pattern = '%s'.",
      length(files), n_before, farm_pattern
    ))
  }

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
