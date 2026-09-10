##
## Utilitarios partilhados pelos read_*.R (tracks, curtailments, SCADA, heartbeats)
##
## Depende de: data.table (so' read_csv_files_safe(), abaixo)
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


## Le varios ficheiros CSV com fread() e junta-os com rbindlist(), mas
## ignora (com aviso, nao silenciosamente) qualquer ficheiro cujo numero de
## colunas nao bate com a maioria dos outros -- protege contra um ficheiro
## vazio/corrompido na pasta de dados brutos (ex: download interrompido, 0
## bytes), que de outra forma faz rbindlist() falhar com "Item N has X
## columns, inconsistent with item 1 which has Y columns" e perde TODO o
## trabalho ja feito nas fontes lidas antes dele na mesma corrida (bug real,
## 2026-09, SCADA: 1 ficheiro de 83 so' com 1 coluna em vez de 13).
##
## ... e' passado directamente a fread() (ex: sep, header, na.strings) --
## mesma assinatura de read_scada_data()/read_heartbeats_data() antes desta
## funcao existir.
read_csv_files_safe <- function(files, ...) {

  dt_list <- lapply(files, function(f) data.table::fread(f, ...))

  ncols <- vapply(dt_list, ncol, integer(1))
  expected_ncol <- as.integer(names(sort(table(ncols), decreasing = TRUE))[1])

  bad <- ncols != expected_ncol
  if (any(bad)) {
    message(sprintf(
      "read_csv_files_safe: %d de %d ficheiro(s) IGNORADOS -- numero de colunas nao bate com a maioria (%d colunas esperadas):\n%s",
      sum(bad), length(files), expected_ncol,
      paste(sprintf("  %s (%d coluna(s))", files[bad], ncols[bad]), collapse = "\n")
    ))
  }

  data.table::rbindlist(dt_list[!bad])
}
