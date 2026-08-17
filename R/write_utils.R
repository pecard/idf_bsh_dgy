##
## Utilitarios de escrita -- corrige a exportacao de POSIXct para xlsx
##
## writexl::write_xlsx() NAO preserva o atributo tzone de colunas POSIXct:
## grava o instante em segundos UTC (a representacao interna do R,
## independente do tzone "rotulado" usado so para mostrar a hora), e o
## Excel mostra esse numero como se fosse a hora literal -- ou seja,
## qualquer coluna POSIXct com tzone diferente de UTC (todo o projeto usa
## proj_timezone = "Asia/Samarkand", UTC+5) sai 5h ATRASADA no ficheiro
## .xlsx, mesmo que o R mostre a hora certa no console.
##
## Confirmado empiricamente (2026-08): um track_id com first_time=10:49:25
## em R (fatality_tracks_dt) saiu como 05:49:25 no
## fatality_track_investigation.xlsx.
##
## E' a mesma familia do bug ja corrigido em R/data_cache.R (o fst tambem
## nao preserva tzone), so que ali a perda acontecia na LEITURA da cache;
## aqui acontece na ESCRITA para xlsx.
##
## Uso: trocar writexl::write_xlsx(...) por write_xlsx_local(...) em
## qualquer chamada que grave uma tabela com colunas POSIXct.
##
## Depende de: writexl
##


## 1. Corrige as colunas POSIXct de UM data.frame/data.table para os
##    numeros saírem certos no xlsx (nao muda o instante real, so a forma
##    como fica gravado) ----

fix_posixct_for_excel <- function(df) {

  df <- as.data.frame(df)

  for (col in names(df)) {
    if (inherits(df[[col]], "POSIXct")) {
      # re-interpreta os MESMOS digitos do relogio (ja no tzone correto, ver
      # force_tz() em read_*_data()) como UTC -- nao desloca o instante
      # mostrado, so evita que o writexl aplique o deslocamento indevido
      df[[col]] <- as.POSIXct(format(df[[col]]), tz = "UTC")
    }
  }

  df
}


## 2. Wrapper direto de writexl::write_xlsx() -- aplica a correcao a cada
##    folha (aceita 1 data.frame ou uma list de varias, tal como a funcao
##    original) antes de gravar ----

write_xlsx_local <- function(x, path, ...) {

  if (is.data.frame(x)) {
    x <- fix_posixct_for_excel(x)
  } else if (is.list(x)) {
    x <- lapply(x, function(sheet) if (is.data.frame(sheet)) fix_posixct_for_excel(sheet) else sheet)
  }

  writexl::write_xlsx(x, path, ...)
}
