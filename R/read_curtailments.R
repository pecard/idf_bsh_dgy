##
## Read Curtailments data (ordens de paragem/curtailment)
##
## Depende de: readxl, janitor, lubridate, dplyr, R/read_utils.R
##
## read_xlsx() abaixo e' sempre com o prefixo readxl:: -- outro pacote
## carregado no mesmo script (ex: openxlsx) pode tambem exportar uma funcao
## com este nome mas assinatura diferente; sem o prefixo, o ultimo pacote
## anexado (library()) na ordem de "packages" ganha o nome no search path,
## e a chamada falha com algo como "unused arguments (sheet = 1, skip = 1)"
## -- ja aconteceu (2026-08).
##

read_curtailments_data <- function(databases_dirs, pattern, tz = "UTC") {

  files <- list_files_multi_dir(databases_dirs, pattern)

  read_one_file <- function(f) {
    readxl::read_xlsx(f, sheet = 1, skip = 1) %>%
      clean_names() %>%
      mutate(
        start = lubridate::as_datetime(start),
        end   = lubridate::as_datetime(end),
        # forcar character explicitamente -- se uma destas colunas vier
        # totalmente vazia num ficheiro (ex: turbina nao preenchida em
        # nenhuma linha), o readxl infere-a como logical (NA), e o
        # bind_rows() abaixo falha a combinar esse ficheiro com os outros
        # (character vs logical) -- ver erro "Can't combine ..$turbine
        # <character> and ..$turbine <logical>"
        turbine  = as.character(turbine),
        track_id = as.character(track_id),
        species  = as.character(species)
      )
  }

  dt <-
    lapply(files, read_one_file) %>%
    bind_rows() %>%
    distinct() %>% # remover linhas duplicadas -- ex: mesmo ficheiro/periodo repetido entre diretorios diferentes
    mutate(
      # fonte (portal IdentiFlight) ja vem em hora LOCAL (confirmado), nao UTC --
      # force_tz() reinterpreta os mesmos numeros do relogio como sendo tz local,
      # SEM deslocar o instante (with_tz() deslocaria +/- o offset, dando horas
      # absolutas erradas -- foi o bug que motivou esta correcao)
      start = lubridate::force_tz(start, tz),
      end   = lubridate::force_tz(end, tz),
      monthy_y = format(as.Date(start), "%Y-%m")
    )

  dt
}
