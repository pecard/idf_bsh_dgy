##
## Monta e renderiza o relatorio Word com os principais resultados
##
## O template (report/report_template.rmd) nao recalcula nada -- so formata
## os objetos ja calculados pelas seccoes do IDF_analysis.R, recebidos aqui
## como params. Cada param pode ser NULL se essa seccao nao tiver corrido
## (ex: sem dados de SCADA nesse periodo) -- o template salta essa secção.
##
## Depende de: rmarkdown
##
## reference_docx (opcional): caminho para um .docx cujos estilos,
## cabecalho/rodape (incluindo numeracao de paginas, se o .docx a tiver) e
## configuracao de pagina o pandoc reutiliza no documento gerado -- o
## CONTEUDO desse .docx (texto/imagens no corpo) e' ignorado, so' os
## estilos/secPr contam. NULL (por omissao) mantem o comportamento antigo
## (estilos genericos do word_document do rmarkdown, sem numeracao de
## pagina).

build_idf_report <- function(output_file, report_params,
                             template = "report/report_template.rmd",
                             reference_docx = NULL) {

  # rmarkdown::render() resolve output_file de forma inconsistente quando
  # tem um caminho relativo com diretorio (por vezes relativo a pasta do
  # proprio .Rmd, nao a working directory da chamada) -- separar em
  # output_dir (absoluto) + output_file (so o nome) evita essa ambiguidade
  out_dir <- dirname(output_file)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  output_options <- if (!is.null(reference_docx)) {
    list(reference_docx = normalizePath(reference_docx))
  } else {
    NULL
  }

  out <- rmarkdown::render(
    input          = template,
    output_dir     = normalizePath(out_dir),
    output_file    = basename(output_file),
    output_options = output_options,
    params         = report_params,
    envir          = new.env()
  )
  # o pos-processamento nunca deve impedir a entrega do relatorio: se falhar,
  # o .docx fica como o pandoc o gerou (com os marcadores LANDSCAPE_SECTION_*
  # visiveis, sem a secao em paisagem) e o erro e' mostrado por inteiro
  tryCatch(
    apply_landscape_sections(out),
    error = function(e) warning(
      "apply_landscape_sections() falhou -- o .docx foi gerado SEM a secao em paisagem (os marcadores ",
      "LANDSCAPE_SECTION_START/END ficam visiveis no texto): ", conditionMessage(e), call. = FALSE
    )
  )
  invisible(out)
}

## Secoes em paisagem dentro do .docx (pos-processamento do ficheiro gerado)
##
## O template marca, em paragrafos proprios, onde comeca e onde acaba o
## troco em paisagem:
##   LANDSCAPE_SECTION_START   (fim da secao vertical anterior)
##   LANDSCAPE_SECTION_END     (fim da secao em paisagem)
## Cada marcador e' substituido por um paragrafo com quebra de secao
## (<w:sectPr>). Num .docx o sectPr de um paragrafo descreve a secao que
## TERMINA nesse paragrafo; o resto do documento fica com o sectPr final
## (o do reference_docx). Por isso a secao vertical clona esse sectPr final
## (mantem cabecalho/rodape/margens) e a paisagem clona-o trocando
## largura/altura e w:orient="landscape".
##
## Sem marcadores no documento, nao faz nada (todos os outros relatorios).
## Escreve bytes num ficheiro tentando varias formas -- no Windows, abrir
## document.xml em "wb" com file(path) chegou a falhar com "Invalid
## argument" (caso real, 2026-10) mesmo depois de o ler com sucesso. Tenta,
## por ordem: (1) ligacao explicita, (2) ficheiro temporario na raiz de
## tempdir() copiado por cima, (3) apagar e recriar. Se todas falharem,
## o erro inclui o motivo de cada tentativa.
write_bytes_robust <- function(bytes, path) {
  errs <- character()
  attempt <- function(label, f) {
    ok <- tryCatch({ f(); file.exists(path) && file.info(path)$size == length(bytes) },
                   error = function(e) { errs <<- c(errs, paste0(label, ": ", conditionMessage(e))); FALSE },
                   warning = function(w) { errs <<- c(errs, paste0(label, ": ", conditionMessage(w))); FALSE })
    isTRUE(ok)
  }
  if (attempt("file(wb)", function() { con <- file(path, "wb"); on.exit(close(con)); writeBin(bytes, con) })) return(invisible(TRUE))
  if (attempt("tempfile+copy", function() {
    tf <- tempfile(fileext = ".xml"); writeBin(bytes, tf)
    if (!file.copy(tf, path, overwrite = TRUE)) stop("file.copy devolveu FALSE")
    unlink(tf)
  })) return(invisible(TRUE))
  if (attempt("unlink+write", function() { unlink(path); writeBin(bytes, path) })) return(invisible(TRUE))
  stop("nao foi possivel escrever '", path, "' -- ", paste(errs, collapse = " | "))
}

apply_landscape_sections <- function(docx_path) {
  tmp <- tempfile("docx_landscape_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  utils::unzip(docx_path, exdir = tmp)
  doc_file <- file.path(tmp, "word", "document.xml")
  # ler/escrever como bytes UTF-8 -- readLines/writeLines em locale nao-UTF-8
  # (ex: "C") trocariam o nbsp por "<U+00A0>" e estragariam o XML
  xml <- rawToChar(readBin(doc_file, "raw", file.info(doc_file)$size))
  Encoding(xml) <- "UTF-8"

  m_start <- "LANDSCAPE_SECTION_START"
  m_end   <- "LANDSCAPE_SECTION_END"
  if (!grepl(m_start, xml, fixed = TRUE) && !grepl(m_end, xml, fixed = TRUE)) return(invisible(FALSE))

  # sectPr final do corpo (o ultimo do documento)
  all_sect <- regmatches(xml, gregexpr("(?s)<w:sectPr[ >].*?</w:sectPr>", xml, perl = TRUE))[[1]]
  if (length(all_sect) == 0) stop("apply_landscape_sections: sectPr final nao encontrado em ", docx_path)
  sect_portrait <- all_sect[length(all_sect)]

  # o pandoc reescreve os atributos de <w:pgSz> por outra ordem -- ler cada um
  attr_val <- function(tag, name) {
    m <- regmatches(tag, regexec(paste0(name, '="([0-9]+)"'), tag))[[1]]
    if (length(m) != 2) stop("apply_landscape_sections: atributo ", name, " nao encontrado em <w:pgSz>")
    m[2]
  }
  pg_tag <- regmatches(sect_portrait, regexpr("<w:pgSz [^>]*/>", sect_portrait))
  if (length(pg_tag) != 1) stop("apply_landscape_sections: <w:pgSz> nao encontrado no sectPr final")
  w <- attr_val(pg_tag, "w:w"); h <- attr_val(pg_tag, "w:h")
  # sem w:code (codigo de papel A4) -- nao se aplica a paisagem
  sect_landscape <- sub("<w:pgSz [^>]*/>",
                        sprintf('<w:pgSz w:w="%s" w:h="%s" w:orient="landscape"/>', h, w),
                        sect_portrait)

  esc_repl <- function(x) gsub("\\", "\\\\", x, fixed = TRUE) # backslashes no replacement do gsub
  break_par <- function(sect) paste0("<w:p><w:pPr>", sect, "</w:pPr></w:p>")
  # o paragrafo que contem o marcador (com ou sem run properties)
  swap <- function(xml, marker, replacement) {
    pat <- paste0("(?s)<w:p>(?:(?!</w:p>).)*?", marker, ".*?</w:p>")
    if (!grepl(marker, xml, fixed = TRUE)) return(xml)
    gsub(pat, replacement, xml, perl = TRUE)
  }
  xml <- swap(xml, m_start, esc_repl(break_par(sect_portrait)))
  xml <- swap(xml, m_end,   esc_repl(break_par(sect_landscape)))
  write_bytes_robust(charToRaw(enc2utf8(xml)), doc_file)

  old <- setwd(tmp); on.exit(setwd(old), add = TRUE)
  out_abs <- normalizePath(docx_path, winslash = "/", mustWork = TRUE)
  files <- list.files(tmp, recursive = TRUE, all.files = TRUE)
  # [Content_Types].xml tem de ir primeiro; zip::zip preserva a ordem dada
  files <- c(intersect("[Content_Types].xml", files), setdiff(files, "[Content_Types].xml"))
  unlink(out_abs)
  zip::zip(out_abs, files = files, mode = "mirror")
  invisible(TRUE)
}
