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
  apply_landscape_sections(out)
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
  writeBin(charToRaw(enc2utf8(xml)), doc_file)

  old <- setwd(tmp); on.exit(setwd(old), add = TRUE)
  out_abs <- normalizePath(docx_path, winslash = "/", mustWork = TRUE)
  files <- list.files(tmp, recursive = TRUE, all.files = TRUE)
  # [Content_Types].xml tem de ir primeiro; zip::zip preserva a ordem dada
  files <- c(intersect("[Content_Types].xml", files), setdiff(files, "[Content_Types].xml"))
  unlink(out_abs)
  zip::zip(out_abs, files = files, mode = "mirror")
  invisible(TRUE)
}
