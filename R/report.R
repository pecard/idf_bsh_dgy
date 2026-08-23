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

  rmarkdown::render(
    input          = template,
    output_dir     = normalizePath(out_dir),
    output_file    = basename(output_file),
    output_options = output_options,
    params         = report_params,
    envir          = new.env()
  )
}
