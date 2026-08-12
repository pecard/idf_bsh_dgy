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

build_idf_report <- function(output_file, report_params,
                             template = "report/report_template.rmd") {

  rmarkdown::render(
    input     = template,
    output_file = output_file,
    params    = report_params,
    envir     = new.env()
  )
}
