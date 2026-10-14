## Teste de apply_landscape_sections() (R/report.R) com um .Rmd minimo
source("R/report.R")
tmp_test <- tempfile("landscape_test_"); dir.create(tmp_test)
rmd_test <- file.path(tmp_test, "t.rmd")
writeLines(c(
  "---", "title: t", "output: word_document", "---", "",
  "Vertical page 1", "", "LANDSCAPE_SECTION_START", "",
  "Landscape table", "", "LANDSCAPE_SECTION_END", "", "Vertical page again"
), rmd_test)
out_test <- build_idf_report(file.path(tmp_test, "t.docx"), list(), template = rmd_test,
                             reference_docx = "inputs/Mod.001.05_template_documentos_gerais.docx")
ex_test <- tempfile(); utils::unzip(out_test, exdir = ex_test)
xml_test <- paste(readLines(file.path(ex_test, "word", "document.xml"), warn = FALSE), collapse = "")
sect_test <- regmatches(xml_test, gregexpr("<w:pgSz [^>]*/>", xml_test))[[1]]
stopifnot(!grepl("LANDSCAPE_SECTION", xml_test))
stopifnot(length(sect_test) == 3)
stopifnot(grepl('w:orient="landscape"', sect_test[2]), !grepl("landscape", sect_test[1]), !grepl("landscape", sect_test[3]))
stopifnot(sum(grepl("headerReference", regmatches(xml_test, gregexpr("<w:sectPr[ >].*?</w:sectPr>", xml_test))[[1]])) == 3)
cat("test_landscape_section OK\n")
