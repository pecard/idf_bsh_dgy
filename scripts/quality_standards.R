quality_standards <- function(username, output_folder, packages) {
  # Save R version and package versions in a txt file to ensure script reproducibility and avoid version issues
  sink(paste0(output_folder, "\\R_and_package_versions.txt"))
  cat(paste0('Username: ', username, '\n'))
  print(sessionInfo(package = NULL))
  sink()
  
  # Save citations
  citations <- packages %>%
    map(citation) 
  sink(paste0(output_folder, "\\R_and_package_citations.txt"))
  print(citations)
  sink()
}