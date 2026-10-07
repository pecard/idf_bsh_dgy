##
## Teste de R/output_paths.R -- nomes de pasta/ficheiro por incidente
## Correr: source("tests/test_output_paths.R")
##

source("R/output_paths.R")

tag_test <- make_incident_tag("ZRF", "T94", as.Date("2026-10-01"))
cat(sprintf("incident_tag: %s (esperado ZRF_T94_20261001) -- %s\n", tag_test,
            if (identical(tag_test, "ZRF_T94_20261001")) "OK" else "FALHOU"))

folder_test <- incident_output_folder("outputs", "20261007", tag_test)
cat(sprintf("pasta: %s (esperado outputs/20261007_ZRF_T94_20261001) -- %s\n", folder_test,
            if (identical(folder_test, "outputs/20261007_ZRF_T94_20261001")) "OK" else "FALHOU"))

name_test <- out_name("coverage_3d_summary", "xlsx", tag_test, "20261007")
cat(sprintf("ficheiro: %s -- %s\n", name_test,
            if (identical(name_test, "coverage_3d_summary_ZRF_T94_20261001_20261007.xlsx")) "OK" else "FALHOU"))

# extensao com ponto inicial tolerada
cat(sprintf("extensao '.docx' tolerada: %s (esperado TRUE)\n",
            identical(out_name("Incident_Report", ".docx", tag_test, "20261007"),
                      "Incident_Report_ZRF_T94_20261001_20261007.docx")))

# 2 incidentes do mesmo parque, mesmo dia -> pastas diferentes
other_tag_test <- make_incident_tag("ZRF", "T23", as.Date("2026-05-03"))
cat(sprintf("pastas distintas para incidentes distintos no mesmo dia: %s (esperado TRUE)\n",
            !identical(incident_output_folder("outputs", "20261007", tag_test),
                       incident_output_folder("outputs", "20261007", other_tag_test))))
