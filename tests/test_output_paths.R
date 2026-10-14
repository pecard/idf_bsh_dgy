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

# make_run_stamp(): AAAAMMDD_HHMM
stamp_test <- make_run_stamp(as.POSIXct("2026-10-14 15:32:10", tz = "UTC"))
cat(sprintf("run stamp: %s (esperado 20261014_1532) -- %s\n", stamp_test, if (identical(stamp_test, "20261014_1532")) "OK" else "FALHOU"))

# mesmo incidente, corridas a horas diferentes -> pastas e ficheiros distintos
tag2_test <- make_incident_tag("ZRF", "T23", as.Date("2026-06-03"))
f1_test <- incident_output_folder("outputs", "20261014_1532", tag2_test)
f2_test <- incident_output_folder("outputs", "20261014_1610", tag2_test)
cat(sprintf("pastas distintas para o mesmo incidente em horas distintas: %s (esperado TRUE)\n", !identical(f1_test, f2_test)))
cat(sprintf("nome de ficheiro com hora: %s\n", out_name("Incident_Report", "docx", tag2_test, "20261014_1532")))

# write_run_info(): copia do settings + run_info
dir_test <- tempfile("runinfo_test_"); dir.create(dir_test)
settings_test <- file.path(dir_test, "userSettings_test.R"); writeLines("x <- 1", settings_test)
out_test <- write_run_info(dir_test, tag2_test, "20261014_1532", "userSettings_test.R", settings_test)
cat(sprintf("copia do settings e run_info criados: %s (esperado TRUE)\n", all(file.exists(out_test))))
cat(sprintf("run_info menciona settings file: %s (esperado TRUE)\n", any(grepl("userSettings_test.R", readLines(out_test[2]), fixed = TRUE))))

# write_run_info() com settings do incidente (sobrepostos aos do parque)
inc_settings_test <- file.path(dir_test, "userSettings_incident_test.R"); writeLines("y <- 2", inc_settings_test)
out2_test <- write_run_info(dir_test, tag2_test, "20261014_1610", "userSettings_test.R", settings_test,
                            incident_settings_file = "userSettings_incident_test.R", incident_settings_path = inc_settings_test)
cat(sprintf("copia do settings do incidente criada: %s (esperado TRUE)\n",
            file.exists(file.path(dir_test, "settings_incident_ZRF_T23_20260603_20261014_1610.R"))))
cat(sprintf("run_info menciona settings do incidente: %s (esperado TRUE)\n",
            any(grepl("userSettings_incident_test.R", readLines(out2_test[length(out2_test)]), fixed = TRUE))))
