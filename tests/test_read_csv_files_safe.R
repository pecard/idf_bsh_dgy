##
## Teste com dados simulados para read_csv_files_safe(), R/read_utils.R
##
## Ao contrario dos outros read_*.R (que dependem de ficheiros reais numa
## pasta databases_dir), read_csv_files_safe() e' testavel com ficheiros CSV
## temporarios -- e' precisamente a funcao que existe para proteger
## read_scada_data()/read_heartbeats_data() de um ficheiro vazio/corrompido
## na pasta de dados brutos (bug real, 2026-09: 1 de 83 ficheiros SCADA so'
## com 1 coluna em vez de 13, rbindlist() falhava e perdia-se TODO o
## trabalho ja feito nas fontes lidas antes dele na mesma corrida).
##
## Correr: source("tests/test_read_csv_files_safe.R")
##
## Depende de: data.table
##

source("R/read_utils.R")

tmp_dir_test <- tempfile("read_csv_files_safe_test_")
dir.create(tmp_dir_test)

## 2 ficheiros "bons" (13 colunas, como um export SCADA real) + 1 ficheiro
## degenerado com so' 1 coluna (mesmo sintoma do bug real de 2026-09: 1 de
## 83 ficheiros SCADA lido com 1 coluna em vez de 13 -- ex: um download
## interrompido que so' guardou um cabecalho/mensagem de erro, sem virgulas) -

good_cols_test <- paste0("v", 1:13)

f1_test <- file.path(tmp_dir_test, "good_1.csv")
f2_test <- file.path(tmp_dir_test, "good_2.csv")
f_bad_test <- file.path(tmp_dir_test, "bad_1col.csv")

data.table::fwrite(as.list(setNames(1:13, good_cols_test)), f1_test)
data.table::fwrite(as.list(setNames(14:26, good_cols_test)), f2_test)
writeLines(c("erro_no_download", "sem dados"), f_bad_test) # sem virgulas -> fread() le isto como 1 coluna

cat("\n===== read_csv_files_safe() -- 2 ficheiros bons + 1 com 1 so' coluna =====\n")
result_test <- read_csv_files_safe(c(f1_test, f2_test, f_bad_test), header = TRUE)
print(result_test)

cat(sprintf(
  "Esperado: 2 linhas, 13 colunas (so' os 2 ficheiros bons) -- obtido: %d linha(s), %d coluna(s)\n",
  nrow(result_test), ncol(result_test)
))
cat(sprintf(
  "Mensagem acima devia identificar '%s' como ignorado -- confirmar a olho.\n",
  f_bad_test
))


## Caso-limite: todos os ficheiros bons (nenhum ignorado) -- sem mensagem
## de aviso, comportamento identico ao rbindlist(lapply(...)) anterior ----

cat("\n===== read_csv_files_safe() -- so' ficheiros bons (sem ficheiro mau) =====\n")
result_allgood_test <- read_csv_files_safe(c(f1_test, f2_test), header = TRUE)
print(result_allgood_test)
cat(sprintf(
  "Esperado: 2 linhas, 13 colunas, SEM mensagem de aviso -- obtido: %d linha(s), %d coluna(s)\n",
  nrow(result_allgood_test), ncol(result_allgood_test)
))

unlink(tmp_dir_test, recursive = TRUE)
