# Compara as métricas de várias corridas do pipeline GEE (mangais_gnb_pipeline.js)
# a partir dos CSV mangal_gnb_metrics_<ano>_<run_id>.csv exportados para o Drive.
#
# Saídas:
#  - mangal_gnb_runs_comparison.csv : uma linha por corrida (OA, kappa, F1/PA/UA por classe, configuração)
#  - matriz de confusão (contagens médias) com PA e UA nas margens, para uma corrida à escolha

# ============================ CONFIGURAÇÃO ============================
dir_csv   <- "G:/O meu disco/Bissau/Mangais/Restauro_Mangais/MangaisGEEngine"
classes   <- 1:4
run_alvo  <- NULL   # run_id da corrida para a matriz de confusão; NULL = a mais recente
ano_alvo  <- NULL   # ano da corrida (só necessário se o mesmo run_id tiver vários anos)
# ======================================================================

ficheiros <- list.files(dir_csv, pattern = "^mangal_gnb_metrics_.*\\.csv$", full.names = TRUE)
if (!length(ficheiros)) stop("Nenhum mangal_gnb_metrics_*.csv em ", dir_csv)

lista <- lapply(ficheiros, read.csv, check.names = FALSE, stringsAsFactors = FALSE)
cols  <- Reduce(union, lapply(lista, names))
lista <- lapply(lista, function(d) { d[setdiff(cols, names(d))] <- NA; d[cols] })
m <- do.call(rbind, lista)
m <- m[order(m$year, m$run_id), ]

# Coluna com NA se não existir (corridas antigas com menos campos)
col <- function(nome) if (nome %in% names(m)) m[[nome]] else rep(NA, nrow(m))

cmp <- data.frame(
  run_id          = col("run_id"),
  year            = col("year"),
  sensors         = col("sensors"),
  n_images        = col("n_images"),
  cloud_cover_max = col("cloud_cover_max"),
  extra_indices   = col("extra_indices"),
  block_deg       = col("block_deg"),
  trees_cv        = col("trees_cv"),
  cv_repeats      = col("cv_repeats"),
  n_points_valid  = col("n_points_valid"),
  oa              = col("cv_oa_mean"),
  kappa           = col("cv_kappa_mean"),
  oob_error       = col("oob_error")
)
for (k in classes) cmp[[paste0("f1_", k)]] <- col(sprintf("cv_f1_%d_mean", k))
cmp$pa_mangal <- col("cv_pa_1_mean")
cmp$ua_mangal <- col("cv_ua_1_mean")

out_cmp <- file.path(dir_csv, "mangal_gnb_runs_comparison.csv")
write.csv(cmp, out_cmp, row.names = FALSE)
message("Comparação escrita em ", out_cmp)
print(cmp, digits = 3)

# --- Matriz de confusão de uma corrida ---
sel <- rep(TRUE, nrow(m))
if (!is.null(run_alvo)) sel <- sel & m$run_id == run_alvo
if (!is.null(ano_alvo)) sel <- sel & m$year == ano_alvo
if (!any(sel)) stop("Corrida não encontrada: ", run_alvo, " / ", ano_alvo)
linha <- m[tail(which(sel), 1), ]
message("Matriz de confusão: ", linha$run_id, " (", linha$year, ")")

# linhas = referência (groundtruthing), colunas = mapa
cm <- sapply(classes, function(c) {
  sapply(classes, function(r) linha[[sprintf("cv_cm_%d_%d_mean", r, c)]])
})
dimnames(cm) <- list(reference = paste0("class_", classes), map = paste0("class_", classes))

tot <- rbind(cm, user_accuracy = diag(cm) / colSums(cm))
tot <- cbind(tot, producer_accuracy = c(diag(cm) / rowSums(cm), NA))
print(round(tot, 3))

out_cm <- file.path(dir_csv, sprintf("mangal_gnb_confusion_%s_%s.csv", linha$year, linha$run_id))
write.csv(round(tot, 4), out_cm)
message("Matriz escrita em ", out_cm)
