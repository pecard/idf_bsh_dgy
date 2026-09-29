# Idade dos mangais a partir de uma série de mapas (GeoTIFF exportados pelo GEE).
#
# Idade = anos desde a primeira cena da corrida ininterrupta de "mangal" que
# termina na cena mais recente (contagem inclusiva: presente só na última cena = 1).
# A idade é um MÍNIMO: pixéis já presentes na primeira cena são censurados à esquerda
# (idade real >= valor).

library(terra)

# ============================ CONFIGURAÇÃO ============================
wdtif <- "G:/O meu disco/Bissau/Mangais/Restauro_Mangais/MangaisGEEngine"

# Um ano por ficheiro; o ano define a idade, por isso não depende da ordem manual
cenas <- data.frame(
  year = c(2014, 2016, 2018, 2020, 2022, 2024, 2025),
  file = c(
    "mangais2014_20240410_R2.tif",
    "mangal_gnb_2016.tif",
    "mangal_gnb_2018.tif",
    "mangal_gnb_2020.tif",
    "mangal_gnb_2022_vf2.tif",
    "mangal_gnb_2024_vf.tif",
    "mangal_gnb_2025_20260105_1.tif"
  )
)
classe_mangal     <- 1
preencher_lacunas <- FALSE   # TRUE: um "não mangal" isolado entre dois "mangal" conta como mangal
sufixo            <- "v2"    # muda por corrida para não sobrescrever resultados anteriores
# ======================================================================

terraOptions(memfrac = 0.8, progress = 1)

cenas <- cenas[order(cenas$year), ]
ano_ref <- max(cenas$year)
caminhos <- file.path(wdtif, cenas$file)
if (!all(file.exists(caminhos))) {
  stop("Ficheiros em falta: ", paste(cenas$file[!file.exists(caminhos)], collapse = ", "))
}

# --- 1. Ler e alinhar todas as cenas com a primeira como referência ---
lista <- lapply(caminhos, rast)
ref <- lista[[1]]
if (terra::is.lonlat(ref)) warning("Raster em graus: as áreas por pixel não são constantes.")
lista <- lapply(seq_along(lista), function(i) {
  x <- lista[[i]]
  if (!terra::compareGeom(ref, x, stopOnError = FALSE)) {
    message("Cena ", cenas$year[i], " reamostrada para a grelha de referência (vizinho mais próximo).")
    x <- terra::resample(x, ref, method = "near")
  }
  x
})
r <- rast(lista)
names(r) <- paste0("y", cenas$year)

# --- 2. Presença/ausência (NA -> 0) ---
p <- terra::subst(r == classe_mangal, NA, 0)

# Opcional: um 0 isolado (uma só cena) entre dois 1 deixa de quebrar a corrida.
# Reduz o efeito do ruído de classificação entre anos.
if (preencher_lacunas && nlyr(p) > 2) {
  q <- p
  for (i in 2:(nlyr(p) - 1)) q[[i]] <- p[[i]] | (p[[i - 1]] & p[[i + 1]])
  p <- q
}

# --- 3. N.º de cenas consecutivas até à cena mais recente ---
# Vetorizado (sem função R por pixel): a corrida acumulada é o produto cumulativo,
# a soma das corridas é o n.º de cenas consecutivas.
pr <- p[[nlyr(p):1]]                 # mais recente primeiro
corrida <- pr[[1]]
n_cenas <- corrida
for (i in seq_len(nlyr(pr))[-1]) {
  corrida <- corrida * pr[[i]]
  n_cenas <- n_cenas + corrida
}
out_n <- file.path(wdtif, sprintf("mangal_age_scenes_%d_%d_%s.tif", min(cenas$year), ano_ref, sufixo))
n_cenas <- writeRaster(n_cenas, out_n, overwrite = TRUE, datatype = "INT1U")

# --- 4. N.º de cenas -> idade em anos, derivado dos anos das cenas ---
# n = 1 -> ano_ref, n = 2 -> cena anterior, ...; sem valores escritos à mão.
primeiro_ano <- rev(cenas$year)
idade_min    <- ano_ref - primeiro_ano + 1
reclass      <- cbind(from = 0:nrow(cenas), to = c(0, idade_min))
print(reclass)

out_idade <- file.path(wdtif, sprintf("idade_mangais%d_%d_%s.tif", min(cenas$year), ano_ref, sufixo))
idade <- terra::classify(n_cenas, rcl = reclass, others = NA,
                         filename = out_idade, overwrite = TRUE, datatype = "INT2U")

# --- 5. Áreas por classe de idade ---
km2_pixel <- prod(terra::res(idade)) / 1e6
tab <- as.data.frame(terra::freq(idade))
tab <- tab[!is.na(tab$value) & tab$value > 0, c("value", "count")]
tab$area_km2 <- tab$count * km2_pixel
tab$pct <- round(100 * tab$area_km2 / sum(tab$area_km2), 3)
tab$left_censored <- tab$value == max(idade_min)   # presente desde a 1.ª cena: idade real >= valor
names(tab)[1:2] <- c("age_years", "n_pixels")

out_csv <- file.path(wdtif, sprintf("mangal_age_areas_%s.csv", sufixo))
write.csv(tab, out_csv, row.names = FALSE)
message("Tabela de áreas escrita em ", out_csv)
print(tab)
