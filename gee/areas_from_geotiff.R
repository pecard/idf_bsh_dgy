# Áreas de mangal (e restantes classes) por região, AP e total, a partir do
# GeoTIFF exportado pelo GEE (mangais_gnb_pipeline.js).
#
# Classes: 1 mangal | 2 agua | 3 floresta | 4 aberto
# Uso: ajustar os caminhos em "CONFIGURAÇÃO" e correr o script.

pkgs <- c("terra", "sf", "exactextractr")
falta <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(falta)) install.packages(falta)
invisible(lapply(pkgs, library, character.only = TRUE))

# ============================ CONFIGURAÇÃO ============================
ano        <- 2025
raster_tif <- sprintf("mangal_gnb_%d.tif", ano)   # exportado pelo GEE (EPSG:32628, 30 m)
camadas <- list(                                   # nível = ficheiro vetorial + coluna com o nome
  admin1         = list(ficheiro = "GNB_admin1.shp",         coluna = "name_1"),
  protected_area = list(ficheiro = "GNB_AP_Mangal_UTM28N.shp", coluna = "NAME")
)
area_mangais_shp <- "Area_Mangais_GNB_Rev2.shp"    # extensão total do mapa
saida_csv  <- sprintf("mangal_gnb_areas_%d.csv", ano)
classes    <- 1:4
# ======================================================================

r <- terra::rast(raster_tif)
if (!terra::same.crs(r, "EPSG:32628")) {
  warning("O raster não está em EPSG:32628; as áreas por pixel podem ficar distorcidas.")
}
# Área de um pixel em km2 (raster projetado, resolução constante)
km2_pixel <- prod(terra::res(r)) / 1e6

# Soma da fração de cobertura de cada classe dentro de cada polígono
area_por_classe <- function(poligonos, coluna, nivel) {
  poligonos <- sf::st_transform(sf::st_make_valid(poligonos), terra::crs(r))
  tabela <- exactextractr::exact_extract(
    r, poligonos,
    fun = function(valores, cobertura) {
      vapply(classes, function(k) sum(cobertura[valores %in% k], na.rm = TRUE), numeric(1))
    },
    progress = FALSE
  )
  tabela <- as.data.frame(t(tabela))
  names(tabela) <- paste0("km2_", classes)
  tabela[] <- lapply(tabela, `*`, km2_pixel)
  data.frame(year = ano, level = nivel, name = as.character(poligonos[[coluna]]), tabela)
}

# Total (união das features da cartografia de referência)
total <- sf::st_sf(name = "total", geometry = sf::st_union(sf::st_read(area_mangais_shp, quiet = TRUE)))
res <- list(area_por_classe(total, "name", "total"))

for (nivel in names(camadas)) {
  v <- sf::st_read(camadas[[nivel]]$ficheiro, quiet = TRUE)
  res[[length(res) + 1]] <- area_por_classe(v, camadas[[nivel]]$coluna, nivel)
}

areas <- do.call(rbind, res)
write.csv(areas, saida_csv, row.names = FALSE)
message("Áreas escritas em ", saida_csv)
print(areas)
