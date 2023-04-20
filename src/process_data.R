library(tidyverse)
library(rgbif)
library(leaflet)
library(Rocc)
library(rgdal)
library(rgeos)
library(raster)
library(sp)
library(maptools)
library(tmap)
library(sf)
library(ellipsenm)
library(kuenm)
library(ntbox)

source("src/functions.R")

df <-
  read.table("data/raw/all_anopheles.txt", header = T) %>%
  rename(
    species = Especie,
    decimalLatitude = Latitude,
    decimalLongitude = longitude
  ) %>%
  mutate(species = gsub("_", " ", species))

splist <-
  levels(as.factor(df$species))

keys <-
  lapply(
    splist,
    function(x) name_suggest(x)$data$key[1]
  )

# problema no limite de requisições

occ_anopheles <- occ_download(
  pred_in("taxonKey", keys),
  pred("hasCoordinate", TRUE),
  format = "SIMPLE_CSV",
  user = "gratidutra", pwd = "iNm!cD!U6@3LhYH",
  email = "gratirodrigues.gdr@gmail.com"
)

all_species <- 
  occ_download_get(key = '0155603-230224095556074', overwrite = TRUE) %>% 
  occ_download_import

all_species_gbif <-
  all_species %>%
  dplyr::select(species, decimalLatitude, decimalLongitude) %>%
  drop_na()


# trocar para variavel de ambiente as credenciais

# Species Link ------------------------------------------------------------

all_species_splink_raw <-
  rspeciesLink(
    filename = "all_species_splink",
    species = splist,
    Coordinates = "Yes",
    CoordinatesQuality = "Good"
  )

all_species_splink <- 
  all_species_splink_raw %>%
  rename(species = scientificName) %>%
  dplyr::select(species, decimalLatitude, decimalLongitude) %>%
  mutate(
    decimalLatitude = as.numeric(decimalLatitude),
    decimalLongitude = as.numeric(decimalLongitude)
  )

# Unindo e tratando o df final--------------------------------------------------

anopheles_df <-
  all_species_gbif %>%
  bind_rows(all_species_splink, df)

# tratamento da escrita das espécies

levels(as.factor(anopheles_df$species))

anopheles_df <- anopheles_df %>%
  mutate(species = case_when(
    species == "Anopheles albitarsis?" ~ "Anopheles albitarsis",
    species == "Anopheles oswaldoi?" ~ "Anopheles oswaldoi",
    species == "Anopheles triannulatus s.l." ~ "Anopheles triannulatus",
    species == "Anopheles albimanus section" ~ "Anopheles albimanus",
    species == "Anopheles albitarsis s.l." ~ "Anopheles albitarsis",
    species == "Anopheles aquasalis?" ~ "Anopheles aquasalis",
    species == "Anopheles argyritarsis section" ~ "Anopheles argyritarsis",
    species == "Anopheles fluminensis *" ~ "Anopheles fluminensis",
    species == "Anopheles mediopunctatus *" ~ "Anopheles mediopunctatus",
    species == "Anopheles rangeli?" ~ "Anopheles rangeli",
    TRUE ~ species
  ))

levels(as.factor(anopheles_df$species))

# removendo as duplicatas

anopheles_processed1 <-
  anopheles_df[
    !duplicated(paste(
      anopheles_df$species,
      anopheles_df$decimalLongitude,
      anopheles_df$decimalLatitude
    )),
  ]

dir_create("data/processed")

# arrumando pontos que deram ruim no processamento

anopheles_processed2 <-
  anopheles_processed1 %>%
  mutate(decimalLongitude = case_when(
    decimalLongitude == -546264.0000 ~ -54.6264,
    TRUE ~ decimalLongitude
  ))

# shapefile

neotropic <-
  readOGR(
    dsn = ("data/raw/raster"),
    layer = "Neotropic",
    verbose = FALSE
  )

# identificando pontos fora do shape

anopheles_processed2["inout"] <- over(
  SpatialPoints(anopheles_processed2[
    , c("decimalLongitude", "decimalLatitude")
  ], proj4string = CRS(projection(neotropic))),
  as(neotropic, "SpatialPolygons")
)

# dropando pontos fora do shape
species_with_100 <- 
  anopheles_processed2 %>% 
  group_by(species) %>% 
  count() %>% 
  filter(n >= 100) 

anopheles_processed3 <-
  anopheles_processed2 %>%
  dplyr::filter(species %in% species_with_100$species) %>% 
  rename(latitude = decimalLatitude, longitude = decimalLongitude) %>%
  drop_na(.) %>% 
  dplyr::select(species, longitude, latitude)
  
# salvando csv

write.csv(anopheles_processed3, "data/processed/anopheles_processed.csv")

# df para o kmeans

kmeans <-
  anopheles_processed3 %>%
  group_by(latitude, longitude) %>%
  summarise(rich = n_distinct(species))

write.csv(kmeans, "data/processed/kmeans.csv")

# criando objeto pro plot via tmap e plotando

anopheles_points_plot <-
  anopheles_processed3 %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_cast("POINT")

tm_shape(neotropic) +
  tm_polygons(border.alpha = 0.3) +
  tm_shape(anopheles_points_plot) +
  tm_dots(size = 0.05)

# download camadas present e recorte

current_layer <- 
  geodata::worldclim_global(
  "bio",
  res = 10,
  path = getwd()
)
class(current_layer)

current <- 
  crop_raster_cmip6(
    current_layer, neotropic,
    "data/current_layer")

# Future ------------------------------------------------------------------
future_layer_mc_126_60 <-
  geodata::cmip6_world(model='MIROC6', 
                       ssp='126', 
                       time = "2041-2060",
                       var = 'bioc',
                       res=10, path = getwd())

mc_126 <- 
  crop_raster_cmip6(
  future_layer_mc_126_60, neotropic, 
  "data/future_layer_mc_126_60")

future_layer_mc_585_60 <-
  geodata::cmip6_world(model='MIROC6', 
                       ssp='585', 
                       time = "2041-2060",
                       var = 'bioc',
                       res=10, path = getwd()
                       )

mc_585 <- crop_raster_cmip6(
  future_layer_mc_585_60, neotropic, 
  "data/future_layer_mc_585_60")

future_layer_can_126_60 <-
  geodata::cmip6_world(model='CanESM5', 
                       ssp='126', 
                       time = "2041-2060",
                       var = 'bioc',
                       res=10, path = getwd())

can_126 <- 
  crop_raster_cmip6(
  future_layer_can_126_60, neotropic, 
  "data/future_layer_can_126_60")

future_layer_can_585_60 <-
  geodata::cmip6_world(model='CanESM5', 
                       ssp='585', 
                       time = "2041-2060",
                       var = 'bioc',
                       res=10, path = getwd()
  )

can_585 <- 
  crop_raster_cmip6(
  future_layer_can_585_60, neotropic, 
  "data/future_layer_can_585_60")

# future_layer_mc_126_60 <- 
#   brick(future_layer_mc_126_60)
# 
# future_neotropic_mc_126_60 <-
#   raster::crop(future_layer_mc_126_60, neotropic)
# 
# dir_create('data/future_neotropic_mc_126_60')
# 
# for (i in 1:19) {
#   writeRaster(future_neotropic_mc_126_60[[i]],
#     paste0("data/future_neotropic_mc_126_60/bio", i, ".asc"),
#     overwrite = T
#   )
# }

# Crop_layers CMIP5-------------------------------------------------------------

dir_create("data/workflow_maxent")
dir_create("data/bioclim_neotropic")

raster_neotropic_list <-
  crop_raster(current_layer@layers, neotropic, 
              "data/bioclim_neotropic")

current_neotropic_layer <- stack(raster_neotropic_list)

# buffer & split ---------------------------------------------------------

dir_create("data/processed/data_by_specie")
dir_create("data/workflow_maxent/an_albimanus")

sp_data_list <-
  data_by_species(anopheles_processed3, 
                  species_with_100$species, 
                  path = "data/processed/data_by_specie")

# split para uma espécie

kuenm_occsplit(
  occ = sp_data_list[[40]], 
  train.proportion = 0.7,
  method = "random", save = T,
  name = "data/workflow_maxent/an_albimanus/an_albimanus"
)

# Data Exploratory -----------------------------------------------------------------

# corr_table --------------------------------------------------------------
# an_alb_df <- as.data.frame(sp_data_list[[1]])
# 
# coordinates(an_alb_df)= ~longitude+latitude
# 
# rasValue=raster::extract(current, sp_data_list[[1]] %>% 
#                            dplyr::select(longitude, latitude))
# 
# View(rasValue %>% 
#   corrr::correlate() %>% 
#   filter())

## only temperature variables

explore_espace(
  data =  sp_data_list[[1]], species = "species", longitude = "longitude",
  latitude = "latitude", raster_layers = current[[1:11]], save = T,
  name = "outputs/an_albimanus/Temperature_variables.pdf"
)

## only precipitation variables

explore_espace(
  data = sp_data_list[[1]], species = "species", longitude = "longitude",
  latitude = "latitude", raster_layers = current[[12:19]], save = T,
  name = "outputs/an_albimanus/Precipitation_variables.pdf"
)

# low corr

lcor <- c(1, 2, 3, 4, 12, 15, 18)

# exploring variable correlation in one plot for all

jpeg("outputs/an_albimanus/corrplot_bioclim.jpg",
     width = 120,
     height = 120, units = "mm", res = 600
)
par(cex = 0.8)
vcor <- 
  variable_correlation(
    current_neotropic_layer,
    save = T, name = "outputs/an_albimanus/correlation_bioclim",
    corrplot = T, magnify_to = 3
)

dev.off()

# variables selected were bio: 1, 2, 12, 14, 15

dir_create("data/workflow_maxent/an_albimanus/Model_calibration/Raw_variables_bio_lcor")
dir_create("data/workflow_maxent/an_albimanus/Model_calibration/M_variables")

file.copy(
  from = paste0("data/bioclim_neotropic/bio", lcor, ".asc"),
  to = paste0("data/workflow_maxent/an_albimanus/Model_calibration/Raw_variables_bio_lcor/bio", lcor, ".asc")
)

# vs <-
#   kuenm_varcomb(
#     var.dir = "data/workflow_maxent/Model_calibration/Raw_variables_bio_lcor",
#     out.dir = "data/workflow_maxent/Model_calibration/M_variables",
#     min.number = 7, in.format = "ascii", out.format = "ascii"
#   )


#------------------Principal component analysis and projections-----------------
# PCA and projections
dir.create("data/workflow_maxent/an_albimanus/pcas")
dir.create("data/workflow_maxent/an_albimanus/pcas/pca_referenceLayers")
dir.create("data/workflow_maxent/an_albimanus/pcas/pca_proj")

s1 <- 
  spca(
    layers_stack = current_neotropic_layer, layers_to_proj = current_neotropic_layer,
    sv_dir = "data/workflow_maxent/an_albimanus/pcas/pca_referenceLayers", 
    layers_format = ".asc",
    sv_proj_dir = "data/workflow_maxent/an_albimanus/pcas/pca_proj"
  )

# Read the pca object (output from ntbox function)
lf <-
  list.files(
    path = paste0("data/workflow_maxent/an_albimanus/pcas/pca_referenceLayers"),
    pattern = "\\.rds$", full.names = TRUE
  )

f1 <- 
  readRDS(
    lf
  )

# Summary

f2 <- 
  summary(f1)

# The scree plot
dir_create('outputs')
dir_create('outputs/an_albimanus')

png(
  filename = "outputs/an_albimanus/screeplot.png",
  width = 1200 * 1.3, height = 1200 * 1.3, res = 300
)
plot(f2$importance[3, 1:5] * 100,
     xlab = "Principal component",
     ylab = "Percentage of variance explained", ylim = c(0, 100),
     type = "b", frame.plot = T, cex = 1.5
)
points(f2$importance[2, 1:5] * 100, pch = 17, cex = 1.5)
lines(f2$importance[2, 1:5] * 100, lty = 2, lwd = 1.5)
legend(
  x = 3.5, y = 60, legend = c("Cumulative", "Non-cumulative"),
  lty = c(1, 2), pch = c(21, 17), bty = "n", cex = 0.85, pt.bg = "white"
)

dev.off()

# PCs used were pc: 1, 2, 3, 4, 
dir_create("data/workflow_maxent/an_albimanus/Model_calibration")
dir_create("data/workflow_maxent/an_albimanus/Model_calibration/PCs_M")

nums <- 1:4

file.copy(
  from = paste0("data/workflow_maxent/an_albimanus/pcas/pca_referenceLayers/PC0", nums, ".asc"),
  to = paste0("data/workflow_maxent/an_albimanus/Model_calibration/PCs_M/PC0", nums, ".asc")
)

dir_create("data/workflow_maxent/an_albimanus/G_Variables")
dir_create("data/workflow_maxent/an_albimanus/G_Variables/Set_1")
dir_create("data/workflow_maxent/an_albimanus/G_Variables/Set_1/Current")

# Aqui da para testar o var comb 

file.copy(
  from = paste0(
    "data/workflow_maxent/an_albimanus/Model_calibration/PCs_M/PC0",
    nums,
    ".asc"
  ),
  to = paste0(
    "data/workflow_maxent/an_albimanus/G_Variables/Set_1/Current/PC0",
    nums,
    ".asc"
  )
)

