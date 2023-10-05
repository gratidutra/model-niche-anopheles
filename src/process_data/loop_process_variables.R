source("src/functions.R")
library(stringr)
library(ellipsenm)
library(ntbox)
library(raster)

list_file_current <- 
  list.files(
  path = paste0(getwd(), "/data/bioclim_layer/CMIP6/current"),
  pattern = "\\.asc$", full.names = T
)

current_neotropic_layer <- 
  stack(list_file_current)

species_list <-
  c(
    "Anopheles_albimanus", "Anopheles_albitarsis", "Anopheles_apicimacula",
    "Anopheles_aquasalis", "Anopheles_argyritarsis", "Anopheles_braziliensis",
    # "Anopheles_cruzii",
    "Anopheles_eiseni", "Anopheles_evansae", "Anopheles_intermedius",
    "Anopheles_mediopunctatus",
    # "Anopheles_nuneztovari",
    "Anopheles_oswaldoi", "Anopheles_peryassui", "Anopheles_pseudopunctipennis",
    "Anopheles_punctimacula", "Anopheles_rangeli", "Anopheles_strodei"
    # "Anopheles_triannulatos")
  )
sp_name<- 'Anopheles_albimanus'
for (i in seq_along(species_list)) {
  sp_name <- species_list[[i]]
  path <- paste0("data/workflow_maxent/", sp_name)

  # sp_name <- sp_data_list[[i]]$species[1]

  unlink(paste0(path, "/G_Variables"), recursive = TRUE)
  unlink(paste0(path, "/pcas"), recursive = TRUE)

  dir_create(path)
  dir_create(paste0(path, "/outputs/"))
  dir_create(paste0(path, "/Model_calibration"))
  dir_create(paste0(path, "/Model_calibration/M_variables"))
  dir_create(paste0(path, "/Model_calibration/M_variables/Set_1"))
  dir_create(paste0(path, "/Model_calibration/M_variables/Set_2"))
  dir_create(paste0(path, "/pcas"))

  # kuenm_occsplit(
  #   occ = sp_data_list[[i]],
  #   train.proportion = 0.7,
  #   method = "random", save = T,
  #   name = paste0(path, "/", sp_name)
  # )

  sp_data <- read.csv(paste0(path, "/", sp_name, "_joint.csv"))

  # Data Exploratory -----------------------------------------------------------------

  ## only temperature variables

  explore_espace(
    data = sp_data, species = "species", longitude = "longitude",
    latitude = "latitude", raster_layers = current_neotropic_layer[[1:11]], save = T,
    name = paste0(path, "/outputs/Temperature_variables.pdf"), open = F
  )

  ## only precipitation variables

  explore_espace(
    data = sp_data, species = "species", longitude = "longitude",
    latitude = "latitude", raster_layers = current_neotropic_layer[[12:19]], save = T,
    name = paste0(path, "/outputs/Precipitation_variables.pdf"), open = F
  )

  # select only current low corr variable

  bios_lcor <- c(1, 2, 3, 4, 12, 15, 18)

  file.copy(
    from =
      paste0("data/current_layer/bio", bios_lcor, ".asc"),
    to =
      paste0(path, "/Model_calibration/M_variables/Set_1/bio", bios_lcor, ".asc")
  )

  # G_Variables SET 1 -------------------------------------------------------------

  dir_create(paste0(path, "/G_Variables"))
  dir_create(paste0(path, "/G_Variables/Set_1"))
  dir_create(paste0(path, "/G_Variables/Set_1/Current"))

  layers_list <-
    c(
      "current", "future_layer_can_126_60", "future_layer_can_585_60",
      "future_layer_mc_126_60", "future_layer_mc_585_60"
    )

  for (layer in seq_along(layers_list)) {
    dir_create(paste0(path, "/G_Variables/Set_1/", layers_list[[layer]]))

    file.copy(
      from = paste0(
        "data/bioclim_layer/CMIP6/", layers_list[[layer]], "/bio", bios_lcor, ".asc"
      ),
      to = paste0(
        path, "/G_Variables/Set_1/", layers_list[[layer]], "/bio", bios_lcor, ".asc"
      )
    )
  }

  # G_Variables SET 1 -------------------------------------------------------------
  
  dir_create(paste0(path, "/G_Variables/Set_2"))

  for (layer in seq_along(layers_list)) {
    do_pca(
      set = 2, time = layers_list[[layer]],
      raster_to_do_pca = paste0("data/bioclim_layer/CMIP6/", layers_list[[layer]]),
      sv_dir = paste0(path, "/pcas/", layers_list[[layer]]),
      sv_proj_dir = paste0(path, "/pcas/pca_proj_", layers_list[[layer]]),
      m_dir = paste0(path, "/Model_calibration/M_variables/Set_2"),
      g_dir = paste0(path, "/G_Variables")
    )
  }
}