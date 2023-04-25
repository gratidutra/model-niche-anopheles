# função pra criar diretório

dir_create <- function(dir_name) {
  if (!file.exists(dir_name)) {
    dir.create(dir_name)
    print(paste("diretório criado", dir_name))
  } else {
    print("diretório já existe")
  }
}

# função pra cortar camadas

crop_raster_cmpi5 <-
  function(raster_list, shp, path) {
    new_raster_list <- list()
    i <- 1
    while (i <= length(raster_list)) {
      new_raster_list[[i]] <-
        raster::crop(raster_list[[i]], shp)
      writeRaster(new_raster_list[[i]],
        paste0(path, "/bio", i, ".asc"),
        overwrite = T
      )
      i <- i + 1
    }
    return(new_raster_list)
  }

# função pra cortar camadas do cmip6

crop_raster_cmip6 <-
  function(spat_raster, shape, name_path) {
    brick_layer <-
      brick(spat_raster)

    croped_layer <-
      raster::crop(brick_layer, shape)

    dir_create(name_path)

    for (i in 1:19) {
      writeRaster(croped_layer[[i]],
        paste0(name_path, "/bio", i, ".asc"),
        overwrite = T
      )
    }
    return(croped_layer)
  }

# função para splitar dataframes por espécies

data_by_species <-
  function(data, list_species, col_long = "longitude",
           col_lat = "latitude", thin_dist = 25, path) {
    list_data <- list()
    list_data_thin <- list()

    for (i in seq_along(list_species)) {
      list_data[[i]] <-
        data %>%
        dplyr::filter(species == list_species[[i]])

      list_data_thin[[i]] <-
        thin_data(list_data[[i]], col_long, col_lat,
          thin_distance = thin_dist, save = T,
          name = paste0(path, "/", list_data[[i]]$species[1])
        )
    }
    result <- c(list_data_thin)
    return(result)
  }
