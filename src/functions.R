# função pra criar diretório

dir_create <- function(dir_name) {
  if (!file.exists(dir_name)) {
    dir.create(dir_name)
    print("diretório criado")
  } else {
    print("diretório já existe")
  }
}

# função pra cortar camadas

crop_raster <- function(raster_list, shp, path) {
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

# função para splitar dataframes por espécies

data_by_species <- function(data, list_species, col_long = "longitude",
                            col_lat = "latitude", thin_dist = 25, path) {
  list_data <- list()
  list_data_thin <- list()
  for (i in seq_along(list_species)) {
    list_data[[i]] <- data %>%
      dplyr::filter(species == list_species[[i]]) %>%
      rename(latitude = decimalLatitude, longitude = decimalLongitude) %>%
      select(-inout)
    list_data_thin[[i]] <- thin_data(list_data[[i]], col_long, col_lat,
      thin_distance = thin_dist, save = T,
      name = paste0(path, "/", list_data[[i]]$species[1])
    )
  }
  result <- c(list_data, list_data_thin)
  return(result)
}
