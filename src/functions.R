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

do_pca <-
  function(set, raster_to_do_pca, sv_dir, sv_proj_dir, time = "Current",
           save_plot = FALSE, fig_output_dir = NULL, nums = 1:4, m_dir = NULL, g_dir) {
    
    list_file <- list.files(
      path = raster_to_do_pca,
      pattern = "\\.asc$", full.names = T
    )
    layer <- stack(list_file)
    
    dir_create(sv_dir)
    dir_create(sv_proj_dir)

    s1 <-
      spca(
        layers_stack = layer,
        layers_to_proj = layer,
        sv_dir = sv_dir,
        layers_format = ".asc",
        sv_proj_dir = sv_proj_dir
      )

    f2 <-
      summary(s1)

    # The scree plot
    if (save_plot == TRUE & !is.null(fig_output_dir)) {
      png(
        filename = fig_output_dir,
        width = 1200 * 1.3, height = 1200 * 1.3, res = 300
      )
      plot(f1$importance[3, 1:5] * 100,
        xlab = "Principal component",
        ylab = "Percentage of variance explained", ylim = c(0, 100),
        type = "b", frame.plot = T, cex = 1.5
      )
      points(f1$importance[2, 1:5] * 100, pch = 17, cex = 1.5)
      lines(f1$importance[2, 1:5] * 100, lty = 2, lwd = 1.5)
      legend(
        x = 3.5, y = 60, legend = c("Cumulative", "Non-cumulative"),
        lty = c(1, 2), pch = c(21, 17), bty = "n", cex = 0.85, pt.bg = "white"
      )

      dev.off()
    }

    if (!is.null(m_dir)) {
      file.copy(
        from = paste0(sv_proj_dir,"/PC0", nums, ".asc"),
        to = paste0(m_dir, "/PC0", nums, ".asc")
      )

      g_dir <- paste0(g_dir, "/Set_", set, "/", time)

      dir_create(g_dir)

      file.copy(
        from = paste0(sv_proj_dir,"/PC0", nums, ".asc"),
        to = paste0(g_dir, "/PC0", nums, ".asc")
      )
    } else {
      g_dir <- paste0(g_dir, "/Set_", set, "/", time)

      dir_create(g_dir)

      file.copy(
        from = paste0(sv_proj_dir,"/PC0", nums, ".asc"),
        to = paste0(g_dir, "/PC0", nums, ".asc")
      )
    }
  }
