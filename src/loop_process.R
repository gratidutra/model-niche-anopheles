source("src/functions.R")

for (i in seq_along(sp_data_list)) {
  
    sp_name <- sp_data_list[[i]]$species[1]

    dir_create(paste0("data/workflow_maxent/", sp_name))
    dir_create(paste0("data/workflow_maxent/", sp_name,"/outputs/"))
    dir_create(paste0("data/workflow_maxent/", sp_name, "/Model_calibration"))
    dir_create(paste0("data/workflow_maxent/", sp_name, "/Model_calibration/M_variables"))
    dir_create(paste0("data/workflow_maxent/", sp_name, "/Model_calibration/M_variables/bio_lcor"))
    dir_create(paste0("data/workflow_maxent/", sp_name, "/Model_calibration/M_variables/PCA"))
    dir.create(paste0("data/workflow_maxent/", sp_name, "/pcas"))
    dir.create(paste0("data/workflow_maxent/", sp_name, "/pcas/pca_referenceLayers"))
    dir.create(paste0("data/workflow_maxent/", sp_name, "/pcas/pca_proj"))

    kuenm_occsplit(
      occ = sp_data_list[[i]],
      train.proportion = 0.7,
      method = "random", save = T,
      name = paste0("data/workflow_maxent/", sp_name, "/", sp_name)
    )

    # Data Exploratory -----------------------------------------------------------------

    ## only temperature variables

    explore_espace(
      data = sp_data_list[[i]], species = "species", longitude = "longitude",
      latitude = "latitude", raster_layers = current_neotropic_layer[[1:11]], save = T,
      name = paste0("data/workflow_maxent/", sp_name,"/outputs/Temperature_variables.pdf"), open = F
    )

    ## only precipitation variables

    explore_espace(
      data = sp_data_list[[i]], species = "species", longitude = "longitude",
      latitude = "latitude", raster_layers = current_neotropic_layer[[12:19]], save = T,
      name = paste0("data/workflow_maxent/", sp_name,"/outputs/Precipitation_variables.pdf"), open = F
    )

    s1 <-
      spca(
        layers_stack = raster_neotropic_list,
        layers_to_proj = raster_neotropic_list,
        sv_dir = paste0("data/workflow_maxent/", sp_name, "/pcas/pca_referenceLayers"),
        layers_format = ".asc",
        sv_proj_dir = paste0("data/workflow_maxent/", sp_name, "/pcas/pca_proj")
      )

    # Read the pca object (output from ntbox function)

    lf <-
      list.files(
        path = paste0("data/workflow_maxent/", sp_name, "/pcas/pca_referenceLayers"),
        pattern = "\\.rds$", full.names = TRUE
      )

    f1 <-
      readRDS(lf)

    # Summary

    f2 <-
      summary(f1)
    
    # save the cumulative contributions PCA
    
    sink("data/workflow_maxent/", sp_name,"/outputs/cumulative_pca.txt")
    print(f2)
    sink()

    # The scree plot

      png(
        filename = paste0("data/workflow_maxent/", sp_name,"/outputs/screeplot.png"),
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
      
    nums <- 1:4

    file.copy(
      from = paste0("data/workflow_maxent/", sp_name, "/pcas/pca_referenceLayers/PC0", nums, ".asc"),
      to = paste0("data/workflow_maxent/", sp_name, "/Model_calibration/M_variables/PCA/PC0", nums, ".asc")
    )

    dir_create(paste0("data/workflow_maxent/", sp_name, "/G_Variables"))
    dir_create(paste0("data/workflow_maxent/", sp_name, "/G_Variables/Set_1"))
    dir_create(paste0("data/workflow_maxent/", sp_name, "/G_Variables/Set_1/Current"))

    # Aqui da para testar o var comb

    file.copy(
      from = paste0(
        "data/workflow_maxent/", sp_name, "/Model_calibration/M_variables/PCA/PC0",
        nums,
        ".asc"
      ),
      to = paste0(
        "data/workflow_maxent/", sp_name, "/G_Variables/Set_1/Current/PC0",
        nums,
        ".asc"
      )
    )
  }

