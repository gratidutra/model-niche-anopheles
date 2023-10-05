library(sp)
library(spatialEco)
library(tidyverse)

sp <- 
  read_csv('data/workflow_maxent/Anopheles_albimanus/Anopheles_albimanus_joint.csv')

xy <- 
  sp %>% 
  select (longitude, latitude) %>% data.matrix()

temperature <-
  raster("data/workflow_maxent/Anopheles_albimanus/G_Variables/Set_1/Current/bio1.asc")

umity <-
  raster("data/workflow_maxent/Anopheles_albimanus/G_Variables/Set_1/Current/bio15.asc")

sp['temperature'] <- 
  raster::extract(temperature, xy)
 
sp['umity'] <- 
  raster::extract(umity, xy)

coordinates(sp) <- 
  ~longitude+latitude

I <- crossCorrelation(sp$temperature, sp$umity, coords = coordinates(sp), 
                         type = c("LSCI", "GSCI"),
                        clust = TRUE, k=99)
sp$lisa <- I$SCI [,"lsci.xy"]
sp$lisa.clust <- as.factor(I$cluster)
spplot(sp, "lisa")
spplot(sp, "lisa.clust") 
