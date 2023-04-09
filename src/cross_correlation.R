library(sp)
library(spatialEco)
install.packages('spatialEco')

data(meuse)
coordinates(meuse) <- ~x+y

( I <- crossCorrelation(meuse$zinc, meuse$copper,meuse$lead, coords = coordinates(meuse), 
                        clust = TRUE, k=99) )
meuse$lisa <- I$SCI[,"lsci.xy"]
meuse$lisa.clust <- as.factor(I$cluster)
spplot(meuse, "lisa")
spplot(meuse, "lisa.clust") 
