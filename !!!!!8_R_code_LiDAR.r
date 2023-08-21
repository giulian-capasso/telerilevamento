# R code to visualize and analysing LiDAR data

library(raster)
library(ggplot2)
library(viridis)
library(RStoolbox)
# install.packages("lidR")
library(lidR)

setwd("~/Desktop/lab")

dsm_2013 <- raster("2013Elevation_DigitalElevationModel-0.5m.tif")

dtm_2013 <- raster("2013Elevation_DigitalTerrainModel-0.5m.tif")

plot(dtm_2013)

# calcolare CHM
chm_2013 <- dsm_2013-dtm_2013
chm_2013

ggplot() + 
  geom_raster(chm_2013, mapping =aes(x=x, y=y, fill=layer)) + 
  scale_fill_viridis() +
  ggtitle("CHM 2013 San Genesio/Jenesien")

# blue =grass
# houses (10 - 20) 
# woods = lighter (20 - 40)
  
 dsm_2004 <- raster("2004Elevation_DigitalElevationModel-2.5m.tif")
  
 dtm_2004 <- raster("2004Elevation_DigitalTerrainModel-2.5m.tif")
  
 chm_2004 <- dsm_2004-dtm_2004
  
 ggplot() + 
  geom_raster(chm_2004, mapping =aes(x=x, y=y, fill=layer)) + 
  scale_fill_viridis() +
  ggtitle("CHM 2004 San Genesio/Jenesien")
  
#comparison

difference_chm <- chm_2013-chm_2004

# got to change the data resolution -> from more accurate to less accurate resolution
# "resample" function: "x" what you want to resample, "y" the resolution of the other image 

chm_2013_r <- resample(chm_2013, chm_2004)
chm_2013_r
  
difference_chm <- chm_2013_r-chm_2004

ggplot() +
  geom_raster(difference_chm, mapping =aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis() +
  ggtitle("CHM difference San Genesio/Jenesien")
# the darker areas are deforested areas
# in the lighter, yellow areas, there is an increase in plants


point_cloud <- readLAS("point_cloud.laz")
plot(point_cloud)
# -> questi due non mi vengono
  
  
  
