# R code to visualize and analysing LiDAR data
# Lidar, short for "Light Detection and Ranging,"  
# advanced remote sensing technology that uses laser pulses to measure the distance between the sensor and the earth's surface 
# This Lidar data provides detailed information about the topography, vegetation structure, and other aspects of the Earth's surface

# detailed and multidimensional overview of the earth's environment
# enables a better understanding of ecosystems, geological processes and environmental changes

# recall packages 
library(raster)
library(ggplot2)
library(viridis)
library(RStoolbox)
# install.packages("lidR")
library(lidR)

# Set working directory on MacOS
setwd("~/Desktop/lab")

# Visualize and analyze data with LIDAR
# Download the files from the data.zip folder which I open in the lab folder

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
  
# Import data from 2004
 dsm_2004 <- raster("2004Elevation_DigitalElevationModel-2.5m.tif")
 dtm_2004 <- raster("2004Elevation_DigitalTerrainModel-2.5m.tif")
 chm_2004 <- dsm_2004-dtm_2004

# and plot the difference with ggplot 
 ggplot() + 
  geom_raster(chm_2004, mapping =aes(x=x, y=y, fill=layer)) + 
  scale_fill_viridis() +
  ggtitle("CHM 2004 San Genesio/Jenesien")
  
#comparison 2004 - 2013

difference_chm <- chm_2013-chm_2004
# error because the images have two different resolutions
# we need to change the resolution of one of the two data. you go from the most accurate to the least

# got to change the data resolution -> from more accurate to less accurate resolution
# "resample" function: "x" what you want to resample, "y" the resolution of the other image 

chm_2013_r <- resample(chm_2013, chm_2004)
chm_2013_r

# x is what we want to resample (2013) 
# y is the resolution to transform it to with 
# this command to resample one image based on another. 
# we had used a similar command which was aggregate but it doesn't do exactly the same thing
# on this we told it how much to transform the resolution

# let's compare now
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
  
  
  
