# Multivariate analysis
# measurement of the variability on a variable
# first pick a band (e.g. NIR) or I calculate the spectral indices
# (e.g. NDVI) or I can compact everything in one band (multivariate analysis) and take only the first band

# how to switch from a multi-dimensional image to a single-dimensional one?
# if I'm in a bi dimension I don't conceive the third but there can be one

# the first band is the one with the most info and therefore we use the one to calculate the variability

# recall the packages
library(raster)
library(RStoolbox)
library(ggplot2)
library(patchwork)
library(viridis)

# se Working directoy for MacOS
setwd("~/Desktop/lab")

# import the image
p224r63_2011 <- brick("p224r63_2011_masked.grd")

plot(p224r63_2011)

# before multivariate analysis let's resample the image to decrease the resolution and aggregate some pixels
# AGGREGATE function makes summary statistics. fact stands for factor, i.e. how much we aggregate pixels
# ex. fact=10 >> 10pixel x 10 pixel

# resampling
p224r63_2011res <- aggregate(p224r63_2011, fact=10)

# Now plot the original one and the resampled to see the difference 
g1 <- ggRGB(p224r63_2011, 4,3,2)
g2 <- ggRGB(p224r63_2011res, 4,3,2)
# (with patchwork)
g1+g2

# even more compression, higher loss of resolution
p224r63_2011res100 <- aggregate(p224r63_2011, fact=100)

# let's compare
g1 <- ggRGB(p224r63_2011, 4,3,2)
g2 <- ggRGB(p224r63_2011res, 4,3,2)
g3 <- ggRGB(p224r63_2011res100, 4,3,2)

g1+g2+g3

# PCA analysis: rasterPCA analizza le componenti principali per i dati raster
p224r63_2011respca <- rasterPCA(p224r63_2011res)

# $call
# $model
# $map

summary(p224r63_2011respca$model)

plot(p224r63_2011respca$map)

# now plot the first component PC1, that has the most variability, with ggplot and virids inferno as color Palette 
g1 <- ggplot() +
geom_raster(p224r63_2011respca$map, mapping =aes(x=x, y=y, fill=PC1)) +
scale_fill_viridis(option = "inferno") +
ggtitle("PC1")

#plot PC7
g2 <- ggplot() +
geom_raster(p224r63_2011respca$map, mapping =aes(x=x, y=y, fill=PC7)) +
scale_fill_viridis(option = "inferno") +
ggtitle("PC7")

g1+g2

g3 <- ggplot() +
geom_raster(p224r63_2011res, mapping =aes(x=x, y=y, fill=B4_sre)) +
scale_fill_viridis(option = "inferno") +
ggtitle("NIR")

g1+g3

g4 <- plotRGB(p224r63_2011, 2, 3, 4, stretch="lin")

g4 <- ggRGB(p224r63_2011, 2, 3, 4, stretch="hist")

g1+g4

plotRGB(p224r63_2011respca$map, 1, 2, 3, stretch="lin")
plotRGB(p224r63_2011respca$map, 5, 6, 7, stretch="lin")
