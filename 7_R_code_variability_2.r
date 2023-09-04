# 1 multivariate analysis # 2 heterogeneity analysis

library(raster)
library(RStoolbox)
library(ggplot2)
library(patchwork)
library(viridis)

setwd("~/Desktop/lab_/")

# let's combine multivariate analysis together with spatial patterns
# measuring heterogeneity in an image is useful for assessing biodiversity in an area
# the lower the sign is heterogeneous the lower biodiversity is


#1st multivariate analysis

similaun <- brick("sentinel.png")

# band1 = NIR
# band2 = red
# band3 = green

ggRGB(similaun, 1, 2, 3)

#rasterPCA> extract a single layer with multivariate analysis> starting from three layers, we compact in a single layer
simPCA <- rasterPCA(similaun)
simPCA
summary(simPCA$model) 
# $model indicates the model we are going to use (let's see the correlations between the bands)
# Importance of components:
# Proportion of Variance = first componet explains 78%, the second 32%, the third 0%
# Cumulative Proportion = btw first and second 99,62%, 

plot(simPCA$map)

pc1sim <- simPCA$map$PC1
pc2sim <- simPCA$map$PC2
pc3sim <- simPCA$map$PC3
 
ggplot() + geom_raster(pc1sim, mapping = aes(x=x, y=y, fill=PC1))


# calculation of the variability on one of the components that shows the maximum variability with the ##focal function
# Standard deviation of PC1
sd_sim_PC1 <- focal(pc1sim, matrix(1/9, 3, 3), fun=sd)

# Map by ggplot the standard deviation of the first proncipal component

ggplot() + 
geom_raster(sd_sim_PC1, mapping =aes(x=x, y=y, fill=layer)) + 
scale_fill_viridis(option = "cividis") + 
ggtitle("Standard deviation of the first PC")

# images altogether 
 
im1 <- ggRGB(similaun, 3, 2, 1)
g1 <- ggplot() + geom_raster(pc1sim, mapping = aes(x=x, y=y, fill=PC1))
im3 <- ggplot() + geom_raster(sd_sim_PC1, mapping =aes(x=x, y=y, fill=layer)) + scale_fill_viridis(option = "inferno")

im1 + g1 + im3 

# Calculate heterogenity in a 5x5 window ((focal(img, matrix(), fun=sd)

sd5 <- focal(pc1sim, matrix(1/25, 5, 5), fun=sd)

# Map by ggplot the standard deviation of the first proncipal component

im4 <- ggplot() + geom_raster(sd5, mapping = aes(x=x, y=y, fill=layer)) + scale_fill_viridis(option = "inferno")

im3 + im4

# now window 7x7 and plot
sd7 <- focal(pc1sim, matrix(1/49, 7, 7), fun=sd)

im7 <- ggplot() + geom_raster(sd7, mapping = aes(x=x, y=y, fill=layer)) + scale_fill_viridis(option = "inferno")

im3 + im4 + im7
