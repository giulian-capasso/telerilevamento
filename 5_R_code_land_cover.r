# Code for generating land cover maps from satellite imagery
# multitemporal analysis of a modification in land use


library(raster)
library(RStoolbox) # #contains functions for classification
# install.packages("ggplot2")
library(ggplot2)
# install.packages("gridExtra")
library(gridExtra) # for grid.arrange plotting

setwd("~/Desktop/lab")

# NIR 1, RED 2, GREEN 3

# let's import the multispectral images defor1 and defor2
defor1 <- brick("defor1.jpg") # brick to import the entire dataset and band of the image
plotRGB(defor1, r=1, g=2, b=3, stretch="lin")
ggRGB(defor1, r=1, g=2, b=3, stretch="lin")

# import defor2 e plot together

defor2 <- brick("defor2.jpg")
plotRGB(defor2, r=1, g=2, b=3, stretch="lin")
ggRGB(defor2, r=1, g=2, b=3, stretch="lin")

par(mfrow=c(1,2)) #plot the images in the same graph
plotRGB(defor1, r=1, g=2, b=3, stretch="lin")
plotRGB(defor2, r=1, g=2, b=3, stretch="lin")

# multiframe with ggplot2 
# today we use it for the first time to make statistical graphs on the frequencies of the classes 
# but we will also use it later as a graphic rendering of satellite or drone images
# ggRGB function is based on two packages, ggplot2 and RStoolbox
# the RStoolbox package contains the ggRGB function, it's an RGB plot but uses ggplot2
# just replace plotRGB with ggRGB

p1 <- ggRGB(defor1, r=1, g=2, b=3, stretch="lin")
p2 <- ggRGB(defor2, r=1, g=2, b=3, stretch="lin")

# now I have to merge these two images with the patchwork package
# to use this function I have to associate the two plots above with a ggRGB object
# install patchwork package
install.packages("patchwork")
library(patchwork)

p1+p2
p1/p2 # to put one image on tho pf the other

# unsupervised classification
d1c <- unsuperClass(defor1, nClasses=2)
plot(d1c$map) #always specify "map"
# class 1: forest
# class 2: agriculture

# set.seed() would allow you to attain the same results ...

d2c <- unsuperClass(defor2, nClasses=2)
plot(d2c$map)
# class 1: agriculture
# class 2: forest

# unsupervised classification of defor2 from 2006
d2c3 <- unsuperClass(defor2, nClasses=3)
plot(d2c3$map)

# we obtained forest pixels to calculate the area occupied by the forest through the yaars
# To do so let's calculate the frequency (how many times a certain event occurs)
# ex. frequency of pixels belonging to the forest class

# frequencies
# to calculate the frequencies, use the freq function, from basic R 
# it generates frequency tables 
freq(defor1c$map)
#   value  count
# [1,]     1 305664 (forest)
# [2,]     2  35628 (agricultural areas)

freq(defor2c$map)
#     value  count
# [1,]     1 164530 (forest)
# [2,]     2 178196 (agricultural areas)

# create a dataset with all frequencies and then do a final plot with ggplot2
# calculation of the proportion and percentage of the forest in 92 and 06

totpxdefor1 <- 341292

# class proportion
prop_defor1 <- 305664 / totpxdefor1
prop_defor1
[1] 0.8956085

# class percentage 
perc_defor1 <- 305664 * 100 / totpxdefor1
# [1] 89.56085

# Calculate percentage agricultural areas
totpxagr1 <- 341292
prop_agr1 <- 35628 / totpxagr1
perc_agr1 <- 35628 * 100 / totpxagr1
perc_agr1
# [1] 10.43915

# Percentage forest and agricultural areas in defor 2
> perc_for2 <- 164530 * 100 / 342726
> perc_for2
[1] 48.00628
> perc_agr2 <- 100 - perc_for2
> perc_agr2
[1] 51.99372

#FINAL DATA 
# Percentage forest 92 = 89.56085 (defor1) 
# Percentage agric 92 = 10.43915 (defor1) 
# Percentage forest 06 = 48.00628 (defor2) 
# Percentage agric 06 = 51.99372 (defor2) 

# build a dataframe with 3 columns
# the first is the class
# the second with the % values of 92 
# the third with the % values of 2006 

class <- c("Forest","Agriculture") # "" because text (e.g. "Forest")
percent_1992 <- c(89.56, 10.44)
percent_2006 <- c(48.00, 51.99)

multitemporal <- data.frame(class, percent_1992, percent_2006)
multitemporal

# plot both of them
ggplot(multitemporal, aes(x=class, y=percent_1992, color=class)) + geom_bar(stat="identity", fill="white")
ggplot(multitemporal, aes(x=class, y=percent_2006, color=class)) + geom_bar(stat="identity", fill="white")

p1 <- ggplot(multitemporal, aes(x=class, y=percent_1992, color=class)) + geom_bar(stat="identity", fill="white")
p2 <- ggplot(multitemporal, aes(x=class, y=percent_2006, color=class)) + geom_bar(stat="identity", fill="white")
