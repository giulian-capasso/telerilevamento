# R_code_classification.r
# Classification of an image
# With Solar Orbiter Data

# Loading required package
install.packages("RStoolbox")

library(raster)
# library(RStoolbox)

setwd("~/Desktop/lab_/") 

# data import
so <- brick("Solar_Orbiter_s_first_views_of_the_Sun_pillars.jpg")

# visualize RGB leves
plotRGB(so, 1, 2, 3, stretch="lin")
plotRGB(so, 1, 2, 3, stretch="hist")

# Classifying the solar data (unsupervised classification > classes decided by the software)
#set.seed(42) to pick always the same specific pixels; used for repeating the experiment in the same manner for N times

soc <- unsuperClass(so, nClasses=3)
cl <- colorRampPalette(c('yellow','black','red'))(100)
plot(soc$map, col=cl)

# Unsupervised Classification with 20 classes
soc20 <- unsuperClass(so, nClasses=20)
cl <- colorRampPalette(c('yellow','black','red'))(100)
plot(soc20$map, col=cl)


## DAY 2 Grand Canyon ##

gc <- brick("dolansprings_oli_2013088_canyon_lrg.jpg")
gc

# red = 1
# green = 2
# blue = 3

plotRGB(gc, r=1, g=2, b=3, stretch="lin")

# change the stretch to histogram stretching
plotRGB(gc, r=1, g=2, b=3, stretch="hist")

# classification
gcclass2 <- unsuperClass(gc, nClasses=2)
gcclass2

plot(gcclass2$map)
# set.seed(17)

# Exercise: classify the map with 4 classes
gcclass4 <- unsuperClass(gc, nClasses=4)
gcclass4

clc <- colorRampPalette(c('yellow','red','blue','black'))(100)
plot(gcclass4$map, col=clc)

# compare the classified map with the original set
par(mfrow=c(2,1))
plot(gcclass4$map, col=clc)
plotRGB(gc, r=1, g=2, b=3, stretch="hist")
