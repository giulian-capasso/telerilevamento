# R_code_classification.r
# Classification of an image
# how to switch from continuous data to classes eg mineralogical composition
# With Solar Orbiter Data

# photos taken from the space station
# we don't have satellite images like previously (imp. never say satellite photos)

# Loading required package
install.packages("RStoolbox")

library(raster)
# library(RStoolbox)

setwd("~/Desktop/lab_/") 

# data import
so <- brick("Solar_Orbiter_s_first_views_of_the_Sun_pillars.jpg")
# solar orbiter is the satellite that takes data on the movements of the sun

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
# use "hist" to increase the visualizatio of the tails of the curve that is a straight line in case of "lin"
plotRGB(gc, r=1, g=2, b=3, stretch="hist")

# classification
gcclass2 <- unsuperClass(gc, nClasses=2)
gcclass2

# select the map with $ to then plot it
plot(gcclass2$map)
# classe 1 is rock and class 2 is water and shadows/different mineralogical composition
# set.seed(17), deliberatly choosen number, to maintain the same pixel classification every time we relaunch the classification

# Exercise: classify the map with 4 classes
gcclass4 <- unsuperClass(gc, nClasses=4)
gcclass4

clc <- colorRampPalette(c('yellow','red','blue','black'))(100)
plot(gcclass4$map, col=clc)

# compare the classified map with the original set
par(mfrow=c(2,1))
plot(gcclass4$map, col=clc)
plotRGB(gc, r=1, g=2, b=3, stretch="hist")
