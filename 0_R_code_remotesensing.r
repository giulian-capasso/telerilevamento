# my first code in R for remote sensing

# install.packages("raster")
# The raster package can create, read, manipulate, and write raster data
library(raster)

# setting working directory in MacOS
setwd("~/Desktop/lab")

# import the first image
# brick funtion creates a multi-layer raster object
# i am using the brick function to import a satellite image

l2011 <- brick("p224r63_2011.grd")
l2011

# in the image p224r63_2011.grd, p224 stands for path 224 of the Landsat satellite ; r63 stands for row in the planisphere in the following link
# https://gisgeography.com/wp-content/uploads/2016/01/World-Referencing-System.png

# class tells me the type of object we have: rasterbrick
# 1499 number of rows
# 2967 columns
# 4447533 pixels for each band
# 7 bands
# n rows for the n of columns gives me the n of pixels or the resolution
# resolution is the pixel size 30x30m
# source data source
# name are the names of the sre bands (spectral reflectans) applies to all bands except for the thermic one
# minimum values is always 0 (except in the thermic band) and then maximum values
# the values range from 0 to 1: because reflectance is the division between the reflected radiant flux and incident 
# if the object does not reflect anything, the reflectance is equal to 0
# if the object reflects everything, the result is 1
# so minimum value 0 and maximum value 1
# (consider that the values don't always go from 0 to 1)

# plot the image
plot(l2011)

# https://www.r-graph-gallery.com/42-colors-names.html color names to use as arguments
# customize color palette to make the plot more visible
cl <- colorRampPalette(c("black", "grey", "light grey")) (100) 
# col is a parameter in the plot function to work on the colours
plot(l2011, col=cl)

# Clarifying Landsat bands 
# b1 = blue
# b2 = green
# b3 = red
# b4 = near infra red NIR
# b5 = middle infra red 
# b6 = thermal infra red
# b7 = second sensor for middle infrared

# to close and definitively clean the specified plot use dev.off() 
dev.off()

# plot now just the blue band - B1_sre
plot(l2011$B1_sre)
# or with [[]] 
plot(l2011[[1]])

plot(l2011$B1_sre) 
# create a personalized color palette with colorRampPalette(c()) 
# see some R colors here https://bookdown.org/hneth/ds4psy/D-3-apx-colors-basics.html
cl <- colorRampPalette(c("black", "grey", "light grey")) (100)
plot(l2011$B1_sre, col=cl) 

# plot b1 from dark blue to blue to light blue
clb <- colorRampPalette(c("dark blue", "blue", "light blue")) (100)
plot(l2011$B1_sre, col=clb) 

# export pdf image to lab folder,
#  always add dev.off() at the end
pdf("banda1.pdf")
plot(l2011$B1_sre, col=clb) 
dev.off()

# or you can also export png
png("banda1.png")
plot(l2011$B1_sre, col=clb) 
dev.off()

# plot b2 from dark green to green to light green
clg <- colorRampPalette(c("dark green", "green", "light green")) (100)
plot(l2011$B2_sre, col=clg) 

# par function allows to set specific multi frame graphics or in general set the image space
# with the par funtion i can plot two or more specific bands/images side by side, in raws and columns
par(mfrow=c(1,2)) # 2 columns 1 row 
plot(l2011$B1_sre, col=clb) 
plot(l2011$B2_sre, col=clg) 
dev.off()

# export multiframe plot
pdf("multiframe.pdf")
par(mfrow=c(1,2))
plot(l2011$B1_sre, col=clb) 
plot(l2011$B2_sre, col=clg) 
dev.off()

# exercise: revert the multiframe 2 rows 1 column 
par(mfrow=c(2,1))
plot(l2011$B1_sre, col=clb) 
plot(l2011$B2_sre, col=clg) 

# let's plot the first four bands
par(mfrow=c(2,2))
# blue
plot(l2011$B1_sre, col=clb) 
# green
plot(l2011$B2_sre, col=clg) 
# red
clr <- colorRampPalette(c("dark red", "red", "pink")) (100)
plot(l2011$B3_sre, col=clr)
# NIR
clnir <- colorRampPalette(c("red", "orange", "yellow")) (100)
plot(l2011$B4_sre, col=clnir)

## DAY 3 ##
# Visualizing data by RGB plotting 
# plot RGB layers
# stretch="lin" allows to stretch/expand the area of reflecance to have a wider color representance
# real colours image 
plotRGB(l2011, r=3, g=2, b=1, stretch="lin")

# trying with different combinations of bands
plotRGB(l2011, r=3, g=4, b=2, stretch="lin")
plotRGB(l2011, r=3, g=2, b=4, stretch="lin")
# stretch="hist" is a non linear function but a "s-lile" function
# hist allows to get more details, in this case possibly about humidity
plotRGB(l2011, r=3, g=4, b=2, stretch="hist")

# Exercise: build a multiframe with visible RGB 
# (linear stretch) on top of false colours (histogram stretch) 
par(mfrow=c(2,1))
plotRGB(l2011, r=3, g=2, b=1, stretch="lin")
plotRGB(l2011, r=3, g=4, b=2, stretch="hist")

# Exercise: upload the image from 1988
l1988 <- brick("p224r63_1988.grd")
l1988

# plot the two images from 1988 and 2011 side by side to compare them
par(mfrow=c(2,1))
plotRGB(l1988, r=4, g=3, b=2, stretch="lin")
plotRGB(l2011, r=4, g=3, b=2, stretch="lin")

