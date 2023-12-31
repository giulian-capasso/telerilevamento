# Let's calculate Spectral vegetation Indices
# When vegetation is not healty, the reflectance in many waves shifts
# this change therefore indicates stress
# we can use the bands we have seen so far to create these indices

# recall the package 
library(raster)

# or install those needed
# install.packages("rgdal")
# install.packages("RStoolbox")
# install.packages("rasterdiv")
# library(rgdal)
library(RStoolbox)
# library(rasterdiv)

# (RStoolbox)library, already loaded, adds tools used to analyze remote sensing data
# it contains a function called spectralIndices that calculates a series of spectral indices like NDVi
# to use it we have to specify the bands involved, we will calculate the indices concerning the red, infrared and green bands
# the NDVI index is used to calculate water stress in vegetation

# Exercise: import the first file -> defor1_.jpg -> give it the name l1992
l1992 <- brick("defor1_.jpg")

plotRGB(l1992, r=1, g=2, b=3, stretch="lin")
                  
# layer 1 = NIR
# layer 2 = red
# layer 3 = green

# Exercise: import the second file -> defor2_.jpg -> give it the name l2006

# l2006 <- brick("defor2_.jpeg")
l2006

# plot in False colors with NIR placed in the Red
plotRGB(l2006, r=1, g=2, b=3, stretch="lin")

# Exercise: plot in a multiframe the two images with one on top of the other
par(mfrow=c(2,1))
plotRGB(l1992, r=1, g=2, b=3, stretch="lin")
plotRGB(l2006, r=1, g=2, b=3, stretch="lin")

# Vegetation heatlh measure
# DVI Difference Vegetation Index
dvi1992 = l1992[[4]] - l1992[[3]]
# or:
# dvi1992 = l1992$defor1_.1 - l1992$defor1_.2 
# $ is used as a rope 
dvi1992 

# DVI Difference Vegetation Index
dvi2006 = l2006[[3]] - l2006[[4]]
dvi2006
plot(dvi2006, col=cl)

# Multiframe visualization 
par(mfrow=c(2,1))
plot(dvi1992, col=cl)
plot(dvi2006, col=cl)

# DVI difference in time to see the alterations 
dvi_dif = dvi1992 - dvi2006
cld <- colorRampPalette(c('blue','white','red'))(100) 
plot(dvi_dif, col=cld)

## DAY 2 ##

# Another index similar to DVI called NDVI is standardized on the sum of the two bands. 
# important because if we use two images with different bit numbers, to standardize the two indices (e.g. 266 and 265) 
# it is sufficient to standardize on the reflectance values of the two total bands. it just serves to standardize our index

# DVI #RANGE if I have an 8-bit image (for each of the two bands, infrared black and red, we have 256 possible values) then a pixel will have the maximum infrared black
# 255-0= 255(reflect all)and max red(absorb all) 0-255=-255
# so the DVI range is -255 and 255
# RANGE NDVI (255-0)/(255+0)=1 this is maximum, minimum is (0-255)/(0+255)=-1
# 8-bit image NDVI range is -1 and 1


# Range DVI (8 bit): -255 a 255
# Range NDVI (8 bit): -1 a 1

# Range DVI (16 bit): -65535 a 65535
# Range NDVI (16 bit): -1 a 1

# Hence, NDVI can be used to compare images with a different radiometric resolution

# NDVI 1992
dvi1992 = l1992[[1]] - l1992[[2]]
ndvi1992 = dvi1992 / (l1992[[1]] + l1992[[2]])
# or
ndvi1992 = (l1992[[1]] - l1992[[2]]) / (l1992[[1]] + l1992[[2]])

# multiframe con il plotRGB dell'immagine sopra
# e l'ndvi sotto

# Multiframe with plotRGB on top of the NDVI image
par(mfrow=c(2,1))
plotRGB(l1992, r=1, g=2, b=3, stretch="lin")
plot(ndvi1992, col=cl)

# 2006
dvi2006 = (l2006[[1]] - l2006[[2]])
ndvi2006 = dvi2006 / (l2006[[1]] + l2006[[2]])

# Multiframe with NDVI1992 on top of the NDVI2006 image
par(mfrow=c(2,1))
plot(ndvi1992, col=cl)
plot(ndvi2006, col=cl)

# Automatic spectral indices by the spectralIndices function (all possible indeces)
si1992 <- spectralIndices(l1992, green=3, red=2, nir=1)
plot(si1992,col=cl)

si2006 <- spectralIndices(l2006, green=3, red=2, nir=1)
plot(si2006,col=cl)

plot(copNDVI)
