# Spectral Indices

library(raster)

# install.packages("rgdal")
# install.packages("RStoolbox")
# install.packages("rasterdiv")
# library(rgdal)
library(RStoolbox)
# library(rasterdiv)

# Exercise: import the first file -> defor1_.jpg -> give it the name l1992
l1992 <- brick("defor1_.jpg")

plotRGB(l1992, r=1, g=2, b=3, stretch="lin")
                  
# layer 1 = NIR
# layer 2 = red
# layer 3 = green

# Exercise: import the second file -> defor2_.jpg -> give it the name l2006

# l2006 <- brick("defor2_.jpeg")
l2006

plotRGB(l2006, r=1, g=2, b=3, stretch="lin")

# Exercise: plot in a multiframe the two images with one on top of the other
par(mfrow=c(2,1))
plotRGB(l1992, r=1, g=2, b=3, stretch="lin")
plotRGB(l2006, r=1, g=2, b=3, stretch="lin")

# vegetation heatlh measure
# DVI Difference Vegetation Index
dvi1992 = l1992[[4]] - l1992[[3]]
# or:
# dvi1992 = l1992$defor1_.1 - l1992$defor1_.2
dvi1992 

# DVI Difference Vegetation Index
dvi2006 = l2006[[3]] - l2006[[4]]
dvi2006
plot(dvi2006, col=cl)

par(mfrow=c(2,1))
> plot(dvi1992, col=cl)
> plot(dvi2006, col=cl)

# DVI difference in time
dvi_dif = dvi1992 - dvi2006
cld <- colorRampPalette(c('blue','white','red'))(100) 
plot(dvi_dif, col=cld)

## DAY 2 ##

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
