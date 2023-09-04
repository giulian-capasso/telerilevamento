# Time series analysis
# Greenland increase in temperature
# Data and code from Emanuela Cosma

# Using copernicus LST (Land Surface Temperature) variable 
# to see how much the temperature has changed in Greenland over a certain period of time

# install.packages("raster")
# install.packages("rasterVis")
library(raster)
# library(rasterVis) 
# rasterVis = visualization methods for raster images 

setwd("~/Desktop/lab/greenland_data")

# REMINDER: brick function creates a Raster Brick object from a satellite image 
# with many bands all together loads them all on R
# however now we don't have a satellite image ready with all the bands merged together 
# we have 4 different data (lst2000, lst2005, lst2010 and lst2015).
# first we import them one by one (later we'll see how they import all together, with just one function)

# import images
lst_2000 <- raster("lst_2000.tif")
lst_2005 <- raster("lst_2005.tif")
lst_2010 <- raster("lst_2010.tif")
lst_2015 <- raster("lst_2015.tif")

# plot and par images
par(mfrow=c(2,2))
plot(lst_2000)
plot(lst_2005)
plot(lst_2010)
plot(lst_2015)


# now let's see how to import this set of images all together.
# we use the lapplay function which applies a function on a list or a vector
# takes a list of files and applies the same function to all of them
# in our case to import we use the raster function that we apply to the entire list of files.

# list f files and use lapply function to apply the funcion "raster" to a file list 
# very useful when you have to import a high number of files
rlist <- list.files(pattern="lst")
rlist

import <- lapply(rlist,raster)
import

# then use the function stack to put together the files in a single one 
TGr <- stack(import)
TGr
plot(TGr)

# now plot
plotRGB(TGr, 1, 2, 3, stretch="Lin") 
plotRGB(TGr, 2, 3, 4, stretch="Lin") 
plotRGB(TGr, 4, 3, 2, stretch="Lin") 

levelplot(TGr)
cl <- colorRampPalette(c("blue","light blue","pink","red"))(100)
plot(TGr, col=cl)

# levelplot(TGr,col.regions=cl, names.attr=c("July 2000","July 2005", "July 2010", "July 2015"))
# levelplot uses a wider colour palette; gives more space to the image by compacting the coordinates ad the legend
levelplot(TGr,col.regions=cl, main="LST variation in time",
           names.attr=c("July 2000","July 2005", "July 2010", "July 2015"))

##############################################################################
#### Example 2: NO2 decrease during the lockdown period ####
##############################################################################

# recall packages
library(raster)

# set working directory (for MacOS) 
setwd("~/Desktop/lab/en")

# upload raster image
en01 <- raster("EN_0001.png") 

# create a personalized color palette and plot
cl <- colorRampPalette(c('red','orange','yellow'))(100) # 
plot(en01, col=cl)

# assign to make the workflow easier 
en13 <- raster("EN_0013.png")
plot(en13, col=cl)

# Let's import the whole set (altogether!)

# Exercise: import the whole as in the Greenland example
# by the following steps: list.files, lapply, stack 

# tell R to list all files with a specific pattern in the file name, in this case "EN" 
rlist <- list.files(pattern="EN")

# lapply(X,FUN)
# now aplly a specific finction, in this case raster (to import the images) to a list of files
rimp <- lapply(rlist, raster)

# stack 
# now merge all files in a unique rasterbirck 
en <- stack(rimp)

# plot everything
plot(en, col=cl)

# Exercise: plot EN01 besides EN13
par(mfrow=c(1,2))
plot(en[[1]], col=cl)
plot(en[[13]], col=cl)

# or:
en113 <- stack(en[[1]], en[[13]])
plot(en113, col=cl)

# let's make the difference:
difen <-  en[[1]] - en[[13]]
cldif <- colorRampPalette(c('blue','white','red'))(100) # 
plot(difen, col=cldif)

# plotRGB of three files together
plotRGB(en, r=1, g=7, b=13, stretch="lin")
plotRGB(en, r=1, g=7, b=13, stretch="hist")
