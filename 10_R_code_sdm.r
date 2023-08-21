# Species Distribution Modelling

# pick some environmental variables and look at the probability of finding a certain species according to them
# the sdm package does species distribution mode

# install.packages("sdm")
# install.packages("rgdal", dependencies=T)

library(sdm)
library(raster) # predictors
library(rgdal) # species

#system.file function loads a file into R specifying from which package it is taken from
file <- system.file("external/species.shp", package="sdm") 
species <- shapefile(file)

# looking at the set
species
# occurrence > presence or absence of a certain species
# 200 points where the species was sampled

# plot
plot(species)

# looking at the occurrences
species$Occurrence

# copy and then write:
plot(species[species$Occurrence == 1,],col='blue',pch=16) #pch is the symbol of the "points" on the map
points(species[species$Occurrence == 0,],col='red',pch=16)
#points in the space where presence(1) or absence(0) was registered

# plot just the presences
plot(species[species$Occurrence == 1,], col="blue", pch=19) #presence
# add points to the previous one
points(species[species$Occurrence == 0,], col="red", pch=19) #absence

# predictors: look at the path
path <- system.file("external", package="sdm") 

# list the predictors
lst <- list.files(path=path,pattern='asc$',full.names = T) #
lst

# stack
preds <- stack(lst)

# plot preds
cl <- colorRampPalette(c('blue','orange','red','yellow')) (100)
plot(preds, col=cl)

# plot predictors and occurrences
plot(preds$elevation, col=cl)
points(species[species$Occurrence == 1,], pch=16)

plot(preds$temperature, col=cl)
points(species[species$Occurrence == 1,], pch=16)

plot(preds$precipitation, col=cl)
points(species[species$Occurrence == 1,], pch=16)

plot(preds$vegetation, col=cl)
points(species[species$Occurrence == 1,], pch=16)

# model

# set the data for the sdm
datasdm <- sdmData(train=species, predictors=preds)

# model
m1 <- sdm(Occurrence ~ elevation + precipitation + temperature + vegetation, data=datasdm, methods = "glm")

# make the raster output layer
p1 <- predict(m1, newdata=preds) 

# plot the output
plot(p1, col=cl)
points(species[species$Occurrence == 1,], pch=16)

# add to the stack
s1 <- stack(preds,p1)
plot(s1, col=cl)

# to change names in the plot of the stack
# choose a vector of names for the stack, looking at the previous graph
names(s1) <- c('elevation', 'precipitation', 'temperature', 'vegetation', 'model')
# and then replot:
plot(s1, col=cl)
