# Species Distribution Modelling

# pick some environmental variables and look at the probability of finding a certain species according to them
# the sdm package does species distribution mode

# install.packages("sdm")
# install.packages("rgdal", dependencies=T)

library(sdm)
library(raster) # predictors
library(rgdal) # species

# MODELLIZATION IN THE DISTRIBUTION OF SPECIES (even of any other variable) 
# we take environmental variables and look at the function of those environmental variables 
# which is the probability of finding a certain species the sdm package makes the species distribution mode

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

# we plot the species and inside we make a subset with the square brackets
# we are interested in mapping only the presences
# the comma after 1 must always be used otherwise the command is not closed

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

# plot predictors
cl <- colorRampPalette(c('blue','orange','red','yellow')) (100)
plot(preds, col=cl)

# plot predictors and occurrences
# plot predictors and occurrences $elevation
plot(preds$elevation, col=cl)
points(species[species$Occurrence == 1,], pch=16)

# plot predictors and occurrences 
# $temperature
plot(preds$temperature, col=cl)
points(species[species$Occurrence == 1,], pch=16)

# plot predictors and occurrences
# $precipitation
plot(preds$precipitation, col=cl)
points(species[species$Occurrence == 1,], pch=16)

# plot predictors and occurrences
# $vegetation
plot(preds$vegetation, col=cl)
points(species[species$Occurrence == 1,], pch=16)

# model
# in the sdm package we have the function that declares the data that are all those on the left of our image 
# of our presentation (species and predictors)
# we are interested in ground data trains (species) and predictors (predictors)

# set the data for the sdm
datasdm <- sdmData(train=species, predictors=preds)
datasdm
#the sdm function takes the initial data makes the logistic model 
# (y axis 0 and 1 and x temp axis) 
# which approximates the data where there is none based on existing ones is a linear function
# we use it for every predictors
# with many variables generalized linear model

# model
m1 <- sdm(Occurrence ~ elevation + precipitation + temperature + vegetation, data=datasdm, methods = "glm")

# now I make the prediction (final part) 
# with the predict function of the sdm package I output the raster layer
# prediction on the greater probability of the presence of our species

# make the raster output layer
p1 <- predict(m1, newdata=preds) 

# plot the output
plot(p1, col=cl)
points(species[species$Occurrence == 1,], pch=16)

# I get a forecast map of the distribution of the pest
# it can be seen that where there are black dots the forecast is higher 
# except in some cases because probably in that part the predictors go against the distribution of the species

# add to the stack
s1 <- stack(preds,p1)
plot(s1, col=cl)

# to change names in the plot of the stack
# choose a vector of names for the stack, looking at the previous graph
names(s1) <- c('elevation', 'precipitation', 'temperature', 'vegetation', 'model')
# and then replot:
plot(s1, col=cl)
