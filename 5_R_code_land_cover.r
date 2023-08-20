# Code for generating land cover maps from satellite imagery

library(raster)
library(RStoolbox) # classification
# install.packages("ggplot2")
library(ggplot2)
# install.packages("gridExtra")
library(gridExtra) # for grid.arrange plotting

setwd("~/Desktop/lab")

# NIR 1, RED 2, GREEN 3

defor1 <- brick("defor1.jpg")
plotRGB(defor1, r=1, g=2, b=3, stretch="lin")
ggRGB(defor1, r=1, g=2, b=3, stretch="lin")

# import defor2 e plot together

defor2 <- brick("defor2.jpg")
plotRGB(defor2, r=1, g=2, b=3, stretch="lin")
ggRGB(defor2, r=1, g=2, b=3, stretch="lin")

par(mfrow=c(1,2))
plotRGB(defor1, r=1, g=2, b=3, stretch="lin")
plotRGB(defor2, r=1, g=2, b=3, stretch="lin")

# multiframe with ggplot2 and gridExtra
p1 <- ggRGB(defor1, r=1, g=2, b=3, stretch="lin")
p2 <- ggRGB(defor2, r=1, g=2, b=3, stretch="lin")
grid.arrange(p1, p2, nrow=2)

# unsupervised classification
d1c <- unsuperClass(defor1, nClasses=2)
plot(d1c$map)
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

# frequencies
freq(defor1c$map)
#   value  count
# [1,]     1 305664 (forest)
# [2,]     2  35628 (agricultural areas)

freq(defor2c$map)
#     value  count
# [1,]     1 164530 (forest)
# [2,]     2 178196 (agricultural areas)

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

# build a dataframe
# Columns (fields)
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

grid.arrange(p1, p2, nrow=1)
