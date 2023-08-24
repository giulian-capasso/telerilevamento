# AUTHOR: Giulian Capasso

# description ------

# NAMES OF THE USED BANDS:

# 2018
#  T33SUC_20180801T095031_B02_10m.jp2
#   T33SUC_20180801T095031_B03_10m.jp2
#    T33SUC_20180801T095031_B04_10m.jp2
#     T33SUC_20180801T095031_B08_10m.jp2

#2019
#  T33SUC_20190816T095031_B02_10m.jp2
#   T33SUC_20190816T095031_B03_10m.jp2
#    T33SUC_20190816T095031_B04_10m.jp2
#     T33SUC_20190816T095031_B08_10m.jp2

#2020
#  T55HEU_20200227T001109_B02_10m.jp2
#   T55HEU_20200227T001109_B03_10m.jp2
#    T55HEU_20200227T001109_B04_10m.jp2
#     T55HEU_20200227T001109_B08_10m.jp2

#2020
#  T33SUC_20200820T095041_B02_10m.jp2
#   T33SUC_20200820T095041_B03_10m.jp2
#    T33SUC_20200820T095041_B04_10m.jp2
#     T33SUC_20200820T095041_B08_10m.jp2

#2021
#  T33SUC_20210825T095031_B02_10m.jp2
#   T33SUC_20210825T095031_B03_10m.jp2
#    T33SUC_20210825T095031_B04_10m.jp2
#     T33SUC_20210825T095031_B08_10m.jp2

#2022
#  T33SUC_20220830T094601_B02_10m.jp2
#   T33SUC_20220830T094601_B03_10m.jp2
#    T33SUC_20220830T094601_B04_10m.jp2
#     T33SUC_20220830T094601_B08_10m.jp2

#2023
#  T33SUC_20230815T095031_B02_10m.jp2
#   T33SUC_20230815T095031_B03_10m.jp2
#    T33SUC_20230815T095031_B04_10m.jp2
#     T33SUC_20230815T095031_B08_10m.jp2



# SUMMARY
# 1 # LAND COVER ANALYSIS
## 2 # LAND SURFACE TEMPERATURE ANALYSIS ??

# 1 
# import and data preparation

library(raster)  
library(ggplot2)
library(RStoolbox)
library(RColorBrewer)
library(viridis)

setwd("~/Desktop/exam")

# blue band 1 - B02
# green band 2 - B03
# red band 3 - B04
# infrared band 4 - B08

# 2018
rlist_18 <- list.files(pattern = "2018") 
rlist_18

import_18 <- lapply(rlist_18, raster)
import_18

tgr_18 <- stack(import_18)
tgr_18

plotRGB(tgr_18, r=4, g=3, b=2, stretch="lin")

# 2019 
rlist_19 <- list.files(pattern = "2019") 
rlist_19

import_19 <- lapply(rlist_19, raster)
import_19

tgr_19 <- stack(import_19)
tgr_19

plotRGB(tgr_19, r=4, g=3, b=2, stretch="lin")

# 2020
rlist_20 <- list.files(pattern = "2020") 
rlist_20

import_20 <- lapply(rlist_20, raster)
import_20

tgr_20 <- stack(import_20)
tgr_20

plotRGB(tgr_20, r=4, g=3, b=2, stretch="lin")


#2021 
rlist_21 <- list.files(pattern = "2021") 
rlist_21

import_21 <- lapply(rlist_21, raster)
import_21

tgr_21 <- stack(import_21)
tgr_21

plotRGB(tgr_21, r=4, g=3, b=2, stretch="lin")

#2022 
rlist_22 <- list.files(pattern = "2022") 
rlist_22

import_22 <- lapply(rlist_22, raster)
import_22

tgr_22 <- stack(import_22)
tgr_22

plotRGB(tgr_22, r=4, g=3, b=2, stretch="lin")

#2023
rlist_23 <- list.files(pattern = "2023") 
rlist_23

import_23 <- lapply(rlist_23, raster)
import_23

tgr_23 <- stack(import_23)
tgr_23

plotRGB(tgr_23, r=4, g=3, b=2, stretch="lin")

par(mfrow)

# plot and par images
par(mfrow=c(3,3))
plotRGB(tgr_18, r=4, g=3, b=2, stretch="lin")
plotRGB(tgr_19, r=4, g=3, b=2, stretch="lin")
plotRGB(tgr_20, r=4, g=3, b=2, stretch="lin")
plotRGB(tgr_21, r=4, g=3, b=2, stretch="lin")
plotRGB(tgr_22, r=4, g=3, b=2, stretch="lin")
plotRGB(tgr_23, r=4, g=3, b=2, stretch="lin")

# Considering that i put the infrared band on red, everything that appears red in the image is vegetation

# Cropping the images to focus on land vegetation

ext <- c(3e+05, 409800, 4190220, 4240220)

#2018
crop_18 <- crop(tgr_18, ext) 
plot(crop_18)

#2019
crop_19 <- crop(tgr_19, ext) 
plot(crop_19)

#2020
crop_20 <- crop(tgr_20, ext) 
plot(crop_20)

#2021
crop_21 <- crop(tgr_21, ext) 
plot(crop_21)

#2022
crop_22 <- crop(tgr_22, ext) 
plot(crop_22)

#2023
crop_23 <- crop(tgr_23, ext) 
plot(crop_23)

# ## LAND COVER

# Classification of the images by function unsuperClass

# set color palette to enhance the differences between the classes
clc <- colorRampPalette(c('yellow2','cyan4','royalblue4','brown2'))(100)

# Classification 2018
set.seed(42) # set specific pixels to reproduce identical results each tipe
class_18 <- unsuperClass(crop_18, nClasses=4)

pdf("class_18.pdf")
par(mfrow=c(1,2))
plotRGB(crop_18, r=3, g=2, b=1, stretch="lin")
plot(class_18$map, col=clc, axes = FALSE, box = FALSE)
dev.off()

freq(class_18$map)
# 1 14603688 - agriculture / dry land
# 2 26700323 - water bodies
# 3 10410076 - vegetation
# 4 3185913 - urban areas

# ------------------------------------------ #

# Classification 2019
set.seed(42)
class_19 <- unsuperClass(crop_19, nClasses=4)

pdf("class_19.pdf")
par(mfrow=c(1,2))
plotRGB(crop_19, r=3, g=2, b=1, stretch="lin")
plot(class_19$map, col=clc, axes = FALSE, box = FALSE)
dev.off()

freq(class_19$map)
# 1  6895229 - vegetation 
# 2 14612683 - agriculture
# 3 26675876 - water bodies
# 4  6716212 - dry land / cities

# ------------------------------------------ #

# Classification 2020 >>> TROVARE FOTO SENZA CLOUDS NUVOLE!!! RIFARE 
set.seed(42) 
class_20 <- unsuperClass(crop_20, nClasses=4)

pdf("class_20.pdf")
par(mfrow=c(1,2))
plotRGB(crop_20, r=3, g=2, b=1, stretch="lin")
plot(class_20$map, col=clc, axes = FALSE, box = FALSE)
dev.off()

freq(class_20$map)
# 1 13365240 - water bodeis
# 2  7249526 - vegetation + agricult
# 3  7477871 - clouds
# 4 26807363 - dry land / urban areas

# ------------------------------------------ #

# Classification 2021
set.seed(42) 
class_21 <- unsuperClass(crop_21, nClasses=4)

pdf("class_21.pdf")
par(mfrow=c(1,2))
plotRGB(crop_21, r=3, g=2, b=1, stretch="lin")
plot(class_21$map, col=clc, axes = FALSE, box = FALSE)
dev.off()

# frequencies
freq(class_21$map)
# 1 10725554 - urban areas / dry land
# 2  4154237 - dry land / bare ground
# 3 26617356 - water bodies
# 4 11460720 - vegetation


#--------------------------------------------#


# Classification 2022 NUVOLE!!! RIFARE NUVOLE!!! RIFARE 
set.seed(42) 
class_22 <- unsuperClass(crop_22, nClasses=4)

pdf("class_22.pdf")
par(mfrow=c(1,2))
plotRGB(crop_22, r=3, g=2, b=1, stretch="lin")
plot(class_22$map, col=clc, axes = FALSE, box = FALSE)
dev.off()

freq(class_22$map)
# 1 10263600 - agriculture
# 2   152543 - clouds 
# 3  5501619 - vegetation
# 4  9799178 - bare ground


#--------------------------------------------#


# Classification 2023
set.seed(42) 
class_23 <- unsuperClass(crop_23, nClasses=4)

pdf("class_23.pdf")
par(mfrow=c(1,2))
plotRGB(crop_23, r=3, g=2, b=1, stretch="lin")
plot(class_23$map, col=clc, axes = FALSE, box = FALSE)
dev.off()

freq(class_23$map)
# 1  7670523 - vegetation
# 2 26746776 - water bodies 
# 3 13143132 - dry field / bare ground
# 4  7339569 - dry land / cities 


# Visualizing this data has been useful to obtain maps where I have pixels of green areas (vegetation),
# I can now calculate the occupied area and the proportion of pixels of green areas from 2018 to 2023
# to do so I use the freq. function


# Classification 2023
class_23 <- unsuperClass(crop_23, nClasses=4)
plot(class_23$map)
class_23
