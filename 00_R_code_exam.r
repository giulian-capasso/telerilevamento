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
library(patchwork)
library(RColorBrewer)

setwd("~/Desktop/exam")

# blue band 1 - B02
# green band 2 - B03
# red band 3 - B04
# infrared band 4 - B08

# 2018
rlist_18 <- list.files(pattern = "201808") 
rlist_18

import_18 <- lapply(rlist_18, raster)
import_18

tgr_18 <- stack(import_18)
tgr_18

plotRGB(tgr_18, r=4, g=3, b=2, stretch="lin")

# 2019
rlist_19 <- list.files(pattern = "201908") 
rlist_19

import_19 <- lapply(rlist_19, raster)
import_19

tgr_19 <- stack(import_19)
tgr_19

plotRGB(tgr_19, r=4, g=3, b=2, stretch="lin")


# 2020 >> NUVOLE!!
rlist_20 <- list.files(pattern = "202008") 
rlist_20

import_20 <- lapply(rlist_20, raster)
import_20

tgr_20 <- stack(import_20)
tgr_20

plotRGB(tgr_20, r=4, g=3, b=2, stretch="lin")


#2021
rlist_21 <- list.files(pattern = "202108") 
rlist_21

import_21 <- lapply(rlist_21, raster)
import_21

tgr_21 <- stack(import_21)
tgr_21

plotRGB(tgr_21, r=4, g=3, b=2, stretch="lin")

#2022 > un po di nuvole
rlist_22 <- list.files(pattern = "202208") 
rlist_22

import_22 <- lapply(rlist_22, raster)
import_22

tgr_22 <- stack(import_22)
tgr_22

plotRGB(tgr_22, r=4, g=3, b=2, stretch="lin")

#2023
rlist_23 <- list.files(pattern = "202308") 
rlist_23

import_23 <- lapply(rlist_23, raster)
import_23

tgr_23 <- stack(import_23)
tgr_23

plotRGB(tgr_23, r=4, g=3, b=2, stretch="lin")

# plot and par images
par(mfrow=c(3,3))
plotRGB(tgr_18, r=4, g=3, b=2, stretch="lin")
plotRGB(tgr_19, r=4, g=3, b=2, stretch="lin")
plotRGB(tgr_20, r=4, g=3, b=2, stretch="lin")
plotRGB(tgr_21, r=4, g=3, b=2, stretch="lin")
plotRGB(tgr_22, r=4, g=3, b=2, stretch="lin")
plotRGB(tgr_23, r=4, g=3, b=2, stretch="lin")

# considering that i put the infrared band on red, everything that appears red in the image is vegetation

dev.off()

## LAND COVER ##

# I classify images by function unsuperClass

pc2018 <- unsuperClass(tgr_18, nClasses=4)
plot(pc2018$map)
pc2018
# class 1: bare ground
# class 2: clouds
# class 3: sea
# class 4: vegetation

pc2019 <- unsuperClass(tgr_19, nClasses=4)
plot(pc2019$map)
pc2018
# class 1: bare ground
# class 2: clouds
# class 3: sea
# class 4: vegetation
