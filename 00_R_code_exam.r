##########################################################################################################################
#-------------------------------------- Pre-post fire analysis Palermo August 2023 --------------------------------------#
##########################################################################################################################

# AUTHOR: Giulian Capasso

# The project focuses on the fires that occurred in the area of Palermo, Sicily between July and August 2023
# The fire caused evacuations, lack of electricity/water and loss of biodiversity 
# I compared the images of 08/2022 and 08/2023. 2021 images were analyzed and excluded because approximable to 2021 
# The project might spark Interest on potential long-term analysis on arson in Sicily and vegetation recovery
# The area examined ranges between Terrasini and Termini Imerese and covers up to Piana degli Albanesi. The Ficuzza wood was not affected
# This code processes Sentinel 2B and landsat 8-9(band 10 for thermic data) images downloaded from https://scihub.copernicus.eu/dhus/#/home and https://earthexplorer.usgs.gov/

# NAMES OF THE USED BANDS:

# 2022
# [1] "T33SUC_20220706T095039_B01_20m" "T33SUC_20220706T095039_B02_20m"
# [3] "T33SUC_20220706T095039_B03_20m" "T33SUC_20220706T095039_B04_20m"
# [5] "T33SUC_20220706T095039_B05_20m" "T33SUC_20220706T095039_B06_20m"
# [7] "T33SUC_20220706T095039_B07_20m" "T33SUC_20220706T095039_B11_20m"
# [9] "T33SUC_20220706T095039_B12_20m" "T33SUC_20220706T095039_B8A_20m"

# 2023
# [1] "T33SUC_20230810T094549_B01_20m" "T33SUC_20230810T094549_B02_20m"
# [3] "T33SUC_20230810T094549_B03_20m" "T33SUC_20230810T094549_B04_20m"
# [5] "T33SUC_20230810T094549_B05_20m" "T33SUC_20230810T094549_B06_20m"
# [7] "T33SUC_20230810T094549_B07_20m" "T33SUC_20230810T094549_B11_20m"
# [9] "T33SUC_20230810T094549_B12_20m" "T33SUC_20230810T094549_B8A_20m"

# LST
# 2022
# LC08_L2SP_189034_20220908_20220914_02_T1_ST_B10.TIF

# 2023
# LC09_L2SP_189034_20230818_20230822_02_T1_ST_B10.TIF

# -------------------------------------------------- #

# Summary 
# 1. Libraries, Data Import and Visualization 
# 2. Spectral Indices
# 3. Classification 
# 4. Land Surface Temperature

# -------------------------------------------------- #

# 1. Libraries, Data Import and Visualization 

# install or recall required packages
library(raster)  
library(ggplot2)
library(RStoolbox)
library(patchwork)
library(viridis)
library(rasterVis)

# Set working directory for MAcOS
setwd("~/Desktop/palermo")

# Import Data and start processing
# 2022 
# use rlist to create a list of files in the folder set as working directoty according to a specific pattern, like 2022 in this case
rlist_22 <- list.files(pattern = "2022") 
# now apply a function to the whole list, in this case 'raster', to import satellite images 
import_22 <- lapply(rlist_22, raster)
# finally merge all the bands in one single rasterbirck with the funcion 'stack'
tgr_22 <- stack(import_22)

# The original image is too wide to focus on the burned areas, so we need to crop
# create an exent and crop the image to focus on the burned area
ext <- c(330000, 371000, 4200220, 4237800)
crop_22 <- crop(tgr_22, ext) 

# export the true colors image in pdf
pdf("Truecolor2022.pdf")
plotRGB(crop_22, r=4, g=3, b=2, stretch ="lin")
dev.off()

# Now do the same with 2023 images in the folder
# 2023
rlist_23 <- list.files(pattern = "2023") 
import_23 <- lapply(rlist_23, raster)
tgr_23 <- stack(import_23)
crop_23 <- crop(tgr_23, ext) 

# and export
pdf("Truecolor2023.pdf")
plotRGB(crop_23, r=4, g=3, b=2, stretch ="lin")
dev.off()

#Quick reminder of the bands
# Bands for both images

# Blue : band 1 - B02 file
# Green: band 2 - B03 file 
# Red:   band 3 -  B04 file
# NIR:   band 4 - B06 file
# SWIR:  band 9 - B07 file

# ______ for LAND SUFRACE TEMPERATURE CHAPTER 4.
# band 10 B10 Landsat8 for Thermal Data

# Image visualization for the burned area 2022-23 with ggplot
# True color visualization
t_2022 <- ggRGB(crop_22, r=4, g=3, b=2, stretch="lin") + ggtitle("2022") +
  theme(axis.text = element_blank(), axis.title = element_blank())
t_2023 <- ggRGB(crop_23, r=4, g=3, b=2, stretch="lin") + ggtitle("2023") +
  theme(axis.text = element_blank(), axis.title = element_blank())

t_2022 + t_2023

# Now False Colors > NIR on Red to enhance vegetation
f_2022 <- ggRGB(crop_22, r=10, g=4, b=3, stretch="lin") + ggtitle("2022")+
  theme(axis.text = element_blank(), axis.title = element_blank())
f_2023 <- ggRGB(crop_23, r=10, g=4, b=3, stretch="lin") + ggtitle("2023")+
  theme(axis.text = element_blank(), axis.title = element_blank())
pdf("ggrgb.pdf")
t_2022 + t_2023 + f_2022 + f_2023
dev.off()

# Multi composition 1990-2023 with title 
# Not relevant for presentation
# used 1990 data exclusiverly for the prequel

par(mfrow = c(2, 3), bty = "n",oma = c(1, 1, 2, 1), bg = "white")

plotRGB(crop_90, r=3, g=2, b=1, stretch="lin")
legend("topright", legend = "1990 RGB", cex = 0.8, inset = 0.03)

plotRGB(crop_22, 4,3,2, stretch="lin")
legend("topright", legend = "2022 RGB", cex = 0.8, inset = 0.03)

plotRGB(crop_23, 4,3,2, stretch="lin")
legend("topright", legend = "2023 RGB", cex = 0.8, inset = 0.03)

plotRGB(crop_90, r=4, g=3, b=2, stretch="lin")
legend("topright", legend = "1990 NIR", cex = 0.8, inset = 0.03)

plotRGB(crop_22, 6,4,3, stretch="lin")
legend("topright", legend = "2022 NIR", cex = 0.8, inset = 0.03)

plotRGB(crop_23, 6,4,3, stretch="lin")
legend("topright", legend = "2023 NIR",cex = 0.8, inset = 0.03)

mtext("RGB Plot Using Different Bands", outer = TRUE, cex = 1, line = 0.5)

dev.off()

#--------------------------------------------#

# 2. Spectral Indices

# Vegetation health measure: DVI e NDVI calculation for 2022 and 2023 using NIR and RED bands
# Burned areas analysis with NBR index using NIR and SWIR bands
# Burned area Index (BAI = 1 / ((0.1 - RED)^2 + (0.06 - NIR)^2). Threshold values of 0.1 in the red channel and 0.06 in the near-infrared channel are used as references.
# SAVI SAVI = ((Band 5 – Band 4) / (Band 5 + Band 4 + 0.5)) * (1.5)
# Creating a customized color palette

####-----#### DVI (Difference Vegetation Index): NIR - RED ####-----####

# My Palettes
# My favorite Blind Friendly palettes 
viridis <- colorRampPalette(viridis(7))(255)
magma <- colorRampPalette(magma(7))(255)
inferno <- colorRampPalette(inferno(7))(255)

# Palettes for personal use - High contrast 
clb2 <- colorRampPalette(c("white","darkgreen", "yellow", "orange"))(100)
col_v <- colorRampPalette(c("white", "lightyellow1", "yellow","palegreen2", "cyan4", "darkblue", "orchid4", "magenta2", "plum1"))(100)
col_v2 <- colorRampPalette(c("lightyellow1", "yellow","palegreen2", "cyan4", "darkblue", "orchid4", "magenta2","plum1"))(100)

# dvi = Nir - Red
dvi22 = crop_22[[6]] - crop_22[[4]]
dvi23 = crop_23[[6]] - crop_23[[4]]

# Different scales so 
# Unify scale 
min_value_dvi <- min(min(values(dvi22)), min(values(dvi23)))
max_value_dvi <- max(max(values(dvi22)), max(values(dvi23)))

# Multiframe creation 2022-2023
par(mfrow=c(1,2))
plot(dvi22, col=magma, main="DVI 2022", zlim = c(min_value_dvi, max_value_dvi))
plot(dvi23, col=magma, main="DVI 2022", zlim = c(min_value_dvi, max_value_dvi))

# Positive Value: healthy vegetation and dense vegetation, as healthy plants reflect more NIR light for photosynthesis
# Negative Value: presence of bare soil or poorly developed vegetation cover

# DVI difference from 2022 to 2023
dvi_dif = dvi22 - dvi23

pdf("dvi-dif23-22")
par(mfrow=c(1,2))
plotRGB(crop_23, r=4, g=3, b=2, stretch="lin", main="True Color Image 2023")
plot(dvi_dif, col=magma, axes = FALSE, box = FALSE, main="DVI Difference 2022-23")
dev.off()

# A positive difference between DVI values of the two years indicates that the vegetation health and density have likely decreased from 2022 to 2023
# A negative difference between DVI values suggests an increase in vegetation health and density 

####-----#### NDVI: (NIR - RED) / (NIR + RED) ####-----#### 

ndvi22 =  dvi22 / (crop_22[[10]] + crop_22[[4]])
ndvi23 =  dvi23 / (crop_23[[10]] + crop_23[[4]])

# Plot and export as pdf
pdf("ndvi-22-23.pdf")
par(mfrow=c(1,2))
plot(ndvi22, col=magma, axes = FALSE, box = FALSE, main="NDVI 2022")
plot(ndvi23, col=magma, axes = FALSE, box = FALSE, main="NDVI 2023")
dev.off()

# positive value = heatly vegetation
# negative value = bare soil/ burned areas / lack of vegetation

# NDVI difference from 2022 to 2023
ndvi_dif = ndvi22 - ndvi23

# unify the scale 
min_value_ndvi <- min(min(values(ndvi22)), min(values(ndvi23)))
max_value_ndvi <- max(max(values(ndvi22)), max(values(ndvi23)))

par(mfrow=c(2,2))
plot(ndvi_dif, col = magma, main = "NDV index 2022", zlim = c(min_value_ndvi, max_value_ndvi))

# positive value = lack og vegetation
# negative value heatly vegetation

####-----#### NBR (Normalized Burn Ratio) = (NIR - SWIR) / (NIR + SWIR) ####-----####

#NBR 2022
NBR_2022 = (crop_22[[10]] - crop_22[[9]]) / (crop_22[[10]] + crop_22[[9]])
plot(NBR_2022, col = magma) + title(main="NBR 2022") 

#NBR 2023
NBR_2023 = (crop_23[[10]] - crop_23[[9]]) / (crop_23[[6]] + crop_23[[9]])
plot(NBR_2023, col = magma) + title(main="NBR 2023") 

pdf("NBR_22-23.pdf")
par(mfrow=c(1,2))
plot(NBR_2022, axes = FALSE, box = FALSE, col = magma) + title(main="NBR 2022") 
plot(NBR_2023, axes = FALSE, box = FALSE, col = magma) + title(main="NBR 2023") 
dev.off()

# positive value = heatly vegetation
# negative value = bare soil/ burned areas / lack of vegetation

# unify the scale 
min_value_nbr <- min(min(values(NBR_2022)), min(values(NBR_2023)))
max_value_nbr <- max(max(values(NBR_2022)), max(values(NBR_2023)))

# Plot NBR and compare it to NDVI
pdf("ndvi+nbr.pdf")
par(mfrow=c(2,2), mai=c(0.2, 0.2, 0.2, 0.2)) 
plot(ndvi22, col=magma, axes = FALSE, box = FALSE, main="NDVI 2022", zlim = c(min_value_ndvi, max_value_ndvi))
plot(ndvi23, col=magma, axes = FALSE, box = FALSE, main="NDVI 2023", zlim = c(min_value_ndvi, max_value_ndvi))
plot(NBR_2022, col = magma,axes = FALSE, box = FALSE, main = "NBR 2022", zlim = c(min_value_nbr, max_value_nbr))
plot(NBR_2023, col = magma, axes = FALSE, box = FALSE, main = "NBR 2023", zlim = c(min_value_nbr, max_value_nbr))
dev.off()

# NDVI DIFFERENCE compared to NBR DIFFERENCE
ndvi_dif = ndvi22 - ndvi23
nbrdif <- NBR_2022 - NBR_2023

# unify the scale 
min_value_dif <- min(min(values(ndvi_dif)), min(values(nbrdif)))
max_value_dif <- max(max(values(ndvi_dif)), max(values(nbrdif)))

par(mfrow=c(1,2))
plot(nbrdif, col = inferno, axes = FALSE, box = FALSE, zlim = c(min_value_nbr, max_value_nbr), main = "NBR Dif")
plot(ndvi_dif, col = inferno, axes = FALSE, box = FALSE, zlim = c(min_value_nbr, max_value_nbr), main = "NDVI Dif")

# positive value = lack of vegetation
# negative lavue = healty vegetation 

####-----#### BAI (SWIR-NIR ) / (SWIR + NIR) + A) * (1 + A) * (1 + B) ####-----####

# Contants definitions
A = 0.08
B = 0.67
L = 0.5  # Regulation constant

# BAI
BAI_S22 <- (crop_22[[9]] - crop_22[[10]]) / (crop_22[[9]] + crop_22[[10]] + A) * (1 + A) * (1 + B)
BAI_S23 <- (crop_23[[9]] - crop_23[[10]]) / (crop_23[[9]] + crop_23[[10]] + A) * (1 + A) * (1 + B)

# AApplication of L constant that equals to the condition of the soil 1=a lot of vegetion; 0=bare soil; 0.5= medium condition/standard
BAI_S22_adj <- (1 + L) * BAI_S22 - L
BAI_S23_adj <- (1 + L) * BAI_S23 - L

pdf("BAI.pdf")
par(mfrow=c(1,2), mar=c(1,2,2,5))
plot(BAI_S22_adj, col = magma, axes = FALSE, box = FALSE, main = "BAI 2022")
plot(BAI_S23_adj, col = magma, axes = FALSE, box = FALSE, main = "BAI 2023")
dev.off()
# positive values = burned areas
# negative values = heakty vegetation

# BAI DIFFERENCE
BAIDIF <- BAI_S22_adj - BAI_S23_adj
plot(BAIDIF, col = magma, axes = FALSE, box = FALSE, main = "BAI 2023")
plot(BAIDIF, col = col_v, axes = FALSE, box = FALSE, main = "BAI 2023")

# positive values = heatly vegetation 
# negative values = burned areas

####-----#### SAVI <- (NIR - RED) / (NIR + RED + L * (1 + L)) || L = 0.5 ####-----####

SAVI22 <- (crop_22[[6]] - crop_22[[4]]) / (crop_22[[6]] + crop_22[[4]] + 0.5 * (1+0.5))
SAVI23 <- (crop_23[[6]] - crop_23[[4]]) / (crop_23[[6]] + crop_23[[4]] + 0.5 * (1+0.5)) 

#SAVI VS BAI
pdf("SAVI.pdf")
par(mfrow=c(2,2), mar=c(1,2,2,5))
plot(SAVI22, col = magma, axes = FALSE, box = FALSE, main = "SAVI 2022")
plot(SAVI23, col = magma, axes = FALSE, box = FALSE, main = "SAVI 2023")
plot(BAI_S22_adj, col = magma, axes = FALSE, box = FALSE, main = "BAI 2022")
plot(BAI_S23_adj, col = magma, axes = FALSE, box = FALSE, main = "BAI 2023")
dev.off()

# posotive values = healty vegetation
# negative value = bare soil / burned areas

#SAVI DIFFERENCE
SAVIDIF <- SAVI22 - SAVI23
plot(SAVIDIF, col = inferno, axes = FALSE, box = FALSE,main = "SAVI DIF")

# posotive values = burned vegetation
# negative value = healty vegetation


# ALL INDICES COMPARED, PLOTTED AND EXPORTED
pdf("totale.pdf")
par(mfrow=c(2,4))
plot(ndvi_dif, col = inferno, axes = FALSE, box = FALSE, main = "NDVI DIF")
plot(nbrdif, col = inferno, axes = FALSE, box = FALSE,main = "NBR DIF")
plot(SAVIDIF, col = inferno, axes = FALSE, box = FALSE,main = "SAVI DIF")
plot(BAIDIF, col = inferno, axes = FALSE, box = FALSE, main = "BAI DIF")
plot(ndvi_dif, col = col_v, axes = FALSE, box = FALSE, main = "NDVI DIF")
plot(nbrdif, col = col_v, axes = FALSE, box = FALSE,main = "NBR DIF")
plot(SAVIDIF, col = col_v, axes = FALSE, box = FALSE,main = "SAVI DIF")
plot(BAIDIF, col = col_v, axes = FALSE, box = FALSE, main = "BAI DIF")
dev.off()

#--------------------------------------------#

# 3. Classification
# Classification with Unsuperclass is a powerful tool in remote sensing for land cover classification 
# When comparing pre and post-fire satellite imagery, it aids in identifying changes in vegetation, land use, and burnt areas
# This function assists in highlighting shifts in land cover, making it easier to assess the impact of wildfires and track environmental changes over time

# set.seed() function is used to set the seed for the random number generator 
# This is a crucial function, especially in statistical and data analysis tasks, where reproducibility of results is important
set.seed(42) 
clas22 <- unsuperClass(crop_22, nClasses=5)
clas23 <- unsuperClass(crop_23, nClasses=5)

# not so blind friendly palette, but useful for me. Please customize  
clr <- colorRampPalette(c("white", "darkorchid", "darkslateblue", "gold2", "palegreen4"))(100)

# 2022
par(mfrow=c(2,1))
plot(clas22$map, col=clr, axes = FALSE, box = FALSE) + title(main="Soil Classification 2022")
plotRGB(crop_22, r=4, g=3, b=2, stretch="lin")

# frequencies of classes = distribution of data points (pixels or segments) among different classes or clusters 
# identified by an unsupervised classification algorithm 
# This distribution provides insights into the composition of the image
freq(clas22$map)
# 1 -  237362 - cities / bare rock  
# 2 - 1048263 - dry / burned land
# 3 - 1354468 - water bodies
# 4 -  615922 - vegetation
# 5 -  593133 - vegetation

# Percentage of classes in 2022
tot_land_22 <- 237362 + 1048263 + 615922 + 593133
tot_veg_22 <- 615922 + 593133
percent_veg_22 <- tot_veg_22 * 100 / tot_land_22
percent_veg_22 # 48.46533 % VEGETATION in 2022

percent_cities_rocks_22 <- 237362 * 100 / tot_land_22
percent_cities_rocks_22 # 9.514727 % CITIES/ROCKS in 2022

tot_dry_22 <- 1048263 
percent_dry_22 <- tot_dry_22 * 100 / tot_land_22
percent_dry_22 # 42.01994 % DRY LAND in 2022

# Now the same with 2023 
par(mfrow=c(1,2))
plot(clas23$map, col=clr, axes = FALSE, box = FALSE) + title(main="Soil Classification 2023")
plotRGB(crop_23, r=4, g=3, b=2, stretch="lin")

freq(clas23$map)
# 1 -  446911 - burned land
# 2 -  946192 - dry land
# 3 - 1354606 - water bodies
# 4 -  351882 - cities / bare rock
# 5 -  752359 - vegetation 

# Percentage of classes in 2023
tot_land_23 <- 446911 + 946192 + 351882 + 752359
tot_veg_23 <- 752359 

percent_veg_23 <- tot_veg_23 * 100 / tot_land_23
percent_veg_23 # 30.12637 % VEGETATION

percent_cities_rocks_23 <- 351882 * 100 / tot_land_23
percent_cities_rocks_23 # 14.09025 % CITIES/ROCKS

tot_dry_23 <-  946192 + 446911
percent_dry_23 <- tot_dry_23 * 100 / tot_land_23
percent_dry_23 #55.78338 DRY / BURNED LAND

## classes and frequencies representation ## 

# dataframe creation
cl_22 <- c("Cities/Rocks", "Vegetation", "Dry/Burned Land")
perc22 <- c(9.5, 48.5, 42.0)
cl_23 <- c("Cities/Rocks", "Vegetation", "Dry/Burned Land")
perc23 <- c(14.1, 30.1, 55.7)

df0 <- data.frame(cl_22, perc22)
df1 <- data.frame(cl_23, perc23)

# Graphs
is1 <- ggplot(df0, aes(x=cl_22, y=perc22, fill=cl_22)) +  
  geom_bar(stat="identity") +
  ggtitle("Land Cover percentage in 2022") +
  labs(fill="Classes", x="Classes", y="%") +
  geom_text(aes(label=perc22), vjust=-0.3, size=3.5) +
  scale_fill_manual(values=c("honeydew3", "plum4", "yellowgreen")) + 
  ylim(0, 100) +
  theme_minimal()

is2 <- ggplot(df1, aes(x=cl_23, y=perc23, fill=cl_23)) +  
  geom_bar(stat="identity") +
  ggtitle("Land Cover percentage in 2023") +
  labs(fill="Classes", x="Classes", y="%") +
  geom_text(aes(label=perc23), vjust=-0.3, size=3.5)+
  scale_fill_manual(values=c("honeydew3", "plum4", "yellowgreen")) + 
  ylim(0, 100) +
  theme_minimal()

is1 + is2

#--------------------------------------------#

# 4. Land Surface Temperature Analysis
# using Band 10 of Landsat 8-9
# valuable remote sensing technique for assessing the temperature of the Earth's surface from satellite imagery 
# in this case I want to compare 2022 to 2023 to see if surface temperature have changed before and after the fire

setwd("~/Desktop/palermo/LST")

lst_22 <- raster("LC08_L2SP_189034_20220908_20220914_02_T1_ST_B10.TIF")
lst_23 <- raster("LC09_L2SP_189034_20230818_20230822_02_T1_ST_B10.TIF")

# crop to the same extent
crop_t_22 <- crop(lst_22,ext)
crop_t_23 <- crop(lst_23,ext)

# Plot but they have different scale 
par(mfrow=c(1,2))
plot(crop_t_22, col = inferno)
plot(crop_t_23, col = inferno)

# unify scale
min_value_t2 <- min(min(values(crop_t_22)), min(values(crop_t_23)))
max_value_t2 <- max(max(values(crop_t_22)), max(values(crop_t_23)))

#plot again with heat.colors color palette
pdf("temp.pdf")
par(mfrow=c(1,2) , mar=c(1,2,2,4))
plot(crop_t_22, col = rev(heat.colors(100)), zlim = c(min_value_t2, max_value_t2),
     axes = FALSE, box = FALSE, main = "2022")
plot(crop_t_23, col = rev(heat.colors(100)), zlim = c(min_value_t2, max_value_t2),
     axes = FALSE,box = FALSE, main = "2023")
dev.off()

####-----####

# Hrre I tried to convert radiance to celsius degrees, but it didnt work

# Calibration values for B10 and B11 bands
M_b10 <- 0.0003342
A_b10 <- 0.1
M_b11 <- 0.0003342
A_b11 <- 0.1

# Radiance of B10 and B11
radianza_b10 <- (lst_22 * M_b10) + A_b10
radianza_b10_23 <- (crop_t_23 * M_b10) + A_b10
#radianza_b11 <- (band_11 * M_b11) + A_b11

# Earth Temperature Constants 
K1 <- 774.8853
K2 <- 1321.0789

# Calculate the brightness temperature for bands 10 and 11
brightness_temp_b10 <- K2 / log((K1 / radianza_b10) + 1)
brightness_temp_b10_23 <- K2 / log((K1 / radianza_b10_23) + 1)
#brightness_temp_b11 <- K2 / log((K1 / radianza_b11) + 1)

# Plot
#par(mfrow=c(1,2))
plot(brightness_temp_b10, main="Band 10 Brightness Temperature")
#plot(brightness_temp_b11, main="Band 11 Brightness Temperature")

brightness_temp_b10_celsius <- brightness_temp_b10 - 273.15
brightness_temp_b10_23_celsius <- brightness_temp_b10_23 - 273.15
plot(brightness_temp_b10_23_celsius, main="Band 10 Brightness Temperature (°C)")
plot(brightness_temp_b10_celsius, main="Band 10 Brightness Temperature (°C)")

##--- Second try

# Radiometric calibration coefficients for band 10
RADIANCE_MULT_BAND_10 <- 3.8000E-04
RADIANCE_ADD_BAND_10 <- 0.10000

# Upload 10 thermal band image
thermal_band <- raster("LC08_L2SP_189034_20220908_20220914_02_T1_ST_B10.TIF")

# Calculation of monochromatic radiance (Lλ)
radiance_band <- (crop_t_22 * RADIANCE_MULT_BAND_10) + RADIANCE_ADD_BAND_10

# Thermodynamic constants
PLANCK_CONSTANT <- 6.62607004e-34
SPEED_OF_LIGHT <- 299792458
BOLTZMANN_CONSTANT <- 1.38064852e-23

# Wavelength of the thermal band 10 in meters
WAVELENGTH_BAND_10 <- 10.8951e-6

# Radiometric temperature calculation (TIR)
temperature_radiometric <- (PLANCK_CONSTANT * SPEED_OF_LIGHT) / (WAVELENGTH_BAND_10 * BOLTZMANN_CONSTANT) / log((radiance_band / 0.99) + 1)

# Calibration coefficients for radiometric temperature
K1_CONSTANT_BAND_10 <- 774.8853
K2_CONSTANT_BAND_10 <- 1321.0789

# Radiometric temperature calibration
temperature_calibrated <- K2_CONSTANT_BAND_10 / log((K1_CONSTANT_BAND_10 / temperature_radiometric) + 1)

# Land surface temperature (LST) display
plot(temperature_calibrated, main = "Land Surface Temperature (LST) in Celsius")

# Calibration coefficients
RADIANCE_MULT_BAND_10 <- 3.8000E-04
RADIANCE_ADD_BAND_10 <- 0.10000

# Thermal band 10  values (from Landsat 8)
dn_values <- values(lst_22)

# Conversion from DN to radiance
radianza_values <- RADIANCE_MULT_BAND_10 * dn_values + RADIANCE_ADD_BAND_10

# Constants for the final conversion to temperature
K1 <- 774.89   # Costante K1 specifica per Landsat 8
K2 <- 1321.08  # Costante K2 specifica per Landsat 8

# Conversion from radiance to temperature in Celsius
temperature_celsius <- K2 / log((K1 / radianza_values) + 1)

# Plot raster image of temperatures in degrees Celsius
plot(temperature_celsius, col = (heat.colors(100)))

# Again, it didn't work

#______________________________#
