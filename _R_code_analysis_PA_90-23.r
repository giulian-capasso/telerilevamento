#******* ******* ******* THIS WAS JUST AN EXPERIMENT 
#******* ******* >> GO TO 00_R_code_exam.r <<
#******* **** FOR A FINALIZED VERSION
#******* ** OF THIS ANALYSIS
#******* * THANKS * 
#*******

####### Prequel Pre-post fire analysis Palermo August 2023 ####### 


# The project focuses on the fires that occurred in the area of Palermo, Sicily between July and August 2023
# The fire caused evacuations, lack of electricity/water and loss of biodiversity 
# I compared the images of08/2022 and 08/2023. 2021 images were analyzed and excluded because approximable to 2021 
# The project might spark Interest on potential long-term analysis on arson in Sicily
# The area examined ranges between Terrasini and Termini Imerese and covers up to Piana degli Albanesi. The Ficuzza wood was not affected
# This code processes landsat 8-9 images downloaded from https://earthexplorer.usgs.gov/


# Summary 
# 1. Libraries, Data Import and Visualization 
# 2. Spectral Indices
# 3. Classification 
# 4. Land Surface Temperature
# 5. 

# -------------------------------------------------- #

# 1. Libraries, Data Import and Visualization 

library(raster)  
library(ggplot2)
library(RStoolbox)
library(patchwork)
library(viridis)

# Set working directory
setwd("~/Desktop/nuovo/20m")

# data import and processing
# 2022 
rlist_22 <- list.files(pattern = "2022") 
import_22 <- lapply(rlist_22, raster)
tgr_22 <- stack(import_22)
ext <- c(330000, 371000, 4200220, 4237800)
crop_22 <- crop(tgr_22, ext) 
#plotRGB(crop_22, r=4, g=3, b=2, stretch="lin")

# 2023
rlist_23 <- list.files(pattern = "2023") 
import_23 <- lapply(rlist_23, raster)
tgr_23 <- stack(import_23)
crop_23 <- crop(tgr_23, ext) 
#plotRGB(crop_23, r=4, g=3, b=2, stretch="lin")
#plotRGB(crop_23, r=6, g=4, b=3, stretch="lin") # False colors

#Bands for both images
# Blue : band 2 - B02 file
# Green: band 3 - B03 file 
# Red: band 4 -  B04 file
# NIR: band 6 - B0 file
# MIR: band 10 - B012


# Image visualization
# True color visualization
t_2022 <- ggRGB(crop_22, r=4, g=3, b=2, stretch="lin") + ggtitle("2022") +
  theme(axis.text = element_blank(), axis.title = element_blank())
t_2023 <- ggRGB(crop_23, r=4, g=3, b=2, stretch="lin") + ggtitle("2023") +
  theme(axis.text = element_blank(), axis.title = element_blank())

t_2022 + t_2023

# False Colors > NIR on Red 
f_2022 <- ggRGB(crop_22, r=6, g=4, b=3, stretch="lin") + ggtitle("2022")+
  theme(axis.text = element_blank(), axis.title = element_blank())
f_2023 <- ggRGB(crop_23, r=6, g=4, b=3, stretch="lin") + ggtitle("2023")+
  theme(axis.text = element_blank(), axis.title = element_blank())

f_2022 + f_2023

#--------------------------------------------#

# 2. Spectral Indices

# Vegetation health measure: DVI e NDVI calculation for 2022 and 2023 using NIR and RED bands
# Burned areas analysis with NBR index using NIR and MIR bands
# Creating a customized color palette
cld <- colorRampPalette(c('darkblue', 'white', 'red'))(100) 

## DVI (Difference Vegetation Index): NIR - RED

#dvi22 = crop_22[[4]] - crop_22[[3]]
dvi22 = crop_22[[6]] - crop_22[[4]]
plot(dvi22, col=cld, main="DVI 2021")

dvi23 = crop_23[[6]] - crop_23[[4]]
plot(dvi23, col=cld, main="DVI 2021")

# Multiframe con i DVI per le due immagini

#min_value_dvi <- min(min(values(dvi22_20m)))
#max_value_dvi <- max(max(values(dvi22_20m)))

# multiframe 
par(mfrow=c(1,2))
plot(dvi22, col=cld)
plot(dvi23, col=cld)

# Positive Value: reflectance in the near-infrared (NIR) band is higher than that in the red (RED) band
# This can be associated with healthy vegetation and dense vegetation, as healthy plants reflect more NIR light for photosynthesis

# Zero or Near-Zero Value: reflectance in the NIR band is similar to that in the red band
# This could indicate the presence of bare soil or poorly developed vegetation cover

# Negative Value: reflectance in the NIR band is lower than that in the red band
# This could be due to various factors such as exposed soil, water, or the presence of snow

dvi_dif = dvi23 - dvi22

# plot DVI difference and export as pdf
cldf <- colorRampPalette(c("beige", "blue3", 'white', "brown2", "yellow"))(100)

pdf("dvi-dif23-22")
par(mfrow=c(1,2))
plotRGB(crop_23, r=4, g=3, b=2, stretch="lin", main="True Color Image 2023")
plot(dvi_dif, col=cldf, axes = FALSE, box = FALSE, main="DVI Difference 2022-23")
dev.off()

# A positive difference between DVI values of the two years indicates that the vegetation health and density have likely increased from 2022 to 2023
# A negative difference between DVI values suggests a decrease in vegetation health and density (see burned area)

## NDVI: (NIR - RED) / (NIR + RED)

ndvi22 =  dvi22 / (crop_22[[6]] + crop_22[[4]])
ndvi23 =  dvi23 / (crop_23[[6]] + crop_23[[4]])

# Plot and export as pdf

pdf("ndvi-22-23_2")
par(mfrow=c(1,2))
plot(ndvi22, col=clb, axes = FALSE, box = FALSE, main="NDVI 2022")
plot(ndvi23, col=clb, axes = FALSE, box = FALSE, main="NDVI 2023")
dev.off()

ndvi_dif = ndvi23 - ndvi22
plot(ndvi_dif, col=cldf, axes = FALSE, box = FALSE) + title(main="NDVI Difference 2022 - 2023")

# NBR (Normalized Burn Ratio) = (NIR - MIR) / (NIR + MIR)
clb <- colorRampPalette(c("white", "darkgreen", "yellow", "orange"))(100)

#2022
NBR_2022 = (crop_22[[6]] - crop_22[[10]]) / crop_22[[6]] + crop_22[[10]]
plot(NBR_2022, col = clb) + title(main="NBR 2022") 

#2023
NBR_2023 = (crop_23[[6]] - crop_23[[10]]) / crop_23[[6]] + crop_23[[10]]
plot(NBR_2023, col = clb) + title(main="NBR 2023") 

# opzione 2 più contrasto 
# min_value <- min(values(NBR_2022_20m))
# max_value <- max(values(NBR_2022_20m))

# normalize the scale 
min_value <- min(min(values(NBR_2022)), min(values(NBR_2023)))
max_value <- max(max(values(NBR_2022)), max(values(NBR_2023)))

clb2 <- colorRampPalette(c("white","darkgreen", "yellow", "orange"))(100)

par(mfrow=c(1,2))
plot(NBR_2022, col = clb2, main = "NBR index 2022", zlim = c(min_value, max_value))
plot(NBR_2023, col = clb2, main = "NBR index 2023", zlim = c(min_value, max_value))

#--------------------------------------------#

# 3. Classification

set.seed(42) 
clas22 <- unsuperClass(crop_22, nClasses=5)
clas23 <- unsuperClass(crop_23, nClasses=5)

clr <- colorRampPalette(c("white", "darkorchid", "darkslateblue", "gold2", "palegreen4"))(100)

# 2022
par(mfrow=c(1,2))
plot(clas22$map, col=clr) + title(main="Soil Classification 2022")
plotRGB(crop_22, r=4, g=3, b=2, stretch="lin")

# frequencies of classes
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
percent_veg_22 # 48.46533 % VEGETATION

percent_cities_rocks_22 <- 237362 * 100 / tot_land_22
percent_cities_rocks_22 # 9.514727 % CITIES/ROCKS

tot_dry_22 <- 1048263 
percent_dry_22 <- tot_dry_22 * 100 / tot_land_22
percent_dry_22 # 42.01994 % DRY LAND

# 2023
par(mfrow=c(1,2))
plot(clas23$map, col=clr) + title(main="Soil Classification 2023")
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

## classes and frequencies representation

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
  scale_fill_manual(values=c("honeydew3", "indianred3", "darkolivegreen")) + 
  ylim(0, 100) +
  theme_minimal()

is2 <- ggplot(df1, aes(x=cl_23, y=perc23, fill=cl_23)) +  
  geom_bar(stat="identity") +
  ggtitle("Land Cover percentage in 2023") +
  labs(fill="Classes", x="Classes", y="%") +
  geom_text(aes(label=perc23), vjust=-0.3, size=3.5)+
  scale_fill_manual(values=c("honeydew3", "indianred3", "darkolivegreen")) + 
  ylim(0, 100) +
  theme_minimal()

is1 + is2

#--------------------------------------------#

# 4. Land Surface Temperature Analysis

setwd("~/Desktop/nuovo/LST")
clLst <- colorRampPalette(c("blue", "light blue", "pink", "red")) (100)

lst_22 <- raster("LC08_L2SP_189034_20220908_20220914_02_T1_ST_B10.TIF")
lst_23 <- raster("LC09_L2SP_189034_20230818_20230822_02_T1_ST_B10.TIF")

crop_t_22 <- crop(lst_22,ext)
crop_t_23 <- crop(lst_23,ext)

# Plot but different scale 
par(mfrow=c(1,2))
plot(crop_t_22, col = rev(heat.colors(100)))
plot(crop_t_23, col = rev(heat.colors(100)))

# unify scale
min_value_t2 <- min(min(values(crop_t_22)), min(values(crop_t_23)))
max_value_t2 <- max(max(values(crop_t_22)), max(values(crop_t_23)))

#plot 
par(mfrow=c(1,2))
plot(crop_t_22, col = rev(heat.colors(100)), zlim = c(min_value_t2, max_value_t2))
plot(crop_t_23, col = rev(heat.colors(100)), zlim = c(min_value_t2, max_value_t2))

# come transformo in temperature C°? 
# è corretto unificare le scale  delle scale? 
# posso lavoare su un progetto migliore? 
# altri indici? 
# fitoplancton 

#--------------------------------------------#

