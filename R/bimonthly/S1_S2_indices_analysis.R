library(tidyr)
library(lubridate)
library(scales)
library(ggthemes)
library(stringr)
library(dplyr)
library(ggplot2)
library(data.table)

setwd("/Users/brbell01/Google Drive/PHD/CUNY/Research/R/Phenology/Bimonthly")
latlon <- read.csv('hectares_latlon.csv', header = TRUE, sep = ',', colClasses = "character") 

#params
#supply a valid plot ID
hect_id <- "02"

#Import data
bimonthlyEVI_SR <- read.csv(paste0("medianEVI_hectareID_", hect_id, ".csv"), header = FALSE, sep = ',')
bimonthlyEVI_TOA <- read.csv(paste0("medianEVI_TOA_hectareID_", hect_id, ".csv"), header = FALSE, sep = ',')
bimonthlyVH <- read.csv(paste0("VH_hectareID_", hect_id, ".csv"), header = FALSE, sep = ',')
bimonthlyRatio <- read.csv(paste0("VHVV_ratio_hectareID_", hect_id, ".csv"), header = FALSE, sep = ',')

#Headers
names(bimonthlyEVI_SR)[1] <- "Date"
names(bimonthlyEVI_TOA)[1] <- "Date"
names(bimonthlyVH)[1] <- "Date"
names(bimonthlyRatio)[1] <- "Date"
names(bimonthlyEVI_SR)[2] <- "Median_EVI_SR"
names(bimonthlyEVI_TOA)[2] <- "Median_EVI_TOA"
names(bimonthlyVH)[2] <- "Median_VH"
names(bimonthlyRatio)[2] <- "Median_Ratio"

#Apply date formatting
bimonthlyEVI_SR$Date <- ymd(bimonthlyEVI_SR$Date)
bimonthlyEVI_TOA$Date <- ymd(bimonthlyEVI_TOA$Date)
bimonthlyVH$Date <- ymd(bimonthlyVH$Date)
bimonthlyRatio$Date <- ymd(bimonthlyRatio$Date)

#Merge Sentinel-2 EVI data
bimonthly <- merge(bimonthlyEVI_SR,bimonthlyEVI_TOA,by="Date",all=TRUE) %>%
  merge(bimonthlyVH, by="Date",all=TRUE) %>%
  merge(bimonthlyRatio, by="Date",all=TRUE)

##make a lookup table for geographic coordinates of the plot locations
colargs <- rep("right", length(names(latlon)))
latlon <- latlon %>% 
  select(Hectare_ID, left, right, top, bottom) %>%
  apply(2, strtrim, 10) %>% data.frame() %>%
  mutate(coords = str_c(left,top, sep = ", ", collapse = NULL),
         left = NULL, 
         right = NULL,
         top = NULL, 
         bottom = NULL)

#extract coordinates
coords_lookup <- latlon[latlon$Hectare_ID %in% hect_id, ][2]

#Second axis overplotting parameters
ylim.prim <- c(-0.5, 1) 
ylim.sec <- c(-15,10)
a <- ylim.prim[1] + 50 #needs to be adjusted for nice fit
scalingfactor <- 5 #to vary the spread

#Plot time series data
bimonthly %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = Median_EVI_SR, color = "Median_EVI_SR")) + 
  geom_line(aes(y = Median_EVI_TOA, color = "Median_EVI_TOA")) +
  geom_line(aes(y = bimonthly$Median_Ratio, color = "Median_VHVV_Ratio")) +
  scale_color_manual("", 
                    breaks = c("Median_EVI_SR", "Median_EVI_TOA", "Median_VHVV_Ratio"),
                    values = c("green", "grey", "purple")) +
  xlab("Month, Year")  +
  theme_tufte(base_size = 16) +
  theme(aspect.ratio = 1 / 2
        , legend.position="top"
        , axis.text.x = element_text(angle = 45
#                                     ,vjust = -0.6, hjust = -1.1)
                                    )
        , panel.grid.minor.x = element_line(color = "gray80", size = 0.3) 
        , panel.grid.major.x = element_line(color = "gray50", size = 0.5)
        , panel.grid.minor.y = element_line(color = "gray90", size = 0.5)
        , panel.grid.major.y = element_line(color = "gray90", size = 0.5)
        ) +
        scale_x_date(date_labels = "%b, %Y"
#              , limits = ymd(c("2016-07-01", "2021-12-01"))
               , date_breaks = "6 months"
               , minor_breaks = "1 month"
               , expand = expansion(add = .05) 
        ) +
  scale_y_continuous("Median EVI, Sentinel-2", limits = c(0, 1)
                     , breaks = seq(0,1,0.1)
                     , sec.axis = sec_axis(~ (.)
                     , name = "Median VH/VV Backscatter Ratio, Sentinel-1")) +
  ggtitle(paste0("Median Bimonthly Index Values over Seasonal Semi-Deciduous Forest 
                (1-ha plot at ", coords_lookup, ")"))
