library(tidyr)
library(lubridate)
library(scales)
library(ggthemes)
library(ggtext)
library(stringr)
library(dplyr)
library(ggplot2)
library(purrr)
library(data.table)

setwd("/Users/brbell01/Google Drive/PHD/CUNY/Research/R/Phenology/Bimonthly")
wd <- getwd()

#parameters
forestype <- "Seasonal Semi-deciduous Forest"
studyarea <- "REBIO Mata Escura, MG"

#Import data

tables <- dir(wd, full.names = T, pattern = "medianEVI_TOA_hectareID_") %>% map(read.csv, header=F)
for (i in 1:length(tables)) {
  names(tables[[i]])[1] <- "Date"
  names(tables[[i]])[2] <- "medianEVI_TOA_hectareID_"
  tables[[i]]$Date <- ymd(tables[[i]]$Date)
}

tables <- tables  %>% reduce(full_join, by = "Date") 

colnames <- list.files(pattern = "medianEVI_TOA_hectareID_") %>%
  str_sub(15, 26) %>% append("Date") %>% str_sort()
names(tables) <- colnames

#prep for plotting
tables.plot <- melt(tables, id = "Date")

#create summary stats 
tables.stats <- tables  %>%
  rowwise() %>% 
  mutate(medianEVI_all = median(c_across(starts_with("hectareID")), na.rm = TRUE)) %>%
  rowwise() %>% 
  mutate(iqrEVI_all = IQR(c_across(starts_with("hectareID")), na.rm = TRUE)) %>% 
  select(Date, medianEVI_all, iqrEVI_all)

latlon <- read.csv('hectares_latlon.csv', header = TRUE, sep = ',', colClasses = "character") 

#Plot time series data for the summary stats

ggplot(data=tables.stats, aes(x = Date, y = medianEVI_all)) +  
  geom_line() +
  geom_ribbon(aes(ymin = medianEVI_all - iqrEVI_all, ymax = medianEVI_all + iqrEVI_all)
              , alpha=0.2) +
  scale_fill_manual(values = "grey20", name = "fill") +
  xlab("Month, Year")  +
  theme_tufte(base_size = 16) +
  theme(aspect.ratio = 1 / 2
        , legend.position="top"
        , axis.text.x = element_text(angle = 45
                                     #       ,vjust = -0.6, hjust = -1.1)
        )
        , panel.grid.minor.x = element_line(color = "gray80", size = 0.3) 
        , panel.grid.major.x = element_line(color = "gray50", size = 0.5)
        , panel.grid.minor.y = element_line(color = "gray90", size = 0.5)
        , panel.grid.major.y = element_line(color = "gray90", size = 0.5)
  ) +
  scale_x_date(date_labels = "%b, %Y"
               #              , limits = ymd(c("2016-07-01", "2021-12-01"))
               , date_breaks = "3 months"
               , minor_breaks = "1 month"
               , expand = expansion(add = .05) 
  ) +
  scale_y_continuous("Median EVI, Sentinel-2 (L1C)", limits = c(0, 1)
                     , breaks = seq(0,1,0.1)) + 
  ggtitle(paste0("Median (+ IQR) Bimonthly EVI Top of Atmosphere Reflectances Values \n for ", length(tables)-1, " 1-ha plots over Seasonal Semi-Deciduous Forest, Brazil"))
