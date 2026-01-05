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
library(magrittr)

setwd("/Users/brbell01/Google Drive/PHD/CUNY/Research/R/Phenology/raw")
wd <- getwd()

#parameters
foresttype <- "Seasonal Semi-deciduous Forest"
studyarea <- "REBIO Mata Escura, MG"
bandname <- "VH"  # "VV" 

#Import data 

tables <- dir(wd, full.names = T, pattern = paste0(bandname,"_hectareID_")) %>% map(read.csv, header=F)
for (i in 1:length(tables)) {
  names(tables[[i]])[1] <- "Date"
  names(tables[[i]])[2] <- paste0("Median_",bandname)
  tables[[i]]$Date <- ymd(tables[[i]]$Date)
}

tables <- tables  %>% reduce(full_join, by = "Date") 

colnames <- list.files(pattern = paste0(bandname,"_hectareID_")) %>%
  str_sub(4, 15) %>% append("Date") %>% str_sort()
names(tables) <- colnames

#prep for plotting
ylims <- range(tables.plot$value)
tables.plot <- melt(tables, id = "Date") %>% as.data.frame()

#plot time series 
ggplot(data=tables.plot, aes(x = Date, y = value)) +  
  geom_point(aes(colour = variable, shape = variable))
  xlab("Month, Year")  +
  theme_tufte(base_size = 16) +
  theme(aspect.ratio = 1 / 2
        , legend.position="top"
        , axis.text.x = element_text(angle = 45)
        , panel.grid.minor.x = element_line(color = "gray80", size = 0.3) 
        , panel.grid.major.x = element_line(color = "gray50", size = 0.5)
        , panel.grid.minor.y = element_line(color = "gray90", size = 0.5)
        , panel.grid.major.y = element_line(color = "gray90", size = 0.5)
  ) +
  scale_x_date(date_labels = "%b, %Y"
               , date_breaks = "3 months"
               , minor_breaks = "1 month"
               , expand = expansion(add = .05) 
  ) +
  scale_y_continuous("VH Backscatter (dB), Sentinel-1", limits = ylims
                     , breaks = pretty_breaks()) + 
  ggtitle(paste0(bandname," backscatter (dB) \n for ", length(tables)-1, 
                 " 1-ha plots over ", foresttype, ",\n", studyarea, ", Brazil"))

#plot time series stacked by year
tables.plot <- melt(tables, id = "Date") %>% as.data.frame()

tables.plot %>% mutate(doy = ymd(paste0("1901-", month(Date), "-", day(Date)))) %>%
  filter(year(Date) > 2018) %>%
ggplot(aes(x = doy, y = value)) +  
    geom_point(aes(colour = variable, shape = variable)) +
    facet_grid(year(Date) ~ ., scales = "free_y") + theme(legend.position = "none") +
    xlab("Month")  +
    theme_tufte(base_size = 16) +
  theme(legend.position="top"
        , axis.text.x = element_text(angle = 45)
        , panel.grid.minor.x = element_line(color = "gray80", size = 0.3) 
        , panel.grid.major.x = element_line(color = "gray50", size = 0.5)
        , panel.grid.minor.y = element_line(color = "gray90", size = 0.5)
        , panel.grid.major.y = element_line(color = "gray90", size = 0.5)
  ) +
  scale_x_date(date_labels = "%b %d"
               , limits = ymd(c("1901-06-01", "1901-07-31"))
               , date_breaks = "1 month"
               , minor_breaks = "1 day"
               , expand = expansion(add = .05) 
  ) +
  geom_vline(xintercept = c(ymd("1901-04-01"), ymd("1901-07-01"), ymd("1901-10-01"))) +
  scale_y_continuous(paste0(bandname," Backscatter (dB)")
                     , breaks = pretty_breaks()) + 
  ggtitle(paste0("Sentinel-1 ", bandname, " backscatter (dB) \n for ", length(tables)-1, 
                 " 1-ha plots over ", foresttype, ",\n", studyarea, ", Brazil"))
  