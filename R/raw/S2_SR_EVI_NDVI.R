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

setwd("/Users/brbell01/Google Drive/PHD/CUNY/Research/R/Phenology/raw")
wd <- getwd()

#parameters
foresttype <- "Seasonal Semi-deciduous Forest"
studyarea <- "REBIO Mata Escura, MG"
bandname <- "EVI"  # "NDVI" 

#Import data 

tables <- dir(wd, full.names = T, pattern = paste0(bandname,"_hectareID_")) %>% map(read.csv, header=F)
for (i in 1:length(tables)) {
  names(tables[[i]])[1] <- "Date"
  names(tables[[i]])[2] <- paste0("Median_",bandname)
  tables[[i]]$Date <- ymd(tables[[i]]$Date)
}

tables <- tables  %>% reduce(full_join, by = "Date") 

colnames <- list.files(pattern = paste0(bandname,"_hectareID_")) %>%
  str_sub(11, 22) %>% append("Date") %>% str_sort()
names(tables) <- colnames

#prep for plotting
tables.plot <- melt(tables, id = "Date") %>% as.data.frame() %>% 
  mutate(doy = ymd(paste0("1901-", month(Date), "-", day(Date)))) %>%
  na.omit() %>% filter(value > 0) %>% distinct() 

tables.plot$value <- as.numeric(tables.plot$value)
ylims <- range(tables.plot$value)
#ylims <- c(0,2)

tables.plot <- tables.plot %>% na.omit()

#plot time series 
ggplot(data=tables.plot, aes(x = Date, y = value)) +  
  geom_point(aes(colour = variable, shape = variable)) +
  xlab("Month, Year")  +
  theme_tufte(base_size = 16) +
  theme(aspect.ratio = 1 / 2
        ,legend.position="top"
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
  scale_y_continuous(paste0(bandname," value, Sentinel-2")
                     , limits = ylims
                     , breaks = pretty_breaks()) + 
  ggtitle(paste0(bandname," Sentinel-1 surface reflectance values \n for ", length(tables)-1, 
                 " 1-ha plots over ", foresttype, ",\n", studyarea, ", Brazil"))

#plot time series stacked by year
tables.plot %>% distinct() %>% 
  mutate(doy = ymd(paste0("1901-", month(Date), "-", day(Date)))) %>%
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
               , limits = ymd(c("1901-11-01", "1901-12-31"))
               , date_breaks = "1 month"
               , minor_breaks = "1 day"
               , expand = expansion(add = .05) 
  ) +
  geom_vline(xintercept = c(ymd("1901-04-01"), ymd("1901-07-01"), ymd("1901-10-01"))) +
  scale_y_continuous(paste0(bandname," value")
                     , limits = ylims
                     , breaks = pretty_breaks()) + 
  ggtitle(paste0("Sentinel-2 ", bandname, " surface reflectance values \n for ", length(tables)-1, 
                 " 1-ha plots over ", foresttype, ",\n", studyarea, ", Brazil"))


#data exploration
#summary stats
tables.plot %>%
  select(variable, value) %>%
  summarise(mean = mean(value, na.rm = T), median = median(value, na.rm = T)
            , sd = sd(value, na.rm = T), IQR = IQR(value, na.rm = T)) 

tables.plot %>%
  select(variable, value) %>%
  group_by(variable) %>%
  summarise(mean = mean(value, na.rm = T), median = median(value, na.rm = T)
            , sd = sd(value, na.rm = T), IQR = IQR(value, na.rm = T)) 

#inspect distribution
tables.plot %>%
  ggplot(aes(x = value, color = "variable")) +
  geom_histogram(binwidth = 0.05, fill="white") +
  theme_minimal()

#boxplot
tables.plot %>%
ggplot(aes(x = variable, y = value, color = variable)) +
  geom_boxplot(show.legend = FALSE) +
  scale_y_continuous(limits = ylims, n.breaks = 10) +
  ylab("Sentinel-2 EVI value") +
  xlab("") +
  theme_tufte(base_size = 14) +
  theme(aspect.ratio = 1.5 / 1, panel.grid.major.y = element_line(color = "gray90", size = 0.5))
  

#by season
tables.plot.dry <- tables.plot %>% 
  select(Date, ID = variable, EVI_value = value) %>% 
  filter(month(Date) >= month(as.Date("1901-04-01")) & month(Date) < month(as.Date("1901-10-01"))) %>%
  mutate(season = "dry season")
tables.plot.wet1 <- tables.plot %>% 
  select(Date, ID = variable, EVI_value = value) %>% 
  filter(month(Date) < month(as.Date("1901-04-01"))) %>%
  mutate(season = "wet season")
tables.plot.wet2 <- tables.plot %>% 
  select(Date, ID = variable, EVI_value = value) %>% 
  filter(month(Date) > month(as.Date("1901-10-01"))) %>%
  mutate(season = "wet season")
tables.plot.season <- tables.plot.wet1 %>% rbind(tables.plot.wet2) %>% rbind(tables.plot.dry)

tables.plot.season %>%
  group_by(season) %>%
  ggplot(aes(x = ID, y = EVI_value, color = season)) +
  geom_boxplot() +
  scale_y_continuous(limits = ylims, n.breaks = 10) +
  ylab("Sentinel-2 EVI value") +
  xlab("") +
  theme_tufte(base_size = 14) +
  theme(legend.title = element_blank(), aspect.ratio = 1.5 / 1, panel.grid.major.y = element_line(color = "gray90", size = 0.5))


