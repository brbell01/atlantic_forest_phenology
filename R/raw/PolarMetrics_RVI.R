library(knitr)
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
library(tibble)
library(geojsonio)
library(sf)
library(tidyquant)
library(PolarMetrics)
library(plotrix)
library(padr)

#definitions
setwd("/Users/brbell01/Google Drive/PHD/CUNY/Research/R/Phenology/raw/")
wd <- getwd()

foresttype <-  "Seasonal Semi-deciduous Forest"  # "Seasonal Semi-deciduous Forest"  ; "Broadleaf Evergreen"
studyarea <- "REBIO Sooretama, ES" #  "PE Rio Doce, MG"  # "REBIO Mata Escura, MG"  # ; "PNH Monte Pascoal, BA" ; "REBIO Sooretama, ES"
studyarea_base <- "sooretama" # "rio_doce" # "mata_escura" ; "monte_pascoal"

#import
band.list <- c("VH", "VV", "RVI")
freq <- 6                 # Sentinel-1 revisit time (days)
indexdata <- vector(mode = "list", length = 0)
for (band in band.list) {
  table <- read.csv(paste0(wd, "/s1/1ha/second_run/", studyarea_base, "_s1_1ha_", band, "_processed.csv"))
  table <- table[,!names(table) %in% c("X")] %>% mutate(date = ymd(date))
  indexdata[[band]] <- table
}

indexmeans <- vector(mode = "list", length = 0)
for (band in band.list) {
  means <- indexdata[[band]] %>% gather("Hectare_ID", value, -date) %>% group_by(date) %>%     
    filter(!grepl('sd', Hectare_ID)) %>%
    summarize(mean(value, na.rm=TRUE)) %>% mutate(date = ymd(date))
  names(means)[2] <- paste0(band,"_hectare_means")
  indexmeans[[band]] <- means
}

#choose band
bandname <- "RVI"
plotdata <- indexmeans[[bandname]]

names(plotdata)[2] <- bandname

plotdata <- plotdata %>% pad(interval = "day") 

firstdate <- as.Date("2016-01-04")
lastdate <- as.Date("2021-12-27")

all_dates <- seq(firstdate, lastdate, "days") %>% data.frame()
names(all_dates)<-"date"
plotdata <- left_join(all_dates,plotdata,by="date")
plotdata <- plotdata %>% slice(seq(1, nrow(.), freq))


#plotdata <- plotdata %>% thicken(interval = paste0(12, " days"), start_val = firstdate) %>%
#                     select(-date) %>% rename_at(ncol(.), ~"date") %>% relocate(date) %>% group_by(date) %>% 
#                     summarize(RVI = first(RVI))

plotdata$day <- as.double(difftime(ymd(plotdata$date), ymd("2016-01-01"), units="days"))+1 

dpy <- 364.2                 # Days/yr
c <- 5                     # Num. of years/cycles 
spc <- 60 # floor(dpy/freq)            # Number of samples in one cycle (yr)
t <- as.vector(plotdata$day)  # Days since January 1, 2016
r <- t2rad(t,dpy)          # Transform days of year to radians
v <- as.vector(plotdata$RVI)   # RVI values
vx <- mean(vec.x(r,v), na.rm=TRUE) # Avg horizontal vector
vy <- mean(vec.y(r,v), na.rm=TRUE) # Avg vertical vector
rv_ang <- vec_ang(vx,vy)   # Angle of resultant vec (point of max activity)
av_ang <- avec_ang(rv_ang)  # Angle marking point of least activity
av_idx <- rad2idx(av_ang, spc=spc) # Index (1-spc) marking avg start of yr
ann_cum <- sum_cycle(v,av_idx,spc=spc)$cumsum # Accum. vals within each yr
# Find seasonal beg, end index for the 2nd yr using 15th pctile of cum NDVI
cy <- 2                    # The second yr of data (which is 2001 here)
es.idx <- window_idx(ann_cum,c-1,cy,0.15,0.8)[1] # Idx of ann_cum marking ES
ms.idx <- window_idx(ann_cum,c-1,cy,0.15,0.8)[2] # Idx of ann_cum marking MS
ls.idx <- window_idx(ann_cum,c-1,cy,0.15,0.8)[3] # Idx of ann_cum marking LS
es <- t[es.idx]          # Early growing season day of pheno yr
ems <- t[es.idx]         # Early-mid growing season day of pheno yr
ms <- t[ms.idx]          # Mid (50th %tile) growing season day
lms <- t[ls.idx]         # Late-mid growing season day of pheno yr
ls <- t[ls.idx]          # Late growing season day of pheno yr
Sintv <- ls-es           # Days in the growing season
ann_cum[es.idx:ls.idx]   # Show cumulative NDVI vals for growing season

#plot
dev.new(width=12,height=6)
par(mfrow=c(2,1))
cxs <- 1
plot(2016+(t/dpy), v, pch=20, col="black", xlab="years", ylab="RVI")
lines(2016+(t/dpy), v, col="blue")

write.csv(plotdata, "RVI_sooretama_TS.csv")

