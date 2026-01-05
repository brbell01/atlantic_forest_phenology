library(tidyr)
library(knitr)
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
library(scales)
library(grid)

setwd("/Users/brbell01/Google Drive/PHD/CUNY/Research/R/Phenology/raw/")
wd <- getwd()

#parameters
foresttype <- "Seasonal Semi-deciduous Forest"
studyarea <- "REBIO Mata Escura, MG"
satellite <- "S1"
bandname <- "VH"  # "VH" ; "VV"  ; "VHVV_ratio"
rainfall_name <- "IMERG"
rainfall_filename <- "IMERG_Daily_max_Mata_Escura.csv"
scale <- "1ha" # "25ha", "625ha", "Park"

#Import max. hourly rate (reported daily) rainfall data ; rainfall is raw and filtered for values 
rainfall <- read.csv(paste0(wd, "/", rainfall_name,"/", rainfall_filename), header=T)
rainfall$date <- ymd(rainfall$date)

#import s1 data
#tables <- dir(paste0(wd, "/", satellite, "/", scale, "/"), full.names = T, pattern = paste0("mata_escura_s1_", scale)) %>% map(read.csv, header=F)
#tables <- read.csv(paste0(wd, "/", satellite, "/", scale, "/", "mata_escura_s1_", scale, ".csv"))

band.list <- c("VH", "VV", "VHVV_ratio")
tables <- vector(mode = "list", length = 0)
for (band in band.list) {
  table <- read.csv(paste0(wd, "/s1/", scale, "/", studyarea_base, "_s1_", band, "_", scale,   
                           "_processed.csv"))
  table <- table[,!names(table) %in% c("X")] %>% mutate(date = ymd(date))
  tables[[band]] <- table
}

#create a function to make a dataframe from each of the data elements in the imported list
make_index_df <- function(df) {
df1 <- df %>% lapply("[[",1)
mat <- gsub("^\\[|\\]$", "", df1[2])
mat <- gsub("^\\[|\\]$", "", mat)
mat <- strsplit(mat, "\\], \\[")[[1]]
mat <- data.frame(date=sub(",.*$", "", mat), VH=sub("^.*, ", "", mat), stringsAsFactors=FALSE) 
mat$date <- ymd(mat$date)
}

#tables[2] %>% make_index_df()

#names(tables) <- c("Hectare_ID", "VH", "VV", "VHVV_ratio")
# s1_raw <- tables # %>% select(date)

#process rainfall data  
rainfall_raw <- rainfall
#rainfall_nonzero  <- rainfall %>%
#        filter(precipitationCal != 0) %>% arrange(precipitationCal)
        # filter out only non-zero values
#rainfall_heavy <- rainfall  %>% 
#        filter(precipitationCal >= 10)  # filter out large values which can be considered rain

#get summary data for all plots
tablemeans <- vector(mode = "list", length = 0)
for (band in band.list) {
  means <- tables[[band]] %>% gather("Hectare_ID", value, -date) %>% group_by(date) %>%     
    filter(!grepl('sd', Hectare_ID)) %>%
    dplyr::summarize(mean(value, na.rm=TRUE)) %>% mutate(date = ymd(date))
  names(means) <- c("date", paste0(bandname,"_hectare_means"))
  tablemeans[[band]] <- means
}

#scale for plotting
ylims <- c(-14,-11)
rainfall_nonzero$precipitationCal-> p
precipitationCal_rescaled <- rescale(p, to=ylims)precipitationCal_rescaled 
rainfall_nonzero <- cbind(rainfall_nonzero, precipitationCal_rescaled)
breaks <- floor(seq(from = min(precipitationCal_rescaled), to = max(precipitationCal_rescaled), by=0.5))
obreaks <- floor(seq(from = min(p), to = max(p), by=1)) ; obreaks

#rescale VH values

ylims <- c(min(p),max(p))
tablemeans[[bandname]]$VH_hectare_means -> VH
VH_rescaled <- rescale(VH, to=ylims)
breaks <- floor(seq(from = min(VH_rescaled), to = max(VH_rescaled), by=1))

ggplot() +
  geom_line(data = tablemeans[[bandname]], aes(x=date, y = VH_rescaled), size=0.25, color = "grey50") +
  #geom_point(data = rainfall_heavy, aes(x=date, y = precipitationCal_rescaled), size=1.5, shape = 21) +
  geom_col(data = rainfall_nonzero, aes(x=date, y = p), color = "blue", alpha = 0.5) +
  xlab("Month, Year")  +
  theme_minimal(base_size = 16) +
  scale_x_date(date_labels = "%b, %Y"
               , date_breaks = "3 months"
               , minor_breaks = "1 month"
              , expand = expansion(add = .05)) +
  theme(axis.text.x=element_text(angle=90)
        , panel.grid.minor.y = element_line(color = "gray90", size = 0.5)
        , panel.grid.major.y = element_line(color = "gray90", size = 0.5)) +
  scale_y_continuous(limits=ylims, label = VH, name = "VH index values (dB)"
             #        , breaks = breaks
                     , sec.axis = sec_axis(~.+0
                                           , name="Max. Daily Precip. (mm/Hr)"))
                 #                          , breaks = prettybreaks()))

plot1 <- tablemeans[[bandname]] %>%
  ggplot() +
  geom_line(aes(x = date, y = VH_hectare_means), size=0.25, color = "grey50") +
  ylab("VH index values (dB)") +
  theme_minimal(base_size = 16) +
  scale_x_date(date_labels = "%b, %Y"
               , date_breaks = "3 months"
               , minor_breaks = "1 month"
               , expand = expansion(add = .05)) +
  theme(axis.title.x = element_blank(), axis.text.x=element_text(angle=90)
        , panel.grid.minor.y = element_line(color = "gray90", size = 0.5)
        , panel.grid.major.y = element_line(color = "gray90", size = 0.5))

plot2 <- rainfall_nonzero %>%
  ggplot() +
  geom_col(aes(x = date, y = precipitationCal), color = 'blue', alpha = 0.5) +
  ylab("Max. Daily Precip. (mm/Hr)") +
  theme_minimal(base_size = 16) +
  scale_x_date(date_labels = "%b, %Y"
               , date_breaks = "3 months"
               , minor_breaks = "1 month"
               , expand = expansion(add = .05)) +
  scale_y_continuous(position = "right") +
  theme(axis.title.x = element_blank(), axis.text.x=element_text(angle=90)
        , panel.grid.minor.y = element_line(color = "gray90", size = 0.5)
        , panel.grid.major.y = element_line(color = "gray90", size = 0.5))

grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2)))

## test correlation between the two time series (Granger causality)
rainfall <- xts(x = rainfall$precipitationCal, order.by = rainfall$date)
VH <- xts(x = tablemeans[[bandname]]$VH_hectare_means, order.by = tablemeans[[bandname]]$date)
# na.locf(merge(rainfall, VH))[time(rainfall)]
rainfall_backscatter <- merge(rainfall, VH)
  
library(lmtest)
library(data.table)
grangertest(VH ~ rainfall, order = 3, data = rainfall_backscatter, na.action = na.omit) %>% kable(digits = 3)
