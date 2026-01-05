# This script formats a settings file to run a series of time series fitting jobs

# library(rTIMESAT)
library(knitr)
library(tidyr)
library(lubridate)
library(clock)
library(stringr)
library(dplyr)
library(data.table)
library(tibble)
library(purrr)
library(magrittr)
library(padr)
library(ggplot2)
library(scales)

# working directory
setwd("/Users/brbell01/Google Drive/PHD/CUNY/Research/R/Phenology/timesat/")
wd <- getwd()
season_directory <- "/output/third_run/S2/"
TS_directory <- "/formatted_timeseries/third_run/S2/"

# Definitions
S2_int <- 5 # Sentinel-2 repeat interval (days)
S2_start_date <- as.Date("2019-01-04")  
S2_band <- "EVI2" # "EVI"  ;  "NDVI" ; "EVI2"

# rTIMESAT functions (doesn't work for me)
# file <- system.file("rio_doce_RVI_seasonality.tpa", package = "rTIMESAT")
# d_tpa <- rTIMESAT::read_tpa(file, t=NULL)

# Read original time series data files
dir <- dir(paste0(wd, TS_directory), full.names = T, pattern = paste0(S2_band,"_processed.txt"))
S2_raw <- dir %>% map(read.csv, header=T)
cutposition <- dir %>% gregexpr('/',.) %>% unlist() %>% tail(n=1)
S2_raw_names <- sub('_processed.txt', '', dir) %>% substring(cutposition+1, nchar(.))
for (i in 1:length(S2_raw)) {
  names(S2_raw[[i]]) <- S2_raw_names[i]
  len <- S2_raw[[i]][,1] %>% length()
  dates <- seq(as.Date(S2_start_date), by = paste0(S2_int, " days"), length.out=len)
  S2_raw[[i]] <- S2_raw[[i]] %>% mutate(Date = dates) %>% relocate(Date)
}

# Read fitted time series and trend data files and append
S2_fitted <- dir(paste0(wd, season_directory), full.names = T, pattern = paste0(S2_band,"_STL_season.txt")) %>% map(fread, sep=" ")
S2_fitted_names <- paste0(S2_raw_names,"_fitted")
S2_trend <- dir(paste0(wd, season_directory), full.names = T, pattern = paste0(S2_band,"_STL_trend.txt")) %>% map(fread, sep=" ")
S2_trend_names <- paste0(S2_raw_names,"_trend")
S2_combined <- list()
for (i in 1:length(S2_fitted)) {
  S2_fitted[[i]] <- S2_fitted[[i]] %>% t() %>% data.frame()
  names(S2_fitted[[i]]) <- S2_fitted_names[i]
  S2_trend[[i]] <- S2_trend[[i]] %>% t() %>% data.frame()
  names(S2_trend[[i]]) <- S2_trend_names[i]
  len <- S2_fitted[[i]][,1] %>% length()
  dates <- seq(S2_start_date, by = paste0(S2_int, " days"), length.out=len)
  S2_fitted[[i]] <- S2_fitted[[i]] %>% mutate(Date = dates) %>% relocate(Date)
  S2_trend[[i]] <- S2_trend[[i]] %>% mutate(Date = dates) %>% relocate(Date)
  S2_combined[[i]] <- right_join(S2_raw[[i]],S2_fitted[[i]],by="Date")
  S2_combined[[i]] <- left_join(S2_combined[[i]],S2_trend[[i]],by="Date")
}

# Write model error stats
# 1) R-SQUARED error metric -- Coefficient of Determination
RSQUARE <- function(y_actual,y_pred){cor(y_actual,y_pred)^2}
# 2) MEAN ABSOLUTE PERCENTAGE ERROR (MAPE)
MAPE <- function(y_actual,y_pred){mean(abs((y_actual-y_pred)/y_actual))*100}
summarytable <- data.frame(matrix(NA, nrow = 2, ncol = 1))
for (i in 1:length(S2_combined)) {
  actual <- S2_combined[[i]][2] %>% unlist %>% as.numeric()
  pred <- S2_combined[[i]][3] %>% unlist %>% as.numeric()
  # 1 calculate R2
  R2 <- RSQUARE(actual,pred)
  # 2 calculate MAPE
  Mape <- MAPE(actual,pred)
  #write out processed file
  filename_base <- names(S2_combined[[i]][2])
  summary <- c("R2"=R2, "MAPE"=Mape) %>% data.frame()
  names(summary) <- names(S2_combined[[i]][2])
  summarytable <- cbind(summarytable, summary)
  write.csv(summary, paste0(wd, season_directory, filename_base, "_R2.csv"))
}

#cleanup
summarytable <- summarytable[, -c(1)]

#write out summary file
write.csv(summarytable, paste0(wd, season_directory, "S2_", S2_band, "_timesat_model_fit_stats.csv"))

#write out summary file
write.csv(S2_combined, paste0(wd, season_directory, "S2", "_timesat_model_combined_data.csv"))



# Read season data from Timesat output seasonality file
S2_season <- dir(paste0(wd, season_directory), full.names = T, pattern = paste0(S2_band,"_seasonality.txt")) %>% map(fread, skip=2, sep=" ")
# S2_season_vars <- dir(paste0(wd, season_directory), full.names = T, pattern = "seasonality.txt") %>% map(read.fwf, skip=1, widths=(c(6,9,12,11,14,13,15,16,12,13,13,13,13,13)), header=F)
S2_season_vars <- c("Seas.", "Start t.", "End t.", "Length", "Base val.","Peak t.","Peak val.", "Ampl.", "L.deriv.", "R.deriv.", "L.integral", "S.integral", "Start val.", "End val.")

study_site_names<- c("mata_escura","monte_pascoal","rio_doce","sooretama")

for (i in 1:length(S2_season)) {
  names(S2_season[[i]]) <- S2_season_vars
  S2_season[[i]] <- S2_season[[i]] %>% data.frame()
  S2_season[[i]] <- S2_season[[i]] %>%
                    mutate(season_start_date = S2_start_date + round(`Start.t.`*S2_int)) %>% 
                    mutate(season_end_date = S2_start_date + round(`End.t.`*S2_int)) %>%
                    mutate(peak_date = S2_start_date + round(`Peak.t.`*S2_int)) %>%
                    mutate(length_days = round(Length*S2_int))
  
  #Define season and phase-shift 3 ways, based on 1) peak date; 2) midpoint of season; 3) ensemble (average of midpoint and peak date)
  # Season and phase definition 1: Peak to Jan. 1
  S2_season[[i]] <- S2_season[[i]] %>% mutate(phase_peak = ifelse(year(peak_date) == year(season_start_date),
                                                                  ymd(peak_date) - floor_date(ymd(peak_date), unit="year"),
                                                                  ymd(peak_date) - floor_date(ymd(peak_date), unit="year") + 365))
  
  # Season and phase definition 2: Season midpoint to Jan. 1
  season_half_length <- (ymd(S2_season[[i]]$season_end_date)-ymd(S2_season[[i]]$season_start_date))/2  # define midpoint
  S2_season[[i]] <- S2_season[[i]] %>% mutate(season_half_date = season_half_length + season_start_date)
  S2_season[[i]] <- S2_season[[i]] %>% mutate(phase_seas = ifelse(year(season_half_date) == year(season_start_date),
                                                                  #case of either one of the phase references falling in the same year
                                                                  ymd(season_half_date) - floor_date(ymd(season_half_date), unit="year"),
                                                                  #case of either one of the phase references falling in a different year
                                                                  ymd(season_half_date) - floor_date(ymd(season_half_date), unit="year")+365))
                                                
  # Season definition 3: Ensemble, midpoint (between peak and seas. midpoint) to Jan. 1
  season_ensemble_length <- (ymd(S2_season[[i]]$season_half_date)-ymd(S2_season[[i]]$peak_date))/2
  S2_season[[i]] <- S2_season[[i]] %>% mutate("season_ensemble_date" = season_ensemble_length + season_start_date)
  S2_season[[i]] <- S2_season[[i]] %>% mutate("phase_ensemble" = ifelse(year(season_ensemble_date) == year(season_start_date),
                                                                       #case of either one of the phase references falling in the same year
                                                                       season_ensemble_date - floor_date(ymd(season_half_date), unit="year"),
                                                                       #case of either one of the phase references falling in a different year
                                                                       season_ensemble_date - floor_date(ymd(season_half_date), unit="year")+365))
  
  #write out seasonality file
  write.csv(S2_season[[i]], paste0(wd, season_directory, study_site_names[[i]],"_S2_", S2_band, "_seasonality_processed.csv"))
}
