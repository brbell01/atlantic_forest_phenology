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
season_directory <- "/output/third_run/"
TS_directory <- "/formatted_timeseries/third_run/"

# Definitions
S2_int <- 5 # Sentinel-2 repeat interval (days)
S1_int <- 12 # Sentinel-1 repeat interval (days)
S2_start_date <- as.Date("2019-01-04")  
S1_start_date <- as.Date("2016-01-10")
S1_band <- "RVI"
S2_band <- "EVI2" # "EVI"  ;  "NDVI"

# rTIMESAT functions (doesn't work for me)
# file <- system.file("rio_doce_RVI_seasonality.tpa", package = "rTIMESAT")
# d_tpa <- rTIMESAT::read_tpa(file, t=NULL)

# Read original time series data files
dir <- dir(paste0(wd, TS_directory, "S2"), full.names = T, pattern = paste0(S2_band,"_processed.txt"))
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
S2_fitted <- dir(paste0(wd, season_directory, "S2"), full.names = T, pattern = paste0(S2_band,"_STL_season.txt")) %>% map(fread, sep=" ")
S2_fitted_names <- paste0(S2_raw_names,"_fitted")
S2_trend <- dir(paste0(wd, season_directory, "S2"), full.names = T, pattern = paste0(S2_band,"_STL_trend.txt")) %>% map(fread, sep=" ")
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
  write.csv(summary, paste0(wd, season_directory, "S2/", filename_base, "_R2.csv"))
}

#cleanup
summarytable <- summarytable[, -c(1)]

#write out summary file
write.csv(summarytable, paste0(wd, season_directory, "S2/", "S2_", S2_band, "_timesat_model_fit_stats.csv"))


# Read season data from Timesat output seasonality file
S2_season <- dir(paste0(wd, season_directory, "S2/"), full.names = T, pattern = paste0(S2_band,"_seasonality.txt")) %>% map(fread, skip=2, sep=" ")
# S2_season_vars <- dir(paste0(wd, season_directory, "S2"), full.names = T, pattern = "seasonality.txt") %>% map(read.fwf, skip=1, widths=(c(6,9,12,11,14,13,15,16,12,13,13,13,13,13)), header=F)
S2_season_vars <- c("Seas.", "Start t.", "End t.", "Length", "Base val.","Peak t.","Peak val.", "Ampl.", "L.deriv.", "R.deriv.", "L.integral", "S.integral", "Start val.", "End val.")

study_site_names<- c("mata_escura","monte_pascoal","rio_doce","sooretama")
for (i in 1:length(S2_season)) {
          names(S2_season[[i]]) <- S2_season_vars
          S2_season[[i]] <- S2_season[[i]] %>% data.frame()
          S2_season[[i]] <- S2_season[[i]] %>% 
                              mutate(season_start_date = S2_start_date + round(`Start.t.`*S2_int)) %>% 
                              mutate(season_end_date = S2_start_date + round(`End.t.`*S2_int)) %>%
                              mutate(peak_date = S2_start_date + round(`Peak.t.`*S2_int)) %>%
                              mutate(length_days = round(Length*S2_int)) %>%
                              mutate(phase = peak_date - floor_date(peak_date, unit="year"))
          #write out seasonality file
          write.csv(S2_season[[i]], paste0(wd, season_directory, "S2/", study_site_names[[i]],"_S2_", S2_band, "_seasonality_processed.csv"))
}

# Plot time series
### Choose study site 
study_site <-"sooretama" # "sooretama" # "rio_doce" # "monte_pascoal" # "mata_escura"
study_siteID <- match(study_site,study_site_names)
S2_plot <- S2_combined[[study_siteID]] %>% pivot_longer(cols = contains(S2_band)) 
S2_plot$name <- S2_plot$name %>% sub(paste0(study_site,"_"),"", .)
S2_plot <- S2_plot %>% group_by(name)
S2_season_offset <- S2_int
raw_name <- S2_band ; fit_name <- paste0(S2_band, " fit") ; trend_name <- paste0(S2_band, " trend")
colors = c(raw_name = "blue", fit_name = "red", trend_name = "grey60")
ylims <- c(0,1)
rects <- data.frame(Jans = as.Date(paste0(seq(from=2019, to=2020),"-01-01")),
                   Peaks = S2_season[[study_siteID]]$peak_date-8*S2_season_offset) %>% mutate(Diffvals = Peaks-Jans)

ggplot(data=S2_plot, aes(x = Date, y=value, color = name)) + 
  geom_line() +
  #geom_line(aes(y = value, color = raw_name)) +
  #geom_line(aes(y = value, color = fit_name), lwd=1.5) +
  #geom_line(aes(y = value, color = trend_name), lwd=1.5) +
  geom_point(data = S2_season[[study_siteID]], 
                    aes(x = season_start_date-S2_season_offset, y=`Start.val.`), color="red") +
  geom_point(data = S2_season[[study_siteID]], 
             aes(x = season_end_date-S2_season_offset, y=`End.val.`), color="red") +
  geom_point(size = 1.9, data = S2_season[[study_siteID]], 
             aes(x = peak_date-8*S2_season_offset, y=`Peak.val.`), color="purple") +
  geom_rect(data=rects, inherit.aes=FALSE,
            aes(xmin=Jans,xmax=Peaks,ymin=ylims[1],ymax=ylims[2]), fill = "purple", color = NA, alpha=0.2) +
  geom_text(data=rects, inherit.aes=FALSE, color = "purple",
            aes(label = paste0("phase = ",Diffvals), x=Jans + 150, y=0.95)) +
  #annotate("rect",xmin=as.Date(paste0(seq(from=2016, to=2021),"-01-01")),xmax=peak_date,ymin=-Inf,ymax=Inf, alpha=0.1, fill="black") +
  #  geom_line(aes(y = VHVV_ratio_hectare_medians, colour = "all plots median VHVVV ratio")) +
  #  geom_line(aes(y = RVI_hectare_medians, colour = "all plots median RVI composite")) +
  scale_color_manual(name = "Legend: \n All 1-ha plots \n (median)", values= c("blue", "red", "grey60")) +
  # stat_summary(geom = "line", fun = "median") +
  # stat_summary(geom = "ribbon", fun.data = "mean_cl_normal", alpha = 0.3) +
  # geom_line(data = park_mean, aes(x=date, y=VH_Park), linetype='dotdash', color="grey25", size=0.5) +
  theme_minimal(base_size = 14) +
  scale_x_date(date_labels = "%b,%Y"
               #               , limits = ymd(c("1901-11-01", "1901-12-31"))
               , date_breaks = "3 month"
               #               , minor_breaks = "1 day"
               , expand = expansion(add = .05) 
  ) +
  scale_y_continuous(limits = ylims, breaks = breaks_pretty()) +
  labs(x = "Month, Year", y = paste0("Sentinel-2 (", S2_band, ")")) +
  theme(legend.position = "right"
        , axis.text.x = element_text(angle = 45)) 
  #ggtitle(paste0("Sentinel-1 backscatter (RVI ratio), original and fitted ", 
  #               " 1-ha plots) \nover ", foresttype, ",\n", studyarea, ", Brazil"))

ggsave(paste0(study_site, "_", S2_band, "_timesat_S2.png"), units="in", width=10, height=5, dpi=300, device = 'png')
#dev.off()

# write an options file
#options <- list(
  # Data file list/name
#  file_y = paste0("/Users/brbell01/Google Drive/PHD/CUNY/Research/R/Phenology/timesat/formatted_timeseries/third_run/",
#    c("mata_escura_s2_1ha_EVI2_processed.txt"),             
#  file_qc              = "",             # Mask file list/name
#  nyear_and_nptperear = c(6, 30),      # No. years and no. points per year
#  ylu                 = c(-50, 1000),     # Valid data range (lower upper)
#  qc_1                = c(-1e+06, 1e+06, 1),     # Quality range 1 and weight
#  qc_2                = c(-1e+06, 1e+06, 1),   # Quality range 2 and weight
#  qc_3                = c(-1e+06, 1e+06, 1),   # Quality range 3 and weight
#  A                   = 0,            # Amplitude cutoff value
#  output_type         = c(1, 1, 0),     # Output files (1/0 1/0 1/0), 1: seasonality data; 2: smoothed time-series; 3: original time-series
#  seasonpar           = 1.0,            # Seasonality parameter (0-1)
#  iters               = 2,              # No. of envelope iterations (3/2/1)
#  FUN                 = 1,              # Fitting method (1/2/3): (SG/AG/DL)
#  half_win           = 4,              # half Window size for Sav-Gol.
#  meth_pheno          = 1,              # Season start / end method (4/3/2/1)
#  trs                 = c(0.5, 0.5)     # Season start / end values
#)
#opt <- update_setting(options)
#print(str(opt))
#write_setting(opt, "S1_settings_third_run.set")


