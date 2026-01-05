library(stringi)
library(data.table)
library(tidyr)
library(purrr)
library(lubridate)
library(tidyquant)

#Report the R2 statistic for a given pair of fitted and raw values

#working directory
setwd("/Users/brbell01/Google Drive/PHD/CUNY/Research/R/Phenology/raw/s1/1ha/harmonic")
wd <- getwd()

S1_band <- "RVI" # 
names <- dir(wd, full.names = T, pattern = paste0(S1_band, "_2016_2022_harmonic.csv"))
tables <- names %>% map(read.csv, header=T)

# 1) R-SQUARED error metric -- Coefficient of Determination
RSQUARE <- function(y_actual,y_pred){
  cor(y_actual,y_pred)^2
}

# 2) MEAN ABSOLUTE PERCENTAGE ERROR (MAPE)
MAPE <- function(y_actual,y_pred){
  mean(abs((y_actual-y_pred)/y_actual))*100
}

# all years 
alltable <- data.frame(matrix(NA, nrow = 2, ncol = 1))
for (i in 1:length(tables)) {
  names(tables[[i]])[1] <- "Date"
  actual <- tables[[i]][2] %>% unlist %>% as.numeric()
  pred <- tables[[i]][3] %>% unlist %>% as.numeric()
  # 1 calculate R2
  R2 <- RSQUARE(actual,pred)
  # 2 calculate MAPE
  Mape <- MAPE(actual,pred)
  #write out processed file
  filename_base <- names[[i]] %>% substring(1,nchar(names[[i]])-4)
  summary <- c("R2"=R2, "MAPE"=Mape) %>% data.frame()
  names(summary) <- as.vector(names[[i]])
  alltable <- cbind(alltable, summary)
 # write.csv(summary, paste0(filename_base, "_R2.csv"))
}

#cleanup
alltable <- alltable[, -c(1)]
names(alltable) <- names(alltable) %>% substring(1,nchar(.)-23) %>% stri_replace_all_fixed(., " ", "")
cutposition <- names(alltable)[1] %>% gregexpr('/',.) %>% unlist() %>% tail(n=1)
names(alltable) <- sub('_s1_1ha_', '_', names(alltable)) %>% substring(cutposition+1, nchar(.))
alltable <- alltable[,order(colnames(alltable))]

# year-by-year
years <- tables[[1]]$Date %>% as.Date(tables[[i]]$Date, format = "%b %d, %Y") %>% year() %>% unique()
summary_table <- vector(mode = "list", length = length(tables))
names(summary_table) <- names(alltable)

for (i in 1:length(tables)) {
  annualtable <- data.frame(year = years, R2 = rep(0, length(years)), Mape = rep(0, length(years)))

  names(tables[[i]])[1] <- "Date"
  tables[[i]]$Date <- as.Date(tables[[i]]$Date, format = "%b %d, %Y")
  for (year in years){ # change this loop to iterate over years
    yeardata <- tables[[i]] %>% filter(year(Date) == year)
    actual <- yeardata[[2]] %>% unlist %>% as.numeric()
    pred <- yeardata[[3]] %>% unlist %>% as.numeric()
    
    # Calculate R2
    R2 <- RSQUARE(actual, pred)
    
    # Calculate MAPE
    Mape <- MAPE(actual, pred)
    
    # Write out processed file
    filename_base <- substring(names(tables[i]), 1, nchar(names(tables[i])) - 4)
    annual <- data.frame(year,R2,Mape)
    annualtable <- rows_update(annualtable, annual)
  }
  
  # Add to yearly totals in the summary table
  summary_table[[i]] <- annualtable
}


summary_table

#write out summary file
write.csv(alltable, paste0(wd, "S1_", S1_band, "_harmonic_model_fit_stats.csv"))

# #write out data files
# write.csv(tables[[1]], paste0(wd, "/mata_escura_S1_", S1_band, "_harmonic_model_data.csv"))
# write.csv(tables[[2]], paste0(wd, "/monte_pascoal_S1_", S1_band, "_harmonic_model_data.csv"))
# write.csv(tables[[3]], paste0(wd, "/rio_doce_S1_", S1_band, "_harmonic_model_data.csv"))
# write.csv(tables[[4]], paste0(wd, "/sooretama_S1_", S1_band, "_harmonic_model_data.csv"))

#write out annual data files
write.csv(summary_table[[1]], paste0(wd, "/mata_escura_S1_", S1_band, "_harmonic_model_annual_fit_stats.csv"))
write.csv(summary_table[[2]], paste0(wd, "/monte_pascoal_S1_", S1_band, "_harmonic_model_annual_fit_stats.csv"))
write.csv(summary_table[[3]], paste0(wd, "/rio_doce_S1_", S1_band, "_harmonic_model_annual_fit_stats.csv"))
write.csv(summary_table[[4]], paste0(wd, "/sooretama_S1_", S1_band, "_harmonic_model_annual_fit_stats.csv"))
