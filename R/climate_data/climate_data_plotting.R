# Load required libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Read the data from CSV file
#data <- read.csv("/Users/brbell01/Downloads/POWER_Point_Monthly_Timeseries_1981_2021_019d0217S_040d1188W_LST1.csv")
data <- read.csv("/Users/brbell01/Downloads/POWER_Point_Monthly_Timeseries_1981_2021_019d6909S_042d5523W_LST.csv")


# Filter the T2M data
t2m_data <- subset(data, PARAMETER == "T2M")

# Filter the PRECTOTCORR_SUM data
precip_data <- subset(data, PARAMETER == "PRECTOTCORR_SUM") %>% select(-ANN)

# Reshape the data to long format
t2m_data_long <- pivot_longer(t2m_data, -c(PARAMETER, YEAR), names_to = "Month", values_to = "Value")
precip_data_long <- pivot_longer(precip_data, -c(PARAMETER, YEAR), names_to = "Month", values_to = "Value")

# Convert Month column to factor with proper ordering
months <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
t2m_data_long$Month <- factor(t2m_data_long$Month, levels = months)
precip_data_long$Month <- factor(precip_data_long$Month, levels = months)

# Calculate mean values by month
t2m_mean <- aggregate(Value ~ Month, t2m_data_long, mean)
precip_mean <- aggregate(Value ~ Month, precip_data_long, mean)
rescaled_t2m_mean <- precip_mean
rescaled_t2m_mean$Value <- t2m_mean$Value * 6.66666

# Plotting
ggplot() +
  geom_line(data = rescaled_t2m_mean, aes(x = Month, y = Value, color = "Temp. (°C)", group = 1), size = 1) +
  geom_line(data = precip_mean, aes(x = Month, y = Value, color = "Precipitation", group = 1), size = 1) +
  labs(title = "Monthly Mean Temperature and Precipitation",
       x = "Month",
       y = "Precipitation (mm)",
       color = "Parameter") +
  scale_color_manual(values = c("Temp. (°C)" = "red", "Precipitation" = "blue"), guide = "none") +
  scale_y_continuous(
    name = "Precipitation (mm)",
    limits = c(0, 200),
    breaks = seq(0, 200, length.out = 9),
    labels = seq(0, 200, length.out = 9),
    sec.axis = sec_axis(~./6.66666, name = "Temp. (°C)", breaks = seq(0, 30, length.out = 9), labels = seq(0, 30, length.out = 9))
  ) +
  theme_minimal() +
  theme(
    axis.text.y.left = element_text(color = "blue"),
    axis.title.y.left = element_text(color = "blue"),
    axis.text.y.right = element_text(color = "red"),
    axis.title.y.right = element_text(color = "red")
  )

