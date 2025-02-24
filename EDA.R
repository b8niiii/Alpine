# install.packages(c("DataExplorer", "tidyverse", "lubridate", "corrplot", "cpp11", "igraph", "gridExtra", "networkD3", "timechange"))
# Load required libraries
library(tidyverse)      # For data manipulation and ggplot2 for visualization
library(lubridate)      # For working with dates
library(DataExplorer)   # For automated EDA reports
library(corrplot)       # For correlation matrix plots
library(patchwork)
library(ggplot2)
library(gridExtra)
library(knitr)
library(plotly)
# Read in the CSV files
maurienne <- read_csv("maurienne_valley_weather_1980_today.csv")
susa <- read_csv("susa_valley_weather_1980_today.csv")


# Quick look at the data structure
print(glimpse(maurienne))
print(glimpse(susa))

# Summary statistics for each dataset
print(summary(maurienne))
print(summary(susa))

# Check for missing values visually using DataExplorer
print(plot_missing(maurienne))
print(plot_missing(susa))


print(colnames(maurienne))
print(colnames(susa))

# Define the new column names in a vector (in the same order as the original columns)
new_col_names <- c("date", 
                   "max_temp", 
                   "min_temp", 
                   "mean_temp", 
                   "avg_rel_humidity", 
                   "avg_cloud_cover", 
                   "total_precip", 
                   "max_wind_speed", 
                   "avg_wind_dir", 
                   "weather_code")

# Rename columns for the Maurienne dataset
colnames(maurienne) <- new_col_names

# Rename columns for the Susa dataset
colnames(susa) <- new_col_names

# Optional: Print out the new column names to verify the change
print(names(maurienne))
print(names(susa))

maurienne <- maurienne %>% mutate(date = as.Date(date, format = "%Y-%m-%d"))

susa <- susa %>% mutate(date = as.Date(date, format = "%Y-%m-%d"))

maurienne$weather_code <- as.factor(maurienne$weather_code) #make wheater code a factor
susa$weather_code <- as.factor(susa$weather_code) 

glimpse(susa)
glimpse(maurienne)                       


plots <- list()


# Initialize an empty list to store plots
plots <- list()
par(mar = c(2, 2, 2, 2))
# Create plots for Maurienne
if ("mean_temp" %in% names(maurienne)) {
  plots[[length(plots) + 1]] <- ggplot(maurienne, aes(x = date, y = mean_temp)) +
    geom_line(color = "tomato") +
    labs(title = "Maurienne Valley Temperature", x = "Date", y = "Mean Temp (°C)") +
    theme_minimal()
}

if ("total_precip" %in% names(maurienne)) {
  plots[[length(plots) + 1]] <- ggplot(maurienne, aes(x = date, y = total_precip)) +
    geom_line(color = "darkorange") +
    labs(title = "Maurienne Valley Precipitation", x = "Date", y = "Total Precip (mm)") +
    theme_minimal()
}

# Create plots for Susa
if ("mean_temp" %in% names(susa)) {
  plots[[length(plots) + 1]] <- ggplot(susa, aes(x = date, y = mean_temp)) +
    geom_line(color = "tomato") +
    labs(title = "Susa Valley Temperature", x = "Date", y = "Mean Temp (°C)") +
    theme_minimal()
}

if ("total_precip" %in% names(susa)) {
  plots[[length(plots) + 1]] <- ggplot(susa, aes(x = date, y = total_precip)) +
    geom_line(color = "darkorange") +
    labs(title = "Susa Valley Precipitation", x = "Date", y = "Total Precip (mm)") +
    theme_minimal()
}

# Display all plots in a grid if there are any
if (length(plots) > 0) {
  combined_plot <- wrap_plots(plots, ncol = 2)
  print(combined_plot)
}


# Basic time-series visualizations for both datasets
par(mar = c(2, 2, 2, 2))
# For Maurienne Valley data
if("mean_temp" %in% names(maurienne)) {
  ggplot(maurienne, aes(x = date, y = mean_temp)) +
    geom_line(color = "steelblue") +
    labs(title = "Maurienne Valley Temperature Over 20 Years",
         x = "Date", y = "Mean Temperature (°C)") +
    theme_minimal() }




if("total_precip" %in% names(maurienne)) {
  ggplot(maurienne, aes(x = date, y = total_precip)) +
    geom_line(color = "forestgreen") +
    labs(title = "Maurienne Valley Precipitation Over 20 Years",
         x = "Date", y = "Total Precipitation (mm)") +
    theme_minimal()
}

# For Susa Valley data
if("mean_temp" %in% names(susa)) {
  ggplot(susa, aes(x = date, y = mean_temp)) +
    geom_line(color = "tomato") +
    labs(title = "Susa Valley Temperature Over 20 Years",
         x = "Date", y = "Mean Temperature (°C)") +
    theme_minimal()
}

if("total_precip" %in% names(susa)) {
  ggplot(susa, aes(x = date, y = total_precip)) +
    geom_line(color = "darkorange") +
    labs(title = "Susa Valley Precipitation Over 20 Years",
         x = "Date", y = "Total Precipitation (mm)") +
    theme_minimal()
}
# Correlation analysis for numeric variables

# Maurienne Valley
maurienne_numeric <- maurienne %>% select_if(is.numeric)

if (ncol(maurienne_numeric) > 1) {
  cor_matrix_maurienne <- cor(maurienne_numeric, use = "complete.obs")
  
  # Print as a formatted table
  kable(cor_matrix_maurienne, digits = 2)
  
  # Display correlation heatmap
  corrplot(cor_matrix_maurienne, method = "color", tl.cex = 0.8)
}

# Susa Valley
susa_numeric <- susa %>% select_if(is.numeric)

if (ncol(susa_numeric) > 1) {
  cor_matrix_susa <- cor(susa_numeric, use = "complete.obs")
  
  # Print as a formatted table
  kable(cor_matrix_susa, digits = 2)
  
  # Display correlation heatmap
  corrplot(cor_matrix_susa, method = "color", tl.cex = 0.8)
}

pairs(susa[, sapply(susa, is.numeric)])
pairs(maurienne[, sapply(maurienne, is.numeric)])



#s <- ggplot(susa, aes(x = date, y = mean_temp)) + geom_line() + labs(title = "Susa Valley Temperature Over 20 Years", x = "Date", y = "Mean Temperature (°C)") + theme_minimal()
#ggplotly(s)

#m <- ggplot(maurienne, aes(x = date, y = mean_temp)) + geom_line() + labs(title = "Maurienne Valley Temperature Over 20 Years", x = "Date", y = "Mean Temperature (°C)") + theme_minimal()
#ggplotly(m)

#SEASONAL ANALYSIS:
# Create a new column for the month of the year
maurienne <- maurienne %>% mutate(month = month(date, label = TRUE))
susa <- susa %>% mutate(month = month(date, label = TRUE))
# Create a new column for the year
maurienne <- maurienne %>% mutate(year = year(date))
susa <- susa %>% mutate(year = year(date))
# Now we can use facetting to see how the variable behaves across each season



# Calculate the mean temperature per year for each month
maurienne_yearly <- maurienne %>% # Calculate the mean temperature per year for each month
  group_by(year, month) %>% # Group by year and month
  summarize(mean_temp_year = mean(mean_temp, na.rm = TRUE)) # Calculate the mean temperature

susa_yearly <- susa %>%
  group_by(year, month) %>%
  summarize(mean_temp_year = mean(mean_temp, na.rm = TRUE))


# Create seasonal plots for Maurienne Valley
ggplot(maurienne, aes(x = year, y = mean_temp)) + # Plot the original data
  geom_line(color = "tomato", alpha = 0.5) + # Add a line plot
  geom_line(data = maurienne_yearly, aes(x = year, y = mean_temp_year), # Add a line plot for yearly means
            color = "blue", size = 0.5) +
  facet_wrap(~ month, ncol = 4) + # Facet by month
  labs(
    title = "Maurienne Valley Temperature by Month across Years",
    x = "Year",
    y = "Mean Temp (°C)"
  ) +
  theme_minimal() # Apply a minimal theme
ggplot(susa, aes(x = year, y = mean_temp)) + # Plot the original data
  geom_line(color = "tomato", alpha = 0.5) + # Add a line plot
  geom_line(data = susa_yearly, aes(x = year, y = mean_temp_year), # Add a line plot for yearly means
            color = "blue", size = 0.5) +
  facet_wrap(~ month, ncol = 4) + # Facet by month
  labs(
    title = "Susa Valley Temperature by Month across Years",
    x = "Year",
    y = "Mean Temp (°C)"
  ) +
  theme_minimal()

# DO THE SAME PLOTS BUT LOOKING AT PRECIPITATIONS

maurienne_yearly_prec <- maurienne %>%
  group_by(year, month) %>%
  summarize(mean_prec_year = mean(total_precip, na.rm = TRUE))

susa_yearly_prec <- susa %>%
  group_by(year, month) %>%
  summarize(mean_prec_year = mean(total_precip, na.rm = TRUE))

# Create seasonal plots of precipitations for Maurienne Valley
ggplot(maurienne, aes(x = year, y = total_precip)) + # Plot the original data
  geom_line(color = "tomato", alpha = 0.5) + # Add a line plot
  geom_line(data = maurienne_yearly_prec, aes(x = year, y = mean_prec_year), # Add a line plot for yearly means
            color = "blue", size = 0.5) +
  facet_wrap(~ month, ncol = 4) + # Facet by month
  labs(
    title = "Maurienne Valley Precipitations by Month across Years",
    x = "Year",
    y = "Mean Prec (mm)"
  ) +
  theme_minimal() # Apply a minimal theme
ggplot(susa, aes(x = year, y = total_precip)) + # Plot the original data
  geom_line(color = "tomato", alpha = 0.5) + # Add a line plot
  geom_line(data = susa_yearly_prec, aes(x = year, y = mean_prec_year), # Add a line plot for yearly means
            color = "blue", size = 0.5) +
  facet_wrap(~ month, ncol = 4) + # Facet by month
  labs(
    title = "Susa Valley Precipitations by Month across Years",
    x = "Year",
    y = "Mean Prec (mm)"
  ) +
  theme_minimal()

# Same plots but just showing the mean:
ggplot(maurienne_yearly_prec, aes(x = year, y = mean_prec_year)) +
  geom_line(color = "blue", size = 0.5) +
  facet_wrap(~ month, ncol = 4) +
  labs(
    title = "Maurienne Valley Precipitations by Month across Years",
    x = "Year",
    y = "Mean Prec (mm)"
  ) +
  theme_minimal()

ggplot(susa_yearly_prec, aes(x = year, y = mean_prec_year)) +
  geom_line(color = "blue", size = 0.5) +
  facet_wrap(~ month, ncol = 4) +
  labs(
    title = "Susa Valley Precipitations by Month across Years",
    x = "Year",
    y = "Mean Prec (mm)"
  ) +
  theme_minimal()

# overlapping them:
ggplot() +
  geom_line(data = maurienne_yearly_prec, 
            aes(x = year, y = mean_prec_year, color = "Maurienne"), 
            size = 0.5) +
  geom_line(data = susa_yearly_prec, 
            aes(x = year, y = mean_prec_year, color = "Susa"), 
            size = 0.5) +
  facet_wrap(~ month, ncol = 4) +
  labs(
    title = "Precipitations by Month across Years: Maurienne vs Susa",
    x = "Year",
    y = "Mean Precipitation (mm)",
    color = "Valley"
  ) +
  scale_color_manual(values = c("Maurienne" = "blue", "Susa" = "red")) +
  theme_minimal()

a <- lm(min_temp ~ year, data = maurienne)
summary(a)
b <- lm(max_temp ~ year, data = maurienne)
summary(b)
c <- lm(mean_temp ~ year, data = maurienne)
summary(c)

d <- lm(min_temp ~ year, data = susa)
summary(d)
e <- lm(max_temp ~ year, data = susa)
summary(e)
f <- lm(mean_temp ~ year, data = susa)
summary(f)


# LET'S TRY BY USING STL DECOMPOSITION
# Again assuming monthly data with frequency = 12
# Example: creating a time series object from daily data
# temp_data: your vector of daily temperature observations, ts() creates a timeseries object
temp_ts_susa <- ts(susa$mean_temp, start = c(1980, 1), frequency = 365)
# Decompose the time series using STL
# Apply STL decomposition on the time series
stl_decomp_susa <- stl(temp_ts_susa, s.window = "periodic", robust = TRUE)

# Plot the decomposition components
plot(stl_decomp_susa, main = "STL Decomposition of Temperature Time Series in Susa Valley", 
     col = c("blue"))


temp_ts_maurienne <- ts(maurienne$mean_temp, start = c(1980, 1), frequency = 365)
stl_decomp_maurienne <- stl(temp_ts_maurienne, s.window = "periodic", robust = TRUE)
plot(stl_decomp_maurienne)