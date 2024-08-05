library(readr)
data <- read_csv("/Users/rhian/Documents/PES1UG21CS486/yield_df.csv")

#Attribute names & types
names(data)
sapply(data, class)

#Checking for missing values
any(is.na(data))

# Displaying the structure of your data
str(data)
# Displaying the first few rows of your data
head(data)

library(pastecs)
stats_of_data <- format(stat.desc(data), scientific = FALSE)
print(stats_of_data)

library(ggplot2)

# Analyzing Item
table(data$Item)

pie(table(data$Item), main = "Crop Frequency Distribution", col = terrain.colors(12))

crop_frequency <- function(chosen_area){
  # Filtering data for the selected area
  filtered_data = data[data$Area == chosen_area, ]
  
  # Plotting a bar chart for the frequency of different crops
  ggplot(filtered_data, aes(x = Item, fill = Item)) +
    geom_bar() +
    labs(title = paste("Crop Frequency in", chosen_area),
         x = "Crops",
         y = "Frequency") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

crop_frequency("Albania")
crop_frequency("Botswana")
crop_frequency("United Kingdom")
crop_frequency("India")

print('The crop which is grown in most of the countries is:')
which.max(table(data$Item))

#Year-wise Crop Yield Analysis
library(dplyr)
library(scales)
plot_yearwise_crop_yield <- function(data) {
  # Grouping the data by Year and Item to get the sum of yield of each crop per year
  yearwise_crop_yield <- data %>%
    group_by(Year, Item) %>%
    summarise(total_yield = sum(`hg/ha_yield`)) %>%
    ungroup()
  
  # Plotting the data using ggplot2
  ggplot(yearwise_crop_yield, aes(x = Year, y = total_yield, color = Item)) +
    geom_line(linewidth = 1.2) +  # Thicker lines for better visibility
    labs(title = "Yearwise crop-yield",
         x = "Year",
         y = "Yield (hg/ha)",
         color = "Crop") +
    scale_y_continuous(labels = comma) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5),
      legend.position = "right"
    )
}
plot_yearwise_crop_yield(data)

library(modeest)

# Grouping data by crop
crop_stats = data %>%
  group_by(Item) %>%
  reframe(
    
    Mean_Yield = mean(`hg/ha_yield`),
    Median_Yield = median(`hg/ha_yield`),
    Mode_Yield = mfv(`hg/ha_yield`)[1],
    Max_yield = max(`hg/ha_yield`),
    Min_yield = min(`hg/ha_yield`)
  )
print(crop_stats)

# Plotting a barplot of various crops
ggplot(data, aes(x = Item, y = `hg/ha_yield`, fill = Item)) +
  geom_bar(stat = "identity") +
  labs(title = "Crop Yield Analysis",
       x = "Crops",
       y = "Yield (hg/ha)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

library(tidyverse)

# Trend analysis using ggplot2
ggplot(data, aes(x = Year, y = `hg/ha_yield`, color = Item)) +
  geom_line() +
  labs(title = "Trend Analysis of Agriculture Yield Over Time",
       x = "Year",
       y = " Yield (hg/ha)") +
  theme_bw()

# Calculating mean yield for each country
country_yield = data %>%
  group_by(Area) %>%
  summarize(mean_yield = mean(`hg/ha_yield`))

#Trends over the years for different countries
plot_trend <- function(chosen_country){
  trend_plot = data %>%
    filter(Area == chosen_country) %>%
    ggplot(aes(x = Year, y = `hg/ha_yield`)) +
    geom_line(color = "turquoise", lwd = 1) +
    labs(title = "Trend of Yield Over Years in the Chosen Country", x = "Year", y = "Yield (hg/ha)") +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  print(trend_plot)
}

plot_trend("United Kingdom")
plot_trend("India")
plot_trend("Botswana")

# Creating a bar plot to compare mean yield between countries
ggplot(country_yield, aes(x = reorder(Area, -mean_yield), y = mean_yield)) +
  geom_bar(stat = "identity", fill = "yellow",color = "black") +
  labs(title = "Mean Yield Comparison between Countries", x = "Country", y = "Mean Yield (hg/ha)")+
  theme(axis.text.x = element_text(angle = 90, size = 4.5))

# Crop-wise analysis for a specific crop in a particular area from 1990-2013
crop_analysis = function(crop_name, area_name) {
  # Filtering data for the specific crop, area, and time period
  crop_data = subset(data, Item == crop_name & Area == area_name & Year >= 1990 & Year <= 2013)
  
  # Displaying summary statistics
  print(paste("Summary Statistics for", crop_name, "in", area_name, "from 1990-2013:"))
  print(summary(crop_data$`hg/ha_yield`))
  
  # Creating a boxplot
  boxplot(crop_data$`hg/ha_yield`, main = paste(crop_name, "Yield Distribution in", area_name), ylab = "Yield (hg/ha)", col = "#D6604D")
  
  # Identification of outliers using the Tukey method
  outliers = boxplot.stats(crop_data$`hg/ha_yield`)$out
  
  # Displaying outliers
  print("Outliers are:")
  print(outliers)
}

crop_analysis("Potatoes","India")
crop_analysis("Sweet potatoes","India")
crop_analysis("Wheat","India")

#Effect of rainfall

ggplot(data, aes(x = Item, y = average_rain_fall_mm_per_year, fill = Item)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", color = "black", fill = "#7443AF") +
  labs(title = "Average Rainfall Required for Different Crops",
       x = "Crops",
       y = "Average Rainfall (mm per year)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Rainfall amount for a particular area 

rainfall_in_area <- function(selected_country){
  # Filtering data for the selected country
  filtered_data = data[data$Area == selected_country, ]
  filtered_data$Year <- as.numeric(as.character(filtered_data$Year))
  # Filtering data for the specified period (1990-2013)
  filtered_data = filtered_data[filtered_data$Year >= 1990 & filtered_data$Year <= 2013, ]
  # Plotting a bar chart for average rainfall
  ggplot(filtered_data, aes(x = as.factor(Year), y = average_rain_fall_mm_per_year, fill = as.factor(Year))) +
    geom_bar(stat = "identity") +
    labs(title = paste("Average Rainfall in", selected_country, " (1990-2013)"),
         x = "Year",
         y = "Average Rainfall (mm per year)") +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
rainfall_in_area("Albania")
rainfall_in_area("United Kingdom")
rainfall_in_area("India")
rainfall_in_area("Botswana")

# Effect of pesticide

custom_color <- "#7CAE00"
ggplot(data, aes(x = Item, y = pesticides_tonnes, fill = Item)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", color = "black", fill = custom_color) +
  labs(title = "Average Pesticides Required for Different Crops",
       x = "Crops",
       y = "Average Pesticides (tonnes)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

pesticide_trend <- function(country_name) {
  # Filter data for the selected country and the specified period
  filtered_data <- data %>%
    filter(Area == country_name & Year >= 1990 & Year <= 2013)
  
  # Convert Year to a numeric type if it's not already
  filtered_data$Year <- as.numeric(as.character(filtered_data$Year))
  
  # Plot the pesticide usage trend
  ggplot(filtered_data, aes(x = Year, y = pesticides_tonnes)) +
    geom_line(color = "#4393C3", linewidth = 1) +
    labs(title = paste("Pesticide Usage Trend in", country_name, "(1990-2013)"),
         x = "Year",
         y = "Pesticide Usage (tonnes)") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      plot.title = element_text(hjust = 0.5)
    )
}

pesticide_trend("India")
pesticide_trend("United Kingdom")
pesticide_trend("Botswana")

# Effect of temperature

ggplot(data, aes(x = Item, y = avg_temp, fill = Item)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", color = "black", fill = "#FD7446FF") +
  labs(title = "Average Temperature Required for Different Crops",
       x = "Crops",
       y = "Average Temperature (deg celsius)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

temperature_trend <- function(country_name) {
  # Filter data for the selected country and the specified period
  filtered_data <- data %>%
    filter(Area == country_name & Year >= 1990 & Year <= 2013)
  
  # Convert Year to a numeric type if it's not already
  filtered_data$Year <- as.numeric(as.character(filtered_data$Year))
  
  # Plot the temperature trend
  ggplot(filtered_data, aes(x = Year, y = avg_temp)) +
    geom_line(color = "#4393C3", linewidth = 1) +
    labs(title = paste("Temperature Trend in", country_name, "(1990-2013)"),
         x = "Year",
         y = "Average Temperature (°C)") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      plot.title = element_text(hjust = 0.5)
    )
}

temperature_trend("India")
temperature_trend("United Kingdom")
temperature_trend("Botswana")


#Correlation analysis

# Scatter plot to visualize the relationship between yield & temp
ggplot(data, aes(x = avg_temp, y = `hg/ha_yield`)) +
  geom_point(col = "#4393C3") +
  geom_smooth(method = "loess", se = TRUE, color = "red") +
  labs(title = "Temperature vs Crop Yield",
       x = "Average Temperature (deg. cel.)",
       y = "Crop Yield (hg/ha)")+
  theme_bw()

library(mgcv)

# Scatter plot to visualize the relationship between yield & rainfall
ggplot(data, aes(x = average_rain_fall_mm_per_year, y =`hg/ha_yield` )) +
  geom_point(col = "#4343C3") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = TRUE, color = "red") +
  labs(title = "Rainfall vs Crop Yield",
       x = "Average Rainfall (mm_per_year)",
       y = "Crop Yield (hg/ha)")+
  theme_bw()

# Scatter plot to visualize the relationship between yield & pesticides
ggplot(data, aes(x = pesticides_tonnes, y = `hg/ha_yield`)) +
  geom_point(col = "#4361A3") +
  labs(title = "Pesticides vs Crop Yield",
       x = "Pesticides Used (tonnes)",
       y = "Crop Yield (hg/ha)")+
  theme_bw()

# Scatter plot to visualize the relationship between rainfall & temperature
ggplot(data, aes(x = average_rain_fall_mm_per_year, y = avg_temp)) +
  geom_point(col = "#4391A3") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Rainfall vs Temperature",
       x = "Average Rainfall (mm_per_year)",
       y = "Average Temperature (deg. cel.)")+
  theme_bw()

correlation_matrix = cor(data[, c( "hg/ha_yield","average_rain_fall_mm_per_year", "pesticides_tonnes", "avg_temp")])
correlation_matrix

library(reshape2)

ggplot(data = melt(correlation_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Inferences
plot_yield_and_stats = function(crop_name, country_name) {
  crop_data = subset(data, Item == crop_name & Area == country_name & Year >= 1990 & Year <= 2013)
  max_yield_year = crop_data$Year[which.max(crop_data$`hg/ha_yield`)]
  
  # Extracting average rainfall and temperature for the year of maximum yield
  max_yield_stats = subset(crop_data, Year == max_yield_year) %>%
    summarise(Avg_Rainfall = mean(average_rain_fall_mm_per_year),
              Avg_Temperature = mean(avg_temp))
  
  print(paste("Average Rainfall in", max_yield_year, ":", max_yield_stats$Avg_Rainfall, "mm per year"))
  print(paste("Average Temperature in", max_yield_year, ":", max_yield_stats$Avg_Temperature, "°C"))
  
  # Plotting the graph
  ggplot(crop_data, aes(x = Year, y = `hg/ha_yield`, color = crop_name)) +
    geom_line() +
    geom_point() +
    labs(title = paste("Yield of", crop_name, "in", country_name, "(1990-2013)"),
         x = "Year",
         y = "Yield (hg/ha)") +
    theme_minimal()
}

plot_yield_and_stats("Potatoes", "India")
plot_yield_and_stats("Potatoes", "United Kingdom")
plot_yield_and_stats("Wheat", "India")
plot_yield_and_stats("Wheat", "United Kingdom")
plot_yield_and_stats("Wheat", "Botswana")

# Additional Statistics: Fitting various distribution to the dataset for yield
#install.packages("fitdistrplus", "MASS", "survival")
library(fitdistrplus)
library(MASS)
library(survival)

yield_fit <- function(crop_name, area_name) {
  crop_data = subset(data, Item == crop_name & Area == area_name & Year >= 1990 & Year <= 2013)
  datatoplot = crop_data$`hg/ha_yield`
  # Fit Poisson distribution (or other discrete distribution) and compare
  fit1 = fitdist(datatoplot, "norm")
  fit2 = fitdist(datatoplot, "weibull")
  fit3 = fitdist(datatoplot, "lnorm")
  par(mfrow = c(2,2))
  plot.legend = c("normal","weibull", "lognormal")
  par(mar = c(2,2,2,2))
  denscomp(list(fit1,fit2,fit3), legendtext = plot.legend)
  qqcomp(list(fit1,fit2,fit3), legendtext = plot.legend)
  ppcomp(list(fit1,fit2,fit3), legendtext = plot.legend)
  cdfcomp(list(fit1,fit2,fit3), legendtext = plot.legend)
  # Summary of the best fit
  best_fit1 = gofstat(fit1)
  best_fit2 = gofstat(fit2)
  best_fit3 = gofstat(fit3)
  # View the summary
  print("For Normal Distribution:")
  print(best_fit1)
  print("For Weibull Distribution:")
  print(best_fit2)
  print("For Lognormal Distribution:")
  print(best_fit3)
}

yield_fit("Potatoes","India")
yield_fit("Sweet potatoes",  "India")
yield_fit("Wheat",  "India")
