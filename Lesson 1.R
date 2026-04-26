# Week 1 - Getting started with R and working with data

# 1 - relevant libraries
library(readr) # this is a specialised bundle of code (i.e., a "library") to read in data in specific formats, such as .csv ("comma-separated values")
library(tidyverse) # a specific library to "manipulate" data (creating new columns etc.)
library(forecast) # specialised library for making forecasts

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 1.a Reading in data from an API

# Instead of downloading large tables or excel files, you can sometimes download data from the data provider's API (Application Programming Interface)

# Example: Our World in Data by Oxford University
co2 <- read.csv("https://nyc3.digitaloceanspaces.com/owid-public/data/co2/owid-co2-data.csv") #the term in brackets is the API + the dataset "owid-co2-data.csv"

# Load & inspect the data
str(co2) # returns the structure of the data table
head(co2) #returns the first five rows 
summary(co2$co2) #summarises a specific column 

# Subset one country
germany <- co2[co2$country == "Germany" & co2$year >= 1990, ]

# Q1 - what is the maximum share of trade in co2 emissions? 
summary(co2$trade_co2_share) #568.635
# Q2 - what is the median share of co2 emissions from cement production?
summary(co2$cement_co2) #0.000
# Q3 - select a different country - repeat Q1 and Q2 
India <- co2[co2$country == "India" & co2$year >= 1990, ] 
  summary(India$trade_co2_share) #-0.471
  summary(India$cement_co2) #66.62


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 1.b Manipulating data 

# Filter, select, mutate
germany <- co2 |>
  filter(country == "Germany", year >= 1990) |>
  select(country, year, co2, co2_per_capita, gdp, population) |> # selecting six columns "Country", "Year", "Co2", "Co2 per capita", "gdp" and "population
  mutate(gdp_trillion = gdp / 1e12, #normalising GDP to trillion dollars
         co2_intensity = co2 / (gdp / 1e9))  # Mt CO2 per billion USD GDP

# Comparing multiple countries
countries <- co2 |>
  filter(country %in% c("Germany", "China", "United States", "India"),
         year >= 1990) |>
  select(country, year, co2, co2_per_capita, gdp)

# Q4 - select four different countries and inspect the data
countries <- co2 |>
  filter(country %in% c("Afghanistan", "Zimbabwe", "Sudan", "Jamaica"),
         year >= 1990) |>
  select(country, year, co2, co2_per_capita, gdp)

# Wide-Format to compare countries
co2_wide <- countries |>
  select(country, year, co2) |> 
  pivot_wider(names_from = country, values_from = co2)

# Q5 - Why should we want to pivot the data tables? 
#Pivoting is used to reshape data so it’s easier to analyze, compare, and visualize.

# Q6 - Change to see co2_per_capita and inspect the pivot table
co2_wide2 <- countries |>
  select(country, year, co2_per_capita) |> 
  pivot_wider(names_from = country, values_from = co2_per_capita)


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 2 Visualising data 

ggplot(germany, aes(x = year, y = co2)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_smooth(method = "loess", #fits a smooth line to see the trend
              se = TRUE, #showing the standard error
              color = "firebrick") +
  labs(title = "Germany: CO2 Emissions 1990–2022",
       x = NULL, y = "Mt CO2") +
  theme_minimal()

# Q7 - Repeat for the chosen country (which is not Germany)

ggplot(India, aes(x = year, y = co2)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_smooth(method = "loess", #fits a smooth line to see the trend
              se = TRUE, #showing the standard error
              color = "firebrick") +
  labs(title = "India: CO2 Emissions 1990–2022",
       x = NULL, y = "Mt CO2") +
  theme_minimal()


ggplot(countries, aes(x = year, y = co2_per_capita, color = country)) +
  geom_line(linewidth = 1) +
  labs(title = "CO2 per Capita by Country",
       y = "t CO2 per person", color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Q8 - Repeat for the chosen country group
ggplot(countries, aes(x = year, y = co2_per_capita, color = country)) +
  geom_line(linewidth = 1) +
  labs(title = "CO2 per Capita by Country",
       y = "t CO2 per person", color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

#comparing a certain year across all countries
co2_2019 <- co2 |>
  filter(year == 2019, !is.na(gdp), !is.na(co2_per_capita), population > 1e6)

ggplot(co2_2019, aes(x = gdp / population, y = co2_per_capita)) +
  geom_point(aes(size = population), alpha = 0.5) +
  geom_smooth(method = "lm", color = "firebrick") +
  scale_x_log10(labels = scales::label_dollar()) +
  geom_text(aes(label = country), size = 3, vjust = -0.5, hjust = 0.5, alpha = 0.7) +  # Adding labels
  scale_size_continuous(guide = "none") +
  labs(title = "Income vs. CO2 per Capita (2019)",
       x = "GDP per Capita (log scale, USD)",
       y = "CO2 per Capita (t)") +
  theme_minimal()

# Q9 - Name a country with a low GDP per capita and low CO2 per capita
#Central African Republic

# Q10 - Name a country with a high GDP per capita and low CO2 per capita
#Norway

# Q11 - Name a country with a high GDP per capita and high CO2 per capita
#Qatar

# Q12 - any outliers? 
#Yes Norway (low CO₂ despite high GDP) can be considered an outlier.

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 3 Forecasting 


# 3.1 Prepare the time series
de_ts <- germany |>
  arrange(year) |>
  pull(co2) |>
  ts(start = 1990, frequency = 1)

# 3.2 Fit & forecast
fit <- auto.arima(de_ts) # autoregressive moving average
fc  <- forecast(fit, h = 15)  # mmoving avergae based on "15 year" time horizon

# 3.3 Plot 
autoplot(fc) +
  labs(title = "Germany CO₂ Forecast (ARIMA)",
       x = NULL, y = "Mt CO₂") +
  theme_minimal()

# Q13 - repeat the pupeline for a country and indicator of your choice
de_ts <- India |>
  arrange(year) |>
  pull(co2) |>
  ts(start = 1990, frequency = 1)

fit <- auto.arima(de_ts)
fc  <- forecast(fit, h = 15)

autoplot(fc) +
  labs(title = "India CO₂ Forecast (ARIMA)",
       x = NULL, y = "Mt CO₂") +
  theme_minimal()
