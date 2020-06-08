
# Load packages
## For data wrangling and tidying
library(tidyverse)
## For clening column names
library(janitor)
## For Date manipulation
library(lubridate)
## To visualize, wrangle and preprocess time series data
library(timetk)

# Read raw data for each a given type of cases: "confirmed", "recovered", or "deaths"
read_raw <- function(cases_type, col_name){  

	url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
    url <- paste(url, "time_series_covid19_", cases_type, "_global.csv", sep="")

	read_csv(url) %>% 
        pivot_longer(-c(`Province/State`, `Country/Region`, Lat, Long), names_to = "date", values_to = "cases_num" ) %>% 
        clean_names() %>% 
        mutate(
          date = as.Date(date, format = "%m/%d/%y")
        ) %>% 
        group_by(date, country_region, lat, long) %>% 
        summarise(cases_num = sum(cases_num)) %>% 
        ungroup() %>% 
        rename({{col_name}} := cases_num)
}

# Get all data and combine it into one data frame.
read_data <- function(){
	cases_type <- list("confirmed", "recovered", "deaths")
	c_titles <- c("confirmed", "recovered", "deaths")
	map2(cases_type, c_titles, read_raw) %>%
    	reduce(left_join) %>%
    	rename(country = country_region) %>%
    	# Here somme corrections for wrong values
    	# Saudi Arabia on 03/18/2020, number of confirmed cases is 238 not 171
    	# Tunisia on 03/30/2020, number of confirmed cases is 362 not 312
    	mutate(confirmed = ifelse(country == "Saudi Arabia" & date == "2020-03-18", 238, confirmed)) %>%
    	mutate(confirmed = ifelse(country == "Tunisia" & date == "2020-03-30", 362, confirmed))
}

daily_cases_country <- function(df, type, ctry){
    data <- df  %>%
        filter(country == ctry, confirmed > 0) %>%
        select(date, type)
    
    data$confirmed <- c(data$confirmed[1], diff(data$confirmed))
    return(data)
}

df <- read_data()
country <- "Saudi Arabia"
data_confirmed_ctry <- daily_cases_country(df, "confirmed", country)

data_confirmed_ctry %>%
    plot_time_series(date, confirmed, .color_var = month(date),
                 .interactive = FALSE, .color_lab = "Month", .line_size = 1, .smooth_size = 0.5,
                 .title = paste("Daily confirmed cases for", country))