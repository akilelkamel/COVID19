
# Load packages
## For data wrangling and tidying
library(tidyverse)

## For cleaning column names
library(janitor)

## For Date manipulation
library(lubridate)

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
	col_names <- c("confirmed", "recovered", "deaths")
	map2(cases_type, col_names, read_raw) %>%
    	reduce(left_join) %>%
    	rename(country = country_region) %>% 
	    group_by(date, country) %>% 
	    summarize(confirmed = sum(confirmed, na.rm = TRUE),
	              recovered = sum(recovered, na.rm = TRUE),
	              deaths = sum(deaths, na.rm = TRUE)) %>%
	    ungroup() %>% 
	    group_by(country) %>% 
	    mutate(confirmed_daily = c(confirmed[1], diff(confirmed)),
	           recovered_daily = c(recovered[1], diff(recovered)),
	           deaths_daily = c(deaths[1], diff(deaths))
	        
	    )
}

df <- read_data()
