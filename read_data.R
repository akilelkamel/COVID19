
source("load_packages.R")

clean_df <- function(cases_type, col_name){  

	url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"

	read_csv(paste(url, "time_series_covid19_", cases_type, "_global.csv", sep="")) %>% 
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


read_data <- function(){  
	cases_type <- list("confirmed", "recovered", "deaths")
	c_titles <- c("confirmed", "recovered", "deaths")
	map2(cases_type, c_titles, clean_df) %>%
	reduce(left_join) %>%
	rename(country = country_region) %>%
	# Here somme corrections for wrong values
	# Saudi Arabia on 03/18/2020, number of confirmed cases is 238 not 171
	# Tunisia on 03/30/2020, number of confirmed cases is 362 not 312
	mutate(confirmed = ifelse(country == "Saudi Arabia" & date == "2020-03-18", 238, confirmed)) %>%
	mutate(confirmed = ifelse(country == "Tunisia" & date == "2020-03-30", 362, confirmed))
}


