# This script uses the open API from the University of Maryland 
# (https://gisumd.github.io/COVID-19-API-Documentation/docs/get_started.html). 
# 
# There is a brief (very brief) tutorial describing how to use the API in R:
# https://gisumd.github.io/COVID-19-API-Documentation/docs/tutorials/get_r.html

# Setup
library(httr)
library(jsonlite)

get_API_data <- function(.variable_name, .dates, smoothed=F) {
  
  if (smoothed) {
    
    # add url
    path <- paste0("https://covidmap.umd.edu/api/resources?indicator=",
                   .variable_name, 
                   "&type=smoothed&country=Australia&region=all&daterange=",
                   .dates)
    
  } else {
    # add url
    path <- paste0("https://covidmap.umd.edu/api/resources?indicator=",
                   .variable_name, 
                   "&type=daily&country=Australia&region=all&daterange=",
                   .dates)
  }
  
  # request data from api
  request <- GET(url = path)
  
  # make sure the content is encoded with 'UTF-8'
  response <- content(request, as = "text", encoding = "UTF-8")
  
  # now we have a dataframe for use!
  coviddata <- fromJSON(response, flatten = TRUE)[[1]] %>% tibble::tibble()
  
  return(coviddata)
  
}

# A full list of indicators is available 
# https://gisumd.github.io/COVID-19-API-Documentation/docs/indicators/indicators.html

# Set date range
date_range <- paste0("20200423-", stringr::str_remove_all(Sys.Date(), "-"))


# Depression variables (K10)
# 
# e.g., depressed_7d: Respondents who reported feeling depressed most or all of 
# the time over the past 7 days
survey_variable = "depressed_7d"
depression <- get_API_data(survey_variable, date_range)
depression_smoothed <- get_API_data(survey_variable, date_range, smoothed = T)
write_rds(depression, "data/depression.RDS")
write_rds(depression_smoothed, "data/depression_smoothed.RDS")

# Anxiety variables (K10)
# 
# anxious_7d: Respondents who reported feeling nervous for most or all of the 
# time over the the past 7 days.
survey_variable = "anxious_7d"
anxiety <- get_API_data(survey_variable, date_range)
anxiety_smoothed <- get_API_data(survey_variable, date_range, smoothed = T)
write_rds(anxiety, "data/anxiety.RDS")
write_rds(anxiety_smoothed, "data/anxiety_smoothed.RDS")

# Financial concern
# 
# Update date range
date_range <- paste0("20200501-", stringr::str_remove_all(Sys.Date(), "-"))

# Respondents who were very worried or somewhat worried about themselves and 
# their household’s finances.
survey_variable = "finance"
date_range = "2020501-20211001"
finance <- get_API_data(survey_variable, date_range)
finance_smoothed <- get_API_data(survey_variable, date_range, smoothed = T)
write_rds(finance, "data/finance.RDS")
write_rds(finance_smoothed, "data/finance_smoothed.RDS")
