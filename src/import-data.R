# This script uses the open API from the University of Maryland 
# (https://gisumd.github.io/COVID-19-API-Documentation/docs/get_started.html). 
# 
# There is a brief (very brief) tutorial describing how to use the API in R:
# https://gisumd.github.io/COVID-19-API-Documentation/docs/tutorials/get_r.html

library(httr)
library(jsonlite)

# A full list of indicators is available 
# https://gisumd.github.io/COVID-19-API-Documentation/docs/indicators/indicators.html
# 
# e.g., depressed_7d: Respondents who reported feeling depressed most or all of 
# the time over the past 7 days
survey_variable = "depressed_7d"
date_range = "20200423-20211001"

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

depression <- get_API_data(survey_variable, date_range)
depression_smoothed <- get_API_data(survey_variable, date_range, smoothed = T)
write_rds(depression, "data/depression.RDS")
write_rds(depression_smoothed, "data/depression_smoothed.RDS")