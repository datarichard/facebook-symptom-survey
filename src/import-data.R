#### Survey and daily cases data ####

#### Import survey data ####
# This script uses the open API from the University of Maryland 
# (https://gisumd.github.io/COVID-19-API-Documentation/docs/get_started.html). 
# 
# There is a brief (very brief) tutorial describing how to use the API in R:
# https://gisumd.github.io/COVID-19-API-Documentation/docs/tutorials/get_r.html

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

#### Import COVID daily cases data ####
# There is a brief tutorial on how to use rvest and polite to scrape data
# https://www.njtierney.com/post/2020/10/11/times-scales-covid/

library(tidyverse)
library(lubridate)
library(rvest)
library(polite)


scrape_daily_cases <- function(.url) {
  polite::bow(.url) %>%
    polite::scrape() %>%
    rvest::html_table() %>%
    purrr::pluck(2) %>%
    as_tibble()
}

# NSW
nsw_cases_url <- "https://covidlive.com.au/report/daily-cases/nsw"
nsw_cases_raw <- scrape_daily_cases(nsw_cases_url) 

# VIC
vic_cases_url <- "https://covidlive.com.au/report/daily-cases/vic"
vic_cases_raw <- scrape_daily_cases(vic_cases_url) 

# QLD
qld_cases_url <- "https://covidlive.com.au/report/daily-cases/qld"
qld_cases_raw <- scrape_daily_cases(qld_cases_url)

# WA
wa_cases_url <- "https://covidlive.com.au/report/daily-cases/wa"
wa_cases_raw <- scrape_daily_cases(wa_cases_url)

# SA
sa_cases_url <- "https://covidlive.com.au/report/daily-cases/sa"
sa_cases_raw <- scrape_daily_cases(sa_cases_url)

# TAS
tas_cases_url <- "https://covidlive.com.au/report/daily-cases/tas"
tas_cases_raw <- scrape_daily_cases(tas_cases_url)

# NT
nt_cases_url <- "https://covidlive.com.au/report/daily-cases/nt"
nt_cases_raw <- scrape_daily_cases(nt_cases_url)

# ACT
act_cases_url <- "https://covidlive.com.au/report/daily-cases/act"
act_cases_raw <- scrape_daily_cases(act_cases_url)


transmute_daily_cases <- function(.df) {
  .df %>%
    transmute(
      date = lubridate::dmy(DATE),
      cases = parse_number(as.character(NEW))
    )
}

nsw_cases <- transmute_daily_cases(nsw_cases_raw) 
vic_cases <- transmute_daily_cases(vic_cases_raw) 
qld_cases <- transmute_daily_cases(qld_cases_raw) 
wa_cases <- transmute_daily_cases(wa_cases_raw) 
sa_cases <- transmute_daily_cases(sa_cases_raw) 
tas_cases <- transmute_daily_cases(tas_cases_raw) 
nt_cases <- transmute_daily_cases(nt_cases_raw) 
act_cases <- transmute_daily_cases(act_cases_raw) 

other_cases <- bind_rows(tas_cases,
                         nt_cases,
                         act_cases) %>%
  group_by(date) %>%
  summarise(cases = sum(cases, na.rm = T))


daily_cases_by_state <- bind_rows(
  "New South Wales" = nsw_cases, 
  "Victoria" = vic_cases, 
  "Queensland" = qld_cases, 
  "Western Australia" = wa_cases, 
  "South Australia" = sa_cases, 
  "Other" = other_cases,
  .id = "State")

write_rds(daily_cases_by_state, "data/daily_cases.RDS")