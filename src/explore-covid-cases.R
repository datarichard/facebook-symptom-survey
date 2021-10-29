#### Setup ####
library(tidyverse)
library(lubridate)
library(rvest)
library(polite)

#### Import data ####

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

#### Preprocessing ####
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

daily_cases_by_state <- bind_rows(
  "New South Wales" = nsw_cases, 
  "Victoria" = vic_cases, 
  "Queensland" = qld_cases, 
  "Western Australia" = wa_cases, 
  "South Australia" = sa_cases, 
  .id = "State")

write_rds(daily_cases_by_state, "data/daily_cases.RDS")

#### Plotting ####
library(ggthemes)

daily_cases_by_state %>%
  filter(date > as.Date("2021-07-01")) %>%
  ggplot(aes(x = date, y = cases)) +
    geom_line(group = "State", color = "dodgerblue3", size = .5) +
    geom_point(color = "white", fill = "dodgerblue3", shape = 21, stroke = .5, size = 2) +
    # scale_y_log10() +
    facet_wrap(~State) +
    theme_fivethirtyeight()
    
         