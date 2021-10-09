library(tidyverse)
library(httr)
library(jsonlite)

# add url
path <- "https://covidmap.umd.edu/api/resources?indicator=covid&type=smoothed&country=France&daterange=20201115-20201130"

path <- "https://covidmap.umd.edu/api/resources?indicator=covid&type=smoothed&country=Australia&daterange=20201115-20201130"

path <- "https://covidmap.umd.edu/api/resources?indicator=depressed_7d&type=smoothed&country=Australia&daterange=20201115-20201130"

path <- "https://covidmap.umd.edu/api/resources?indicator=depressed_7d&type=smoothed&country=Australia&region=all&daterange=20201115-20201130"

path <- "https://covidmap.umd.edu/api/resources?indicator=depressed_7d&type=daily&country=Australia&region=all&daterange=20201115-20201130"

path <- "https://covidmap.umd.edu/api/resources?indicator=depressed_7d&type=daily&country=Australia&region=all&daterange=20200423-20211001"
anxious_7d_path <- "https://covidmap.umd.edu/api/resources?indicator=anxious_7d&type=daily&country=Australia&region=all&daterange=20200423-20211001"
path <- "https://covidmap.umd.edu/api/resources?indicator=finance&type=daily&country=Australia&region=all&daterange=20200423-20211001"


# e.g., depressed_7d: Respondents who reported feeling depressed most or all of 
# the time over the past 7 days.
# 
# anxious_7d: Respondents who reported feeling nervous for most or all of the 
# time over the the past 7 days.
# 
# request data from api
request <- GET(url = path)

# make sure the content is encoded with 'UTF-8'
response <- content(request, as = "text", encoding = "UTF-8")

# now we have a dataframe for use!
coviddata <- fromJSON(response, flatten = TRUE)[[1]] %>% tibble()

colnames(coviddata)

[1] "smoothed_pct_depressed_7d" "smoothed_depressed_7d_se" 
[3] "sample_size"               "country"                  
[5] "region"                    "iso_code"                 
[7] "gid_0"                     "gid_1"                    
[9] "survey_date"              


coviddata %>%
  transmute(pct_depressed = pct_depressed_7d,
         date = as.Date(survey_date, format = "%Y%m%d"),
         total_responses = sample_size,
         state = fct_lump(region, n = 5)) %>%
  filter(date < as.Date("2020-09-12")) %>%
  ggplot(aes(x = date, y = pct_depressed)) +
  geom_point(aes(size = total_responses), alpha = 0.25) +
  geom_smooth(se = FALSE, span = 1/4, method = "loess", formula = "y ~ x") +
  labs(title = "Percentage of population reporting they feel depressed (most or all of the time)",
       # subtitle = "'Feel depressed for most or all of the time over the past 7 days'",
       y = "",
       x = "2020",
       size = "Sample size:",
       caption = "Source: University of Maryland Global Symptom Survey of Facebook users") + 
  scale_y_continuous(label = scales::percent_format(accuracy = 1)) +
  facet_wrap(~state, scales = "fixed")  +
  theme_test()

coviddata %>%
  transmute(pct_depressed = percent_hf, #pct_depressed_7d,
            date = as.Date(survey_date, format = "%Y%m%d"),
            total_responses = sample_size,
            state = fct_lump(region, n = 5)) %>%
  filter(date >= as.Date("2021-01-01")) %>%
  ggplot(aes(x = date, y = pct_depressed)) +
  geom_point(aes(size = total_responses), alpha = 0.25) +
  geom_smooth(se = FALSE, span = 1/4, method = "loess", formula = "y ~ x") +
  labs(title = "Percentage of Facebook users reporting they feel depressed",
       subtitle = "'Feel depressed for most or all of the time over the past 7 days'",
       y = "",
       x = "Date in 2021",
       size = "Sample size:",
       caption = "Source: UMD Global Symptom Survey of Facebook users") + 
  scale_y_continuous(label = scales::percent_format(accuracy = 1)) +
  facet_wrap(~state, scales = "fixed")  +
  theme_test()