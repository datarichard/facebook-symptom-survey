# Setup
library(tidyverse)

# Import data
depression <- read_rds("data/depression.RDS")
depression_smoothed <- read_rds("data/depression_smoothed.RDS")

colnames(depression)
[1] "pct_depressed_7d"     "depressed_7d_se"      "pct_depressed_7d_unw" "depressed_7d_se_unw" 
[5] "sample_size"          "country"              "region"               "iso_code"            
[9] "gid_0"                "gid_1"                "survey_date" 

colnames(depression_smoothed)
[1] "smoothed_pct_depressed_7d" "smoothed_depressed_7d_se" 
[3] "sample_size"               "country"                  
[5] "region"                    "iso_code"                 
[7] "gid_0"                     "gid_1"                    
[9] "survey_date"              


# Set up plot
plot_response <- function(.d, .title) {
  ggplot(.d, aes(x = date, y = response)) +
    geom_point(aes(size = sample_size), alpha = 0.25) +
    geom_smooth(se = FALSE, span = 1/4, method = "loess", formula = "y ~ x") +
    geom_vline(aes(xintercept = as.Date("2020-07-07")), 
               linetype = "dotted", 
               size = 0.25,
               color = "red") +
    geom_vline(aes(xintercept = as.Date("2020-08-02")), 
               size = 0.2,
               color = "red") +
    labs(
      title = .title,
      y = "",
      x = "",
      size = "Sample size:",
      caption = "Red dotted line: VIC lockdown
       Solid red line: VIC State of Emergency declared
       
       Data source: University of Maryland Global CTIS") + 
    scale_y_continuous(label = scales::percent_format(accuracy = 1)) +
    scale_x_date(date_breaks = "2 months", # 28 days works
                 date_labels = "%b") +
    facet_wrap(~state, scales = "fixed")  +
    theme_test()
}

# Plot depression data
title_str = c(
  "Percent of population feeling depressed most or all of the time in past 7 days (2020)"
)

depression %>%
  transmute(response = pct_depressed_7d,
            date = as.Date(survey_date, format = "%Y%m%d"),
            sample_size,
            state = fct_lump(region, n = 5),
            state = fct_relevel(state, "New South Wales", "Victoria")) %>%
  filter(date < as.Date("2020-12-31")) -> d1

plot_response(d1, title_str)

depression_smoothed %>%
  transmute(response = smoothed_pct_depressed_7d,
            date = as.Date(survey_date, format = "%Y%m%d"),
            sample_size,
            state = fct_lump(region, n = 5),
            state = fct_relevel(state, "New South Wales", "Victoria")) %>%
  filter(date < as.Date("2020-12-31")) -> d1

plot_response(d1, title_str)

# Plot anxiety data
title_str = c(
  "Percent of population feeling anxious most or all of the time in past 7 days (2020)"
)

anxiety %>%
  transmute(response = pct_anxious_7d,
            date = as.Date(survey_date, format = "%Y%m%d"),
            sample_size,
            state = fct_lump(region, n = 5),
            state = fct_relevel(state, "New South Wales", "Victoria")) %>%
  filter(date < as.Date("2020-12-31")) -> d1

plot_response(d1, title_str)

anxiety_smoothed %>%
  transmute(response = smoothed_pct_anxious_7d,
            date = as.Date(survey_date, format = "%Y%m%d"),
            sample_size,
            state = fct_lump(region, n = 5),
            state = fct_relevel(state, "New South Wales", "Victoria")) %>%
  filter(date < as.Date("2020-12-31")) -> d1

plot_response(d1, title_str)

# Plot financial concern data
title_str = c(
  "Percent of population with financial concerns (2020)"
)

finance %>%
  transmute(response = percent_hf,
            date = as.Date(survey_date, format = "%Y%m%d"),
            sample_size,
            state = fct_lump(region, n = 5),
            state = fct_relevel(state, "New South Wales", "Victoria")) %>%
  filter(date < as.Date("2020-12-31")) -> d1

plot_response(d1, title_str)

finance_smoothed %>%
  transmute(response = smoothed_hf,
            date = as.Date(survey_date, format = "%Y%m%d"),
            sample_size,
            state = fct_lump(region, n = 5),
            state = fct_relevel(state, "New South Wales", "Victoria")) %>%
  filter(date < as.Date("2020-12-31")) -> d1

plot_response(d1, title_str)
