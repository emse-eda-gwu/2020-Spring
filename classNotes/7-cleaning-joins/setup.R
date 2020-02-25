library(knitr)
library(tidyverse)
library(cowplot)
library(readxl)
library(lubridate)
library(janitor)
library(here)
library(countdown)

options(
    htmltools.dir.version = FALSE,
    knitr.table.format = "html",
    knitr.kable.NA = '',
    tibble.width = Inf)
knitr::opts_chunk$set(
    warning = FALSE,
    message = FALSE,
    fig.retina = 3)
dplyr.width = Inf

# Read in data
wildlife_impacts <- read_csv(here::here('data', 'wildlife_impacts.csv'))
milk_production  <- read_csv(here::here('data', 'milk_production.csv'))
msleep           <- read_csv(here::here('data', 'msleep.csv'))
tuberculosis_rates <- table3

# Add new variables
wildlife_impacts_orig <- wildlife_impacts
wildlife_impacts <- wildlife_impacts %>%
    mutate(
        weekday_name = wday(incident_date, label = TRUE),
        phase_of_flt = str_to_lower(phase_of_flt),
        phase_of_flt = case_when(
            phase_of_flt %in% c('approach', 'arrival', 'descent',
                                'landing roll') ~ 'arrival',
            phase_of_flt %in% c('climb', 'departure',
                                'take-off run') ~ 'departure',
            TRUE ~ 'other'))

# Abbreviations: https://www.50states.com/abbreviations.htm
state_abbs <- tibble::tribble(
                  ~state_name,              ~state_abb,
                     "Alabama",             "AL",
                      "Alaska",             "AK",
                     "Arizona",             "AZ",
                    "Arkansas",             "AR",
                  "California",             "CA",
                    "Colorado",             "CO",
                 "Connecticut",             "CT",
                    "Delaware",             "DE",
                     "Florida",             "FL",
                     "Georgia",             "GA",
                      "Hawaii",             "HI",
                       "Idaho",             "ID",
                    "Illinois",             "IL",
                     "Indiana",             "IN",
                        "Iowa",             "IA",
                      "Kansas",             "KS",
                    "Kentucky",             "KY",
                   "Louisiana",             "LA",
                       "Maine",             "ME",
                    "Maryland",             "MD",
               "Massachusetts",             "MA",
                    "Michigan",             "MI",
                   "Minnesota",             "MN",
                 "Mississippi",             "MS",
                    "Missouri",             "MO",
                     "Montana",             "MT",
                    "Nebraska",             "NE",
                      "Nevada",             "NV",
               "New Hampshire",             "NH",
                  "New Jersey",             "NJ",
                  "New Mexico",             "NM",
                    "New York",             "NY",
              "North Carolina",             "NC",
                "North Dakota",             "ND",
                        "Ohio",             "OH",
                    "Oklahoma",             "OK",
                      "Oregon",             "OR",
                "Pennsylvania",             "PA",
                "Rhode Island",             "RI",
              "South Carolina",             "SC",
                "South Dakota",             "SD",
                   "Tennessee",             "TN",
                       "Texas",             "TX",
                        "Utah",             "UT",
                     "Vermont",             "VT",
                    "Virginia",             "VA",
                  "Washington",             "WA",
               "West Virginia",             "WV",
                   "Wisconsin",             "WI",
                     "Wyoming",             "WY",
        "District of Columbia",             "DC",
            "Marshall Islands",             "MH",
         "Armed Forces Africa",             "AE",
       "Armed Forces Americas",             "AA",
         "Armed Forces Canada",             "AE",
         "Armed Forces Europe",             "AE",
    "Armed Forces Middle East",             "AE",
        "Armed Forces Pacific",             "AP"
)

# # scraping version:
# library(rvest)
# html <- read_html('https://www.50states.com/abbreviations.htm')
# table <- html %>%
#     html_node('#content') %>%
#     html_nodes("table") %>%
#     html_table(fill = T)
# table[[1]]
