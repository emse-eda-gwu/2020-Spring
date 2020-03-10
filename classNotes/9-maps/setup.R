library(knitr)
library(ggrepel)
library(cowplot)
library(viridis)
library(tidyverse)
library(here)
library(ggrepel)
library(janitor)
library(countdown)

# install.packages('maps')
# install.packages('sf')
# install.packages('rgeos')
# install.packages('rnaturalearth')
# devtools::install_github("ropensci/rnaturalearthhires")
# devtools::install_github("ropensci/rnaturalearthdata")
library(maps)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)

theme_set(theme_minimal(base_size = 18))

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
milk_production  <- read_csv(here::here('data', 'milk_production.csv'))
us_coffee_shops  <- read_csv(here::here('data', 'us_coffee_shops.csv'))

# Filter out coffee shops to continental 48 states
us_coffee_shops <- us_coffee_shops %>%
    filter(lat > 22,    lat < 50,
           long > -150, long < -66)

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
