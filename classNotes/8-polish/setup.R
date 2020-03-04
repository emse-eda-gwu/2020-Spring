library(hrbrthemes)
library(knitr)
library(cowplot)
library(viridis)
library(lubridate)
library(countdown)
library(tidyverse)
library(here)
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
lotr_words       <- read_csv(here::here('data', 'lotr_words.csv'))
federal_spending <- read_csv(here::here('data', 'federal_spending_long.csv'))
milk_production  <- read_csv(here::here('data', 'milk_production.csv'))
msleep           <- read_csv(here::here('data', 'msleep.csv'))
us_gdp <- read_csv(here::here('data', 'total_gdp_us_inflation_adjusted.csv'))

# Make data summaries
lotr_summary <- lotr_words %>%
    gather(key = 'gender', value = 'wordCount', Female:Male) %>%
    group_by(Race, gender) %>%
    summarise(wordCount = sum(wordCount)) %>%
    ungroup() %>%
    mutate(Race = fct_reorder(Race, desc(wordCount)))

us_gdp <- us_gdp %>%
    gather(key = year, value = gdp, `1960`:`2017`) %>%
    filter(country == 'United States') %>%
    mutate(gdp = gdp / 10^12,
           year = as.numeric(year))

federal_spending_2017 <- federal_spending %>%
    filter(year == 2017) %>%
    mutate(rd_budget = rd_budget / 10^3) %>%
    mutate(department = fct_reorder(
        department, rd_budget))

federal_spending_summary <- federal_spending %>%
    mutate(department = fct_other(
        department, keep = 'DOD')) %>%
    group_by(department, year) %>%
    summarise(rd_budget = sum(rd_budget) / 10^3) %>%
    ungroup() %>%
    mutate(department = fct_relevel(
        department, c('Other', 'DOD')))

wildlife_impacts_2016 <- wildlife_impacts %>%
    filter(incident_year == 2016) %>%
    mutate(
        phase_of_flt = str_to_lower(phase_of_flt),
        phase_of_flt = case_when(
            phase_of_flt %in% c('approach', 'arrival', 'descent',
                                'landing roll') ~ 'arrival',
            phase_of_flt %in% c('climb', 'departure',
                                'take-off run') ~ 'departure',
            TRUE ~ 'other'),
        phase_of_flt = str_to_title(phase_of_flt))

wildlife_costs <- wildlife_impacts %>%
    rename(cost = cost_repairs_infl_adj) %>%
    filter(! is.na(cost)) %>%
    mutate(cost = cost / 10^6,
           incident_date = as.Date(incident_date))

milk_region <- milk_production %>%
    filter(region %in% c(
        'Pacific', 'Northeast', 'Lake States', 'Mountain')) %>%
    group_by(year, region) %>%
    summarise(milk_produced = sum(milk_produced)) %>%
    ungroup() %>%
    mutate(label = ifelse(year == max(year), region, NA))

milk_top10states <- milk_production %>%
    filter(year == 2017) %>%
    arrange(desc(milk_produced)) %>%
    slice(1:10) %>%
    mutate(
        milk_produced = milk_produced / 10^9,
        state = fct_reorder(state, milk_produced))

milk_summary_dumbbell <- milk_production %>%
    filter(
        year %in% c(1970, 2017),
        state %in% milk_top10states$state) %>%
    mutate(
        # Reorder state variables to follow top 10 states
        state = fct_relevel(state, levels(milk_top10states$state)),
        # Convert year to discrete variable
        year = as.factor(year),
        # Modify the units
        milk_produced = milk_produced / 10^9)
