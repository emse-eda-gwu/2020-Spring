library(knitr)
library(cowplot)
library(tidyverse)
library(viridis)
library(gganimate)
library(plotly)
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

# Set main plot theme
theme_set(theme_cowplot(font_size = 20))

# Read in data
gapminder        <- read_csv(here::here('data', 'gapminder.csv'))
milk_production  <- read_csv(here::here('data', 'milk_production.csv'))
global_temps     <- read_csv(here::here('data', 'nasa_global_temps.csv'))
hotdogs          <- read_csv(here::here('data', 'hot_dog_winners.csv'))
internet_country <- read_csv(here::here('data', 'internet_users_country.csv'))
internet_region  <- read_csv(here::here('data', 'internet_users_region.csv'))
us_diseases      <- read_csv(here::here('data', 'us_contagious_diseases.csv'))

# Modify data
milk_production <- milk_production %>%
    mutate(
        milk_produced = milk_produced / 10^9,
        state = fct_reorder(state, milk_produced))
gapminder <- gapminder %>%
    mutate(year = as.integer(year))

# Make data subsets
milk_ca <- milk_production %>%
    filter(state == 'California')

milk_ca_sparse <- milk_ca %>%
    mutate(yearColor = ifelse(year %in% seq(1970, 2020, 10), 'one', 'two'))

milk_region <- milk_production %>%
    filter(region %in% c(
        'Pacific', 'Northeast', 'Lake States', 'Mountain')) %>%
    group_by(year, region) %>%
    summarise(milk_produced = sum(milk_produced)) %>%
    ungroup() %>%
    mutate(label = ifelse(year == max(year), region, NA))

milk_race <- milk_production %>%
    group_by(year) %>%
    mutate(
        rank = rank(-milk_produced),
        Value_rel = milk_produced / milk_produced[rank==1],
        Value_lbl = paste0(' ', round(milk_produced))) %>%
    group_by(state) %>%
    filter(rank <= 10) %>%
    ungroup() %>%
    mutate(year = as.integer(year))

hotdogs_mens <- hotdogs %>%
    filter(Competition == 'Mens') %>%
    rename(dogs = `Dogs eaten`,
           record = `New record`) %>%
    mutate(
        record = if_else(record == 1, 'Yes', 'No'),
        Winner = fct_other(Winner,
            keep = c('Joey Chestnut', 'Takeru Kobayashi')))

internet_country_summary <- internet_country %>%
    filter(country %in% c(
        'United States', 'China',
        'Singapore', 'Cuba')) %>%
    mutate(
        label = ifelse(year == max(year), country, NA))

internet_region_summary <- internet_region %>%
    mutate(
        numUsers = numUsers / 10^9,
        label    = ifelse(year == max(year), region, NA))

measles <- us_diseases %>%
    filter(!state %in% c("Hawaii", "Alaska"),
           disease == 'Measles',
           ! is.na(count),
           ! is.na(population)) %>%
    mutate(
        rate = (count / population) * 10000,
        state = fct_reorder(state, rate)) %>%
    filter(! is.na(rate))

measles_ca <- measles %>%
    filter(state == "California")

measles_us <- measles %>%
    group_by(year) %>%
    summarize(rate = sum(count) / sum(population) * 10000)
