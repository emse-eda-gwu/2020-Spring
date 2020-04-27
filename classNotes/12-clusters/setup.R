library(knitr)
library(ggrepel)
library(cowplot)
library(viridis)
library(tidyverse)
library(ggridges)
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

# Set main theme settings
theme_set(theme_gray(base_size = 18))

# Read in data
gapminder        <- read_csv(here('data', 'gapminder.csv'))
wildlife_impacts <- read_csv(here('data', 'wildlife_impacts.csv'))
college_all_ages <- read_csv(here('data', 'college_all_ages.csv'))
lotr_words       <- read_csv(here('data', 'lotr_words.csv'))
federal_spending <- read_csv(here('data', 'federal_spending_long.csv'))
avengers         <- read_csv(here('data', 'avengers.csv'))
milk_production  <- read_csv(here('data', 'milk_production.csv'))
marathon         <- read_csv(here('data', 'marathon.csv'))
