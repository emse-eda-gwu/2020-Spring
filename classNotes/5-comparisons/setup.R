library(knitr)
library(tidyverse)
library(cowplot)
library(ggrepel)
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
college_all_ages <- read_csv(here('data', 'college_all_ages.csv'))
federal_spending <- read_csv(here('data', 'federal_spending_long.csv'))
gapminder        <- read_csv(here('data', 'gapminder.csv'))
marathon         <- read_csv(here('data', 'marathon.csv'))
milk_production  <- read_csv(here('data', 'milk_production.csv'))
