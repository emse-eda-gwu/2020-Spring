# install.packages('HistData')
# install.packages('GGally')
library(HistData)
library(GGally)
library(lubridate)
library(countdown)
library(knitr)
library(cowplot)
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

# Set main theme settings
theme_set(theme_gray(base_size = 18))

# Read in data
wildlife_impacts <- read_csv(here::here('data', 'wildlife_impacts.csv'))
msleep           <- read_csv(here::here('data', 'msleep.csv'))
