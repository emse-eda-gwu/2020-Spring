library(knitr)
library(here)
library(tidyverse)
library(fontawesome) # from github: https://github.com/rstudio/fontawesome
options(knitr.kable.NA = '')

# default figure size
knitr::opts_chunk$set(
    fig.width  = 4.75,
    fig.height = 4.75,
    fig.align  = "center"
)
