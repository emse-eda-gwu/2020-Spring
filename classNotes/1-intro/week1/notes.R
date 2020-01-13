library(tidyverse)
library(here)

# Read in .csv files with read_csv():
csvPath <- here('data', 'milk_production.csv')
milk_production <- read_csv(csvPath)

# Read in .txt files with read.table():
txtPath <- here('data', 'nasa_global_temps.txt')
global_temps <- read.table(txtPath, skip = 5, header = FALSE)
# Rename the variables
names(global_temps) <- c('year', 'meanTemp', 'smoothTemp')

# Read in .xlsx files with read_excel():
library(readxl)
xlsxPath <- here('data', 'pv_cell_production.xlsx')
pv_cells <- read_excel(xlsxPath, sheet = 'Cell Prod by Country', skip = 2)
# Drop rows that don't contain data
pv_cells <- pv_cells %>%
    filter(is.na(Year) == FALSE, Year <= 2013)

# ----------------------------------------------------------------------
# Importing data practice:
#
# Write code to import the following files inside the "data" folder:
#
#  - wildlife_impacts.csv
#  - north_america_bear_killings.txt
#  - .xlsx



# ----------------------------------------------------------------------
# Data provenance practice:
#
# Site 1 - wildlife_impacts.csv
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-07-23
#
# Site 2 - north_america_bear_killings.txt
# https://data.world/makeovermonday/2019w21
#
# Site 3 - pc_sales_2018.xlsx
# http://www.oica.net/category/sales-statistics/

# Go to each of the above sites, then add the following information about the data to the "data_sources.txt" file:
#
# - The name of the downloaded file in the "`data`" folder.
# - The date you downloaded the file (i.e. today's date).
# - The url to the site you downloaded the data from.
# - The source of the _original_ data (if different from where you downloaded the data).
# - A short description of the data and how they were collected.
# - A dictionary for the data (if available).


# ----------------------------------------------------------------------
# Tidy data

# Read in data:
library(readxl)
xlsxPath <- here('data', 'pv_cell_production.xlsx')
pv_cells <- read_excel(xlsxPath, sheet = 'Cell Prod by Country', skip = 2) %>%
    filter(is.na(Year) == FALSE, Year <= 2013)

# Convert to "long" format (tidy)
pv_cells_long <- pv_cells %>%
    gather(key = 'Country', value = 'nPVCells', China:World)

# Convert to "wide" format (un-tidy)
pv_cells_wide <- pv_cells_long %>%
    spread(key = Country, value = nPVCells)


# ----------------------------------------------------------------------
# Tidy data practice

# 1. Read in the `lotr_words.csv` data file.
# 2. Use `gather()` to "tidy" the data into four columns:
#    Film, Race, Gender, Words.
# 3. Use `write_csv()` and `here()` to save your file at
#    `lotr_words_tidy.csv` in the `data` folder.



# 1. Read in the `lotr_words_tidy.csv` data file.
# 2. Use `spread()` to convert the "tidy" data back into it's
#    untidy format with the columns: Film, Race, Female, Male.
