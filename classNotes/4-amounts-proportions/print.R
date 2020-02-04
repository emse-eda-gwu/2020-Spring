# Print a pdf copy
library(pagedown)

slidesName <- "slides-4-amounts-proportions"

htmlName <- paste0(slidesName, '.html')
pagedown::chrome_print(htmlName)
