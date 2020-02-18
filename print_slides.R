library(pagedown)
library(here)

folder <- '1-intro'
file   <- 'slides-1-intro'

folder <- '2-eda'
file   <- 'slides-2-eda'

folder <- '4-amounts-proportions'
file   <- 'slides-4-amounts-proportions'

folder <- '5-comparisons'
file   <- 'slides-5-comparisons'

folder <- '6-trends'
file   <- 'slides-6-trends'

# Save the slides
pagedown::chrome_print(
    input  = here('classNotes', folder, paste0(file, '.html')),
    output = here('classNotes', folder, paste0(file, '.pdf')))
