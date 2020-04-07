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

folder <- '7-cleaning-joins'
file   <- 'slides-7-cleaning-joins'

folder <- '8-polish'
file   <- 'slides-8-polish'

folder <- '9-maps'
file   <- 'slides-9-maps'

folder <- '11-correlation'
file   <- 'slides-11-correlation'

folder <- '13-communicating'
file   <- 'slides-13-communicating'

# Save the slides
pagedown::chrome_print(
    input  = here('classNotes', folder, paste0(file, '.html')),
    output = here('classNotes', folder, paste0(file, '.pdf')))
