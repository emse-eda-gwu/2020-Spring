library(here)
source(here('setup.R'))

college_summary <- college_all_ages %>% 
    mutate(
        major_category = fct_reorder(major_category, median))


