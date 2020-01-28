#-----------------------------------------------------------------------------
# All the code to create the images in this week's lecture slides
# The majority of the plots come from John Rauser's code for his
# presentation on "How Humans See Data", first presented at
# Velocity Amsterdam 2016:
# http://conferences.oreilly.com/velocity/devops-web-performance-eu/public/schedule/detail/54354)

library(tidyverse)
library(here)
library(RColorBrewer)
library(scales)
library(cowplot)
library(ggrepel)
library(MASS)
library(viridis)
library(ggforce)
set.seed(42)

#-----------------------------------------------------------------------------
# Climate change barcode

climateChangeBarcode <- read.table(
    here('data', 'nasa_global_temps.txt'),
    col.names = c('year', 'meanTempCelsius', 'smoothTempCelsius'), skip=5) %>%
    mutate(group = "group") %>%
    ggplot(aes(x = group, y = as.factor(year))) +
    geom_tile(aes(fill = meanTempCelsius)) +
    scale_fill_gradientn(colours = rev(brewer.pal(10, "RdBu"))) +
    coord_flip() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        plot.margin = margin(-2, 0, -2, 0, "cm"))

ggsave(here('images', 'plots', 'climateChangeBarcode.png'),
       climateChangeBarcode, width = 9, height = 4, dpi = 150)

#-----------------------------------------------------------------------------
# Student engagement

engagement_data <- data.frame(
    Male   = c(643, 735, 590, 863),
    Female = c(793, 928, 724, 662),
    School = c('Special Ed., Charter', 'Special Ed., Public',
               'General Ed., Charter', 'General Ed., Public'),
    Highlight = c(0, 0, 0, 1)) %>%
    gather(Gender, Engagement, Male:Female) %>%
    mutate(
        Gender = fct_relevel(Gender, c('Male', 'Female')),
        Highlight = as.factor(Highlight),
        x = ifelse(Gender == 'Female', 1, 0))

engagement <- engagement_data %>%
    mutate(Highlight = as.factor(Highlight)) %>%
    ggplot(aes(x = Gender, y = Engagement,
               group = School, color = School)) +
    geom_point() +
    geom_line() +
    theme_cowplot() +
    labs(x = 'Sex', y = 'Engagement')

engagement_labeled <- engagement_data %>%
    mutate(Highlight = as.factor(Highlight)) %>%
    ggplot(aes(x = x, y = Engagement, group = School, color = School)) +
    geom_point() +
    geom_line() +
    labs(x = 'Sex', y = 'Engagement') +
    theme_cowplot() +
    scale_x_continuous(limits = c(-0.2, 2), labels = c('Male', 'Female'),
                       breaks = c(0, 1)) +
    theme(legend.position = 'none') +
    geom_text_repel(aes(label = School, color = as.factor(School)),
                    data          = subset(engagement_data, Gender == 'Female'),
                    size          = 5,
                    nudge_x       = 0.2,
                    hjust         = 0,
                    segment.color = NA)

engagement_final <- engagement_data %>%
    ggplot(aes(x = x, y = Engagement, group = School, color = Highlight)) +
    geom_point() +
    geom_line() +
    scale_color_manual(values = c('#757575', '#ed573e')) +
    labs(x = 'Sex', y = 'Engagement',
         title = paste0('Female students in public, general education\n',
                        'schools have surprisingly low engagement')) +
    scale_x_continuous(limits = c(-1.2, 1.2), labels = c('Male', 'Female'),
                       breaks = c(0, 1)) +
    geom_text_repel(aes(label = Engagement, color = as.factor(Highlight)),
                    data          = subset(engagement_data, Gender == 'Female'),
                    size          = 5,
                    nudge_x       = 0.1,
                    segment.color = NA) +
    geom_text_repel(aes(label = Engagement, color = as.factor(Highlight)),
                    data          = subset(engagement_data, Gender == 'Male'),
                    size          = 5,
                    nudge_x       = -0.1,
                    segment.color = NA) +
    geom_text_repel(aes(label = School, color = as.factor(Highlight)),
                    data          = subset(engagement_data, Gender == 'Male'),
                    size          = 5,
                    nudge_x       = -0.25,
                    hjust         = 1,
                    segment.color = NA) +
    theme_cowplot() +
    background_grid(major = 'x') +
    theme(axis.line = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          legend.position = 'none')

ggsave(here('images', 'plots', 'engagement.png'),
       engagement, width = 5, height = 4)
ggsave(here('images', 'plots', 'engagement_labeled.png'),
       engagement_labeled, width = 5, height = 4)
ggsave(here('images', 'plots', 'engagement_final.png'),
       engagement_final, width = 6, height = 5)

#-----------------------------------------------------------------------------
# Anscombe Plots

x       <- c(anscombe$x1, anscombe$x2, anscombe$x3, anscombe$x4)
y       <- c(anscombe$y1, anscombe$y2, anscombe$y3, anscombe$y4)
quartet <- rep(c('A', 'B', 'C', 'D'), each = nrow(anscombe))
ansDf   <- tibble(x, y, quartet)
anscombePlot <- ansDf %>%
    ggplot(aes(x = x, y = y)) +
    geom_point(alpha = 0.6, size = 2) +
    stat_smooth(method = 'lm', se = F, size = 0.4, alpha = 0.6) +
    facet_wrap(vars(quartet)) +
    scale_x_continuous(breaks=seq(4, 18, 2), limits=c(4, 19)) +
    scale_y_continuous(breaks=seq(4, 12, 2), limits=c(3, 13)) +
    theme_bw() +
    theme(
        axis.ticks = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(0, 0, 0, 0, "cm"))

ggsave(here('images', 'plots', 'anscombePlot.png'),
       anscombePlot, width = 8, height = 6)

# Print out table of data
write_csv(anscombe, here('images', 'plots', 'anscombe.csv'))

#-----------------------------------------------------------------------------
# Graph quality over time

graphQuality <- data.frame(
    x = c(1600, 1700, 1800, 1900, 1980, 2020),
    y = c(20, 100, 200, 300, 400, 40)) %>%
    ggplot() +
    geom_line(aes(x = x, y = y), size = 1.5) +
    theme_minimal_hgrid() +
    labs(x = 'Year', y = 'Graph quality', title = 'Graphing quality over time')

ggsave(here('images', 'plots', 'graphQuality.png'),
       graphQuality, width = 6, height = 3.5)

#-----------------------------------------------------------------------------
# mammals

mammalsScatter <- mammals %>%
    ggplot(aes(x = body, y = brain)) +
    geom_point(alpha = 0.6) +
    theme_bw() +
    scale_x_log10() +
    scale_y_log10() +
    stat_smooth(method = 'lm', col = 'red', se = F, size = 0.7) +
    labs(x = 'log(body weight)', y = 'log(brain weight)')

ggsave(here('images', 'plots', 'mammalsScatter.png'),
       mammalsScatter, width = 5, height = 4)

#-----------------------------------------------------------------------------
# Mtcars scatterplot

mtCarsScatter <- mtcars %>%
    ggplot(aes(x = mpg, y = hp)) +
    geom_point() +
    theme_cowplot() +
    labs(color = 'Cylinders')

mtCarsScatterSmooth <- mtCarsScatter +
    geom_smooth()

mtCarsScatterGradient <- mtcars %>%
    ggplot(aes(x = wt, y = hp)) +
    geom_point(aes(color = mpg), size = 1.5) +
    theme_cowplot()

mtCarsScatterColor <- mtcars %>%
    ggplot() +
    geom_point(aes(x = mpg, y = hp, color = as.factor(cyl))) +
    theme_cowplot() +
    labs(color = 'Cylinders')

mtCarsScatterShape1 <- mtcars %>%
    ggplot() +
    geom_point(aes(x = mpg, y = hp, shape = as.factor(cyl))) +
    theme_cowplot() +
    labs(shape = 'Cylinders')

mtCarsScatterShape2 <- mtcars %>%
    ggplot() +
    geom_point(aes(x = mpg, y = hp, shape = as.factor(cyl))) +
    scale_shape_manual("Cylinders", values = c(1,2,3)) +
    theme_cowplot() +
    labs(shape = 'Cylinders')

mtCarsScatterShape3 <- mtcars %>%
    ggplot() +
    geom_point(aes(x = mpg, y = hp, shape = as.factor(cyl))) +
    scale_shape_manual(values = c(52,54,56)) +
    theme_cowplot() +
    labs(shape = 'Cylinders')

mtCarsScatterGray <- mtcars %>%
    ggplot() +
    geom_point(aes(x = mpg, y = hp), color = 'gray80') +
    theme_cowplot() +
    labs(color = 'Cylinders')

mtCarsScatterSmall <- mtcars %>%
    ggplot() +
    geom_point(aes(x = mpg, y = hp), size = 0.3) +
    theme_cowplot() +
    labs(color = 'Cylinders')

mtCarsScatterGrid1 <- mtcars %>%
    ggplot() +
    geom_point(aes(x = mpg, y = hp)) +
    theme_cowplot() +
    labs(color = 'Cylinders') +
    theme(panel.grid.major = element_line(colour = "grey20", size = 0.2))

mtCarsScatterGrid2 <- mtcars %>%
    ggplot() +
    geom_point(aes(x = mpg, y = hp)) +
    theme_cowplot() +
    labs(color = 'Cylinders') +
    theme(
        panel.border     = element_rect(fill = NA),
        panel.grid.minor = element_line(size = 0.2),
        panel.grid.major = element_line(size = 0.5))

mtCarsScatterGrid3 <- mtcars %>%
    ggplot() +
    geom_point(aes(x = mpg, y = hp)) +
    theme_cowplot() +
    labs(color = 'Cylinders') +
    theme(
        panel.border     = element_rect(fill = NA),
        panel.grid.minor = element_line(size = 0.5),
        panel.grid.major = element_line(size = 1))

mtCarsScatterGrid4 <- mtcars %>%
    ggplot() +
    geom_point(aes(x = mpg, y = hp), size = NA) +
    theme_cowplot() +
    labs(color = 'Cylinders') +
    theme(
        panel.border     = element_rect(fill = NA),
        panel.grid.major = element_line(size = 5))

mtCarsScatterGrid5 <- mtcars %>%
    ggplot() +
    geom_point(aes(x = mpg, y = hp), alpha = 0.7) +
    theme_cowplot() +
    labs(color = 'Cylinders') +
    theme(
        axis.line = element_line(color = "grey70"),
        axis.ticks = element_line(color = "grey70"),
        axis.text = element_text(color = "grey20"),
        axis.title = element_text(color = "grey20"),
        panel.border     = element_rect(fill = NA, colour = "grey70"),
        panel.grid.major = element_line(colour = "grey80", size = 0.2))

mtCarsScatterLabels <- mtCarsScatter + 
    geom_mark_ellipse(aes(fill = as.factor(cyl), 
                          label = paste0(cyl, 'Cylinders'))) +
    geom_point() + 
    theme(legend.position = 'none')

ggsave(here('images', 'plots', 'mtCarsScatter.png'),
       mtCarsScatter, width = 4, height = 3)

ggsave(here('images', 'plots', 'mtCarsScatterSmooth.png'),
       mtCarsScatterSmooth, width = 4, height = 3)

ggsave(here('images', 'plots', 'mtCarsScatterGradient.png'),
       mtCarsScatterGradient, width = 4, height = 3)

ggsave(here('images', 'plots', 'mtCarsScatterColor.png'),
       mtCarsScatterColor, width = 4, height = 3)

ggsave(here('images', 'plots', 'mtCarsScatterShape1.png'),
       mtCarsScatterShape1, width = 4, height = 3)

ggsave(here('images', 'plots', 'mtCarsScatterShape2.png'),
       mtCarsScatterShape2, width = 4, height = 3)

ggsave(here('images', 'plots', 'mtCarsScatterShape3.png'),
       mtCarsScatterShape3, width = 4, height = 3)

ggsave(here('images', 'plots', 'mtCarsScatterSmall.png'),
       mtCarsScatterSmall, width = 4, height = 3)

ggsave(here('images', 'plots', 'mtCarsScatterGray.png'),
       mtCarsScatterGray, width = 4, height = 3)

ggsave(here('images', 'plots', 'mtCarsScatterGrid1.png'),
       mtCarsScatterGrid1, width = 4, height = 3)

ggsave(here('images', 'plots', 'mtCarsScatterGrid2.png'),
       mtCarsScatterGrid2, width = 4, height = 3)

ggsave(here('images', 'plots', 'mtCarsScatterGrid3.png'),
       mtCarsScatterGrid3, width = 4, height = 3)

ggsave(here('images', 'plots', 'mtCarsScatterGrid4.png'),
       mtCarsScatterGrid4, width = 4, height = 3)

ggsave(here('images', 'plots', 'mtCarsScatterGrid5.png'),
       mtCarsScatterGrid5, width = 4, height = 3)

ggsave(here('images', 'plots', 'mtCarsScatterLabels.png'),
       mtCarsScatterLabels, width = 7, height = 5)

#-----------------------------------------------------------------------------
# Monster bars

monsterBars <- data.frame(
    year  = c(1972, 1974, 1976, 1978, 1980, '1982\nest.'),
    y = c(70, 85, 125, 200, 240, 310)) %>%
    ggplot() +
    geom_col(aes(x = year, y = y), width = 0.7) +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
    theme_minimal_hgrid() +
    labs(x = 'Year', y = 'Campaign expenditures ($ mil)',
         title = 'Total House and Senate\ncampaign expenditures')

ggsave(here('images', 'plots', 'monsterBars.png'),
       monsterBars, width = 4.5, height = 4)

#-----------------------------------------------------------------------------
# Preattentive color vs. shape

plotTheme <- function() {
    theme_bw() +
    theme(
        legend.position = 'none',
        axis.ticks = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(0, 0, 0, 0, "cm"))
}

# Create function to generate the data so that each plot has different points
makePlotData <- function() {
    plotData <- tibble(
        x      = runif(60, 0, 10),
        y      = runif(60, 0, 10),
        color  = c(rep('2', 59), '1'),
        group1 = c(rep('2', 20), rep('2', 20), rep('1', 19), '1'),
        group2 = c(rep('2', 20), rep('1', 20), rep('2', 19), '1'))
    return(plotData)
}

preattentive1 <- makePlotData() %>%
    ggplot(aes(x = x, y = y)) +
    geom_point(aes(color = color), size = 2) +
    scale_color_manual(values = c('red', 'blue')) +
    plotTheme() +
    labs(x = '', y = '')

preattentive2 <- makePlotData() %>%
    ggplot(aes(x = x, y = y)) +
    geom_point(aes(shape = color), color = 'red', , size = 2) +
    plotTheme() +
    labs(x = '', y = '')

preattentive3 <- makePlotData() %>%
    ggplot(aes(x = x, y = y)) +
    geom_point(aes(shape = group2, color = group1), size = 2) +
    scale_color_manual(values = c('red', 'blue')) +
    plotTheme() +
    labs(x = '', y = '')

ggsave(here('images', 'plots', 'preattentive1.png'),
    preattentive1, width = 4, height = 4)
ggsave(here('images', 'plots', 'preattentive2.png'),
    preattentive2, width = 4, height = 4)
ggsave(here('images', 'plots', 'preattentive3.png'),
    preattentive3, width = 4, height = 4)

#-----------------------------------------------------------------------------
# Preattentive dots

preattentiveData <- data.frame(
    x        = rnorm(100),
    y        = rnorm(100),
    category = rep(letters[1:10], each = 10))

preattentivePlot_bad <- preattentiveData %>%
    ggplot() +
    geom_point(aes(x = x, y = y, color = category), size = 1) +
    theme_bw()

preattentivePlot_good <- preattentiveData %>%
    ggplot() +
    geom_point(aes(x = x, y = y), size = 1) +
    theme_bw() +
    facet_wrap(vars(category))

ggsave(here('images', 'plots', 'preattentivePlot_bad.png'),
       preattentivePlot_bad, width = 4, height = 3)
ggsave(here('images', 'plots', 'preattentivePlot_good.png'),
       preattentivePlot_good, width = 5, height = 5)

#-----------------------------------------------------------------------------
# faceted bars

facetedBars <- data.frame(
    count  = c(5, 12, 14, 7, 13, 17, 8, 18, 19),
    type = c(rep(paste('type', seq(3)), 3)),
    group = c(rep('group 1', 3), rep('group 2', 3), rep('group 3', 3))) %>%
    ggplot() +
    geom_col(aes(x = type, y = count)) +
    facet_grid(group ~ .) +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
    coord_flip() +
    theme_bw()

ggsave(here('images', 'plots', 'facetedBars.png'),
       facetedBars, width = 7, height = 5)

#-----------------------------------------------------------------------------
# simple bars

simpleBars <- data.frame(
    count  = seq(5),
    group = c(LETTERS[1:5])) %>%
    ggplot() +
    geom_col(aes(x = group, y = count), width = 0.7) +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
    theme_minimal_hgrid()

ggsave(here('images', 'plots', 'simpleBars.png'),
       simpleBars, width = 4.5, height = 4)

#-----------------------------------------------------------------------------
# mpg bars

mpg_bars <- mpg %>%
    ggplot() +
    geom_bar(aes(class), width = 0.8) +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
    theme_cowplot()
mpg_bars_flipped <- mpg_bars +
    coord_flip()
mpg_bars_flipped_sorted <- mpg %>%
    group_by(class) %>%
    summarise(count = n()) %>%
    mutate(class = fct_reorder(class, count)) %>%
    ggplot() +
    geom_col(aes(x=class, y=count), width = 0.8) +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
    coord_flip() +
    theme_cowplot()

ggsave(here('images', 'plots', 'mpg_bars.png'),
       mpg_bars, width = 6, height = 5)
ggsave(here('images', 'plots', 'mpg_bars_bad.png'),
       mpg_bars, width = 5, height = 5)
ggsave(here('images', 'plots', 'mpg_bars_flipped.png'),
       mpg_bars_flipped, width = 6, height = 4)
ggsave(here('images', 'plots', 'mpg_bars_flipped_sorted.png'),
       mpg_bars_flipped_sorted, width = 6, height = 4)

#-----------------------------------------------------------------------------
# Bar chart at zero

barchart_zero <- data.frame(
    x = c('March 27', 'March 31 Goal'),
    y = c(6, 7.066)) %>%
    ggplot(aes(x = x, y = y)) +
    geom_col(fill = 'blue', width = 0.7) +
    theme_minimal_hgrid() +
    labs(x= '', y = 'Millions of enrollments') +
    scale_y_continuous(breaks = seq(0, 8, 2), limits = c(0,8),
                       expand = expand_scale(mult = c(0, 0.05)))

ggsave(here('images', 'plots', 'barchart_zero.png'),
       barchart_zero, width = 3, height = 4)

#-----------------------------------------------------------------------------
# Pies

statesData <- data.frame(
    area = state.area,
    region = state.region) %>%
    group_by(region) %>%
    summarise(area = sum(area)) %>%
    mutate(region = fct_reorder(region, area))

statesPie <- statesData %>%
    ggplot(aes(x = '', y = area, fill = region)) +
    geom_col(width = 1) +
    scale_fill_brewer(palette = "Spectral") +
    coord_polar("y", start = 0) +
    theme(axis.text.x = element_blank()) +
    geom_text(aes(y = area/4 + c(0, cumsum(area)[-length(area)]),
              label = region, size = 5)) +
    theme_void() +
    theme(legend.position = "none")

statesBar <- statesData %>%
    ggplot(aes(x = region, y = area, fill = region)) +
    geom_col(position = 'dodge') +
    scale_fill_brewer(palette = "Spectral") +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
    coord_flip() +
    theme_minimal_vgrid() +
    labs(x = 'Region', y = 'Area (Sq. Miles)') +
    theme(legend.position = 'none')

ggsave(here('images', 'plots', 'statesPie.png'),
    statesPie, width = 5, height = 4)
ggsave(here('images', 'plots', 'statesBar.png'),
    statesBar, width = 5, height = 4)

#-----------------------------------------------------------------------------
# Color blind

colorBlindData <- data.frame(
    x = runif(30, -1, 1),
    y = runif(30, -1, 1),
    group = rep(c('A', 'B'), 15))
colorBlindBad1 <- colorBlindData %>%
    ggplot() +
    geom_point(aes(x = x, y = y, color = group), size = 3) +
    scale_color_manual(values = c('red', 'green')) +
    theme_cowplot()
colorBlindBad2 <- colorBlindBad1 +
    scale_color_manual(values = c('brown', 'brown'))
colorBlindBad3 <- colorBlindData %>%
    ggplot() +
    geom_point(aes(x = x, y = y), size = 3) +
    facet_wrap(vars(group)) +
    theme_bw()

ggsave(here('images', 'plots', 'colorBlindBad1.png'),
       colorBlindBad1, width = 5, height = 4)
ggsave(here('images', 'plots', 'colorBlindBad2.png'),
       colorBlindBad2, width = 5, height = 4)
ggsave(here('images', 'plots', 'colorBlindBad3.png'),
       colorBlindBad3, width = 5, height = 4)

#-----------------------------------------------------------------------------
# Prisoner bars 

prisoner_bars <- data.frame(
    category = c('Violent', 'Property', 'Drug', 'Public Order', 'Other'), 
    rate = c(61.7, 73.8, 66.7, 62.2, 64.7)) %>% 
    ggplot() +
    geom_point(aes(x = fct_reorder(category, rate), y = rate)) + 
    geom_hline(yintercept = 67.5, linetype = 'dashed') +
    scale_y_continuous(breaks = seq(60, 75, 5), limits = c(60, 75),
                       expand = expand_scale(mult = c(0, 0.05))) + 
    annotate('text', label = 'All released prisoners', 
             x = 5, y = 67.7, hjust = 0) +
    coord_flip() + 
    theme_minimal_vgrid() + 
    labs(
        x = 'Offense category',
        y = 'Recidivism rate (%)', 
        title = 'Recidivism rate of prisoners released in 1994')

ggsave(here('images', 'plots', 'prisoner_bars.png'),
       prisoner_bars, width = 7, height = 4)

#-----------------------------------------------------------------------------
# Begin plots from Hauser
#-----------------------------------------------------------------------------

jr_theme <- function() {
    theme(
        axis.title       = element_blank(),
        axis.ticks.x     = element_blank(),
        panel.border     = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background  = element_blank(),
        legend.key       = element_blank(),
        legend.background = element_rect(colour = NA))
}

make_segment <- function(df, scale_low, scale_high) {
    scale_range <- scale_high-scale_low
    fraction <- (df$mpg-scale_low) / scale_range
    theta <- fraction * pi/2
    return(data.frame(x=c(0, cos(theta)),
                      y=c(0, sin(theta))))
}

# Make model an actual column in the dataset.
mtcars <- datasets::mtcars %>%
    mutate(model = rownames(datasets::mtcars))

jr_hue <- ggplot(mtcars, aes(x = "", y = model, fill = mpg)) +
    geom_tile() +
    scale_fill_viridis() +
    scale_x_discrete(expand = expand_scale(mult = c(0.01, 0.05))) +
    jr_theme()

jr_hue_ordered <- ggplot(mtcars, aes(x = "", y = reorder(model, mpg), fill = mpg)) +
    geom_tile() +
    scale_fill_viridis() +
    scale_x_discrete(expand = expand_scale(mult = c(0.01, 0.05))) +
    jr_theme()

jr_saturation <- ggplot(mtcars, aes(x = "", y = model, fill = mpg)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = muted("blue")) +
    scale_x_discrete(expand = expand_scale(mult = c(0.01, 0.05))) +
    ylab(NULL) +
    jr_theme()

jr_saturation_ordered <- ggplot(mtcars, aes(x = "", y = reorder(model, mpg), fill = mpg)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = muted("blue")) +
    scale_x_discrete(expand = expand_scale(mult = c(0.01, 0.05))) +
    ylab(NULL) +
    jr_theme()

jr_saturation_ordered_zero <- ggplot(mtcars, aes(x = "", y = reorder(model, mpg), fill = mpg)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = muted("blue")) +
    scale_x_discrete(expand = expand_scale(mult = c(0.01, 0.05))) +
    ylab(NULL) +
    jr_theme() +
    expand_limits(fill=0)

jr_area <- ggplot(arrange(mtcars, mpg, model),
                  aes(rep(1:4, 32/4), rep(1:(32/4), each=4))) +
    geom_point(aes(size=mpg)) +
    geom_text(aes(label=model),
              position=position_nudge(y=0.5), size=2) +
    expand_limits(x=c(0.5,4.5), y=c(1,8.5)) +
    ylab(NULL) +
    scale_size_area() +
    jr_theme() +
    theme(
        axis.text = element_blank(),
        axis.ticks = element_blank())

jr_angle <- mtcars %>%
    arrange(mpg, model) %>%
    group_by(model) %>%
    do(make_segment(., 0, max(mtcars$mpg))) %>%
    ungroup() %>%
    mutate(model = fct_reorder(model, y)) %>%
    ggplot(aes(x, y, group=model)) +
    geom_path() +
    facet_wrap(~model,ncol=8) +
    coord_equal() +
    jr_theme() +
    theme(
        strip.text.x = element_text(size=rel(0.5), hjust=0),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())

jr_length <- mtcars %>%
    mutate(random = runif(32, 0, 5)) %>%
    ggplot(aes(x=random, xend=mpg+random, y=model, yend=model)) +
    geom_segment(size=3) +
    scale_x_discrete(expand = expand_scale(mult = c(0.01, 0.05))) +
    xlab(NULL) +
    ylab(NULL) +
    jr_theme() +
    theme(axis.text.y=element_text(size=rel(0.7)),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

jr_bars <- mtcars %>%
    ggplot(aes(x=0, xend=mpg, y=model, yend=model)) +
    geom_segment(size=3) +
    scale_x_discrete(expand = expand_scale(mult = c(0.01, 0.05))) +
    xlab(NULL) +
    ylab(NULL) +
    jr_theme() +
    theme(axis.text.y=element_text(size=rel(0.7)),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

jr_position_non_aligned <- mtcars %>%
    mutate(model = fct_reorder(model, mpg)) %>%
    ggplot(aes(x=mpg, y="a")) +
    geom_point(size=1) +
    scale_x_continuous(breaks=c(10, 20, 30)) +
    facet_wrap(~model, ncol=8) +
    jr_theme() +
    theme(
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        strip.text.x = element_text(size=rel(0.5), hjust=0),
        panel.grid.major = element_line(colour = "grey50", size = 0.2),
        panel.grid.minor = element_line(colour = "grey50", size = 0.2, linetype="dotted"))

jr_position_aligned <- mtcars %>%
    ggplot(aes(x=mpg, y=model)) +
    geom_point() +
    scale_x_continuous(expand = expand_scale(mult = c(0.01, 0.05))) +
    ylab(NULL) +
    theme_minimal_vgrid() +
    theme(
        axis.text.y=element_text(size=rel(0.7)),
        axis.title.y=element_text(size=rel(0.7)),
        panel.grid.major =   element_line(colour = "grey20", size = 0.2))

jr_position_aligned_ordered <- mtcars %>%
    mutate(model = fct_reorder(model, mpg)) %>%
    ggplot(aes(x=mpg, y=model)) +
    geom_point() +
    scale_x_continuous(expand = expand_scale(mult = c(0.01, 0.05))) +
    theme_minimal_vgrid() +
    ylab(NULL) +
    theme(
        axis.text.y=element_text(size=rel(0.7)),
        axis.title.y=element_text(size=rel(0.7)))

jr_position_aligned_ordered_zero <- jr_position_aligned_ordered +
    scale_x_continuous(expand = expand_scale(mult = c(0, 0.05))) +
    expand_limits(x=0)

jr_diamonds_bars_stacked <- ggplot(diamonds, aes(clarity, fill=cut, group=cut)) +
    geom_bar(stat="count", position="stack") +
    scale_y_continuous("Count", labels = comma,
                       expand = expand_scale(mult = c(0, 0.05))) +
    theme_cowplot()

jr_diamonds_bars_dodged <- ggplot(diamonds, aes(clarity, fill=cut, group=cut)) +
    geom_bar(stat="count", position="dodge") +
    scale_y_continuous("Count", labels = comma,
                       expand = expand_scale(mult = c(0, 0.05))) +
    theme_cowplot()

jr_diamonds_line_total <- diamonds %>%
    group_by(clarity) %>%
    summarize(n=length(clarity)) %>%
    ggplot(aes(clarity, n)) +
    geom_line(group=1) +
    geom_point() +
    expand_limits(y=0) +
    scale_y_continuous("Count", labels=comma) +
    theme_cowplot()

jr_diamonds_line_cut <- diamonds %>%
    group_by(clarity, cut) %>%
    summarize(n=length(clarity)) %>%
    ggplot(aes(clarity, n, color=cut, group=cut)) +
    geom_line() +
    geom_point() +
    expand_limits(y=0) +
    scale_y_continuous("Count", labels=comma) +
    theme_cowplot()

# Phone data from wikipedia: https://en.wikipedia.org/wiki/Mobile_operating_system
phone_data<-
"Quarter	Windows Mobile	RIM	Symbian	iOS	Android	Bada	Windows Phone	Other
2016 Q2[39]	0	400.4	0	44395	296912	0	1971	680.6
2016 Q1[40]	0	660	0	51630	293771	0	2400	791
2015 Q4[41]	0	906.9	0	71526	325394	0	4395	887.3
2015 Q3[42]	0	977	0	46062	298797	0	5874	1133.6
2015 Q2[43]	0	1153	0	48086	271010	0	8198	1229
2015 Q1[44]	0	1325	0	60177	265012	0	8271	1268
2014 Q4[45]	0	1734	0	74832	279058	0	10425	1286.9
2014 Q3[46]	0	2420	0	38187	254354	0	9033	1310.2
2014 Q2[47]	0	2044	0	35345	243484	0	8095	2044
2014 Q1[44]	0	1714	0	43062	227549	0	7580	1371
2013 Q4[48]	0	1807	0	50224	219613	0	8534	1994
2013 Q3[49]	0	4401	458	30330	205023	633	8912	475
2013 Q2[50]	0	6180	631	31900	177898	838	7408	472
2013 Q1[51]	0	6219	1349	38332	156186	1371	5989	600
2012 Q4[52]	0	7333	2569	43457	144720	2684	6186	713
2012 Q3[53]	0	8947	4405	23550	122480	5055	4058	684
2012 Q2[54]	0	7991	9072	28935	98529	4209	4087	863
2012 Q1[55]	0	9939	12467	33121	81067	3842	2713	1243
2011 Q4[56]	0	13185	17458	35456	75906	3111	2759	1167
2011 Q3[57]	0	12701	19500	17295	60490	2479	1702	1018
2011 Q2[58]	0	12652	23853	19629	46776	2056	1724	1051
2011 Q1	982	13004	27599	16883	36350	1862	1600	1495
2010 Q4[56]	3419	14762	32642	16011	30801	2027	0	1488
2010 Q3[57]	2204	12508	29480	13484	20544	921	0	1991
2010 Q2[58]	3059	11629	25387	8743	10653	577	0	2011
2010 Q1[59]	3696	10753	24068	8360	5227	0	0	2403
2009 Q4[60]	4203	10508	23857	8676	4043	0	0	2517
2009 Q3[61]	3260	8523	18315	7040	1425	0	0	2531
2009 Q2[62]	3830	7782	20881	5325	756	0	0	2398
2009 Q1[63]	3739	7534	17825	3848	575	0	0	2986
2008 Q4[64]	4714	7443	17949	4079	639	0	0	3319
2008 Q3[65]	4053	5800	18179	4720	0	0	0	3763
2008 Q2[66]	3874	5594	18405	893	0	0	0	3456
2008 Q1[64]	3858	4312	18400	1726	0	0	0	4113
2007 Q4[64]	4374	4025	22903	1928	0	0	0	3536
2007 Q3[65]	4180	3192	20664	1104	0	0	0	3612
2007 Q2[66]	3212	2471	18273	270	0	0	0	3628
2007 Q1[64]	2931	2080	15844	0	0	0	0	4087"
phones <- read.delim(textConnection(phone_data))
phones <- phones %>%
    mutate(Other = Other + Bada + Windows.Phone,
           Bada = NULL,
           Windows.Phone = NULL,
           Quarter = str_replace(Quarter, "\\[\\d+\\]", ""),
           Year = as.integer(str_split_fixed(Quarter, " ", 2)[,1]),
           Quarter = str_split_fixed(Quarter, " ", 2)[,2],
           qtr = as.integer(str_replace(Quarter, "Q", "")),
           year = Year+0.25*(qtr-1)) %>%
    gather(os, ct, -Year, -Quarter, -qtr, -year) %>%
    mutate(ct = as.numeric(ct)) %>%
    group_by(year) %>%
    mutate(share = ct/sum(ct))

jr_phones_area <- ggplot(phones, aes(year, share, group=os, fill=os)) +
    geom_area() +
    theme_cowplot()

jr_phones_line <- ggplot(phones, aes(year, share, group=os, color=os)) +
    geom_line(size = 1) +
    theme_cowplot()

circleData <- read.delim(textConnection(
        "x	y
1.972	1.236
1.112	1.994
0	1.009
0.665	1.942
0.235	0.356
0.247	1.658
1.275	1.961
0.702	0.045
1.76	0.35
1.691	0.277
1.628	1.778
1.957	1.29
0.111	0.542
0.902	0.005
0.598	0.085
1.613	1.79
1.298	1.955
0.651	1.937
1.949	1.316
0.099	0.567
0.862	0.01
0.027	0.768
0.706	1.956
1.042	1.999"))

jr_circle <- ggplot(circleData, aes(x, y)) +
    geom_point(size = 2) +
    coord_equal() +
    theme_cowplot()

ggsave(here('images', 'plots', 'jr_hue.png'),
    jr_hue, width = 5, height = 4)
ggsave(here('images', 'plots', 'jr_hue_ordered.png'),
    jr_hue_ordered, width = 5, height = 4)
ggsave(here('images', 'plots', 'jr_saturation.png'),
    jr_saturation, width = 5, height = 4)
ggsave(here('images', 'plots', 'jr_saturation_ordered.png'),
    jr_saturation_ordered, width = 5, height = 4)
ggsave(here('images', 'plots', 'jr_saturation_ordered_zero.png'),
    jr_saturation_ordered_zero, width = 5, height = 4)
ggsave(here('images', 'plots', 'jr_area.png'),
    jr_area, width = 5, height = 4)
ggsave(here('images', 'plots', 'jr_angle.png'),
    jr_angle, width = 5, height = 4)
ggsave(here('images', 'plots', 'jr_length.png'),
    jr_length, width = 5, height = 4)
ggsave(here('images', 'plots', 'jr_bars.png'),
    jr_bars, width = 5, height = 4)
ggsave(here('images', 'plots', 'jr_position_non_aligned.png'),
    jr_position_non_aligned, width = 5, height = 4)
ggsave(here('images', 'plots', 'jr_position_aligned.png'),
    jr_position_aligned, width = 5, height = 4)
ggsave(here('images', 'plots', 'jr_position_aligned_ordered.png'),
    jr_position_aligned_ordered, width = 5, height = 4)
ggsave(here('images', 'plots', 'jr_position_aligned_ordered_zero.png'),
    jr_position_aligned_ordered_zero, width = 5, height = 4)
ggsave(here('images', 'plots', 'jr_diamonds_bars_stacked.png'),
    jr_diamonds_bars_stacked, width = 6, height = 4)
ggsave(here('images', 'plots', 'jr_diamonds_bars_dodged.png'),
    jr_diamonds_bars_dodged, width = 6, height = 4)
ggsave(here('images', 'plots', 'jr_diamonds_line_total.png'),
    jr_diamonds_line_total, width = 6, height = 4)
ggsave(here('images', 'plots', 'jr_diamonds_line_cut.png'),
    jr_diamonds_line_cut, width = 6, height = 4)
ggsave(here('images', 'plots', 'jr_phones_area.png'),
    jr_phones_area, width = 7, height = 4)
ggsave(here('images', 'plots', 'jr_phones_line.png'),
    jr_phones_line, width = 7, height = 4)
ggsave(here('images', 'plots', 'jr_circle.png'),
    jr_circle, width = 5, height = 5)
