source('setup.R')

# wildlife scatter ----------------------------------------------------

ggplot(wildlife_impacts_2016) +
    geom_point(aes(x = speed, y = height, color = phase_of_flt)) +
    theme_minimal_grid() +
    labs(x = 'Speed (mph)',
         y = 'Height (ft)',
         color = 'Phase of flight',
         title = 'Wildlife impacts in 2016')

# msleep ------------------------------------------------------

ggplot(msleep) + 
    geom_point(aes(x = brainwt, y = bodywt)) +
    theme_half_open() + 
    labs(x = 'Brain weight (kg)',
         y = 'Body weight (kg)')

ggplot(msleep) + 
    geom_point(aes(x = brainwt, y = bodywt)) +
    scale_x_log10() +
    scale_y_log10() +
    theme_half_open() + 
    labs(x = 'Log brain weight (kg)',
         y = 'Log body weight (kg)')

# wildlife lines ------------------------------------------------------

wildlife_summary <- wildlife_impacts %>% 
    filter(incident_year == 2016) %>% 
    count(operator, incident_date) %>% 
    mutate(incident_date = as.Date(incident_date))

ggplot(wildlife_summary) + 
    geom_line(aes(x = incident_date, y = n, 
                  color = operator, group = operator)) + 
    facet_wrap(~operator, ncol = 1) +
    scale_x_date(
        date_breaks = '1 month',
        date_labels = '%b') +
    theme_minimal_grid(font_size = 16) +
    panel_border() +
    theme(legend.position = 'none') +
    labs(x = 'Incident date', 
         y = 'Number of incidents')

# Just Delta    
wildlife_impacts %>% 
    filter(
        incident_year == 2016,
        operator == 'DELTA AIR LINES') %>% 
    count(operator, incident_date) %>% 
    mutate(incident_date = as.Date(incident_date)) %>% 
    ggplot() +
    geom_line(aes(x = incident_date, y = n)) + 
    scale_x_date(
        date_breaks = '1 month',
        date_labels = '%b') +
    theme_minimal_grid(font_size = 16) +
    panel_border() +
    theme(legend.position = 'none') +
    labs(x = 'Incident date', 
         y = 'Number of incidents')

# wildlife cost ------------------------------------------------------

ggplot(wildlife_costs) + 
    geom_point(aes(x = incident_date, y = cost)) + 
    scale_x_date(
        date_breaks = '3 years',
        date_labels = "'%y") + 
    scale_y_continuous(
        labels = scales::dollar,
        expand = expand_scale(mult = c(0, 0.05))) +
    theme_minimal_grid() + 
    labs(x = 'Year', 
         y = 'Cost of incident repairs ($ millions)')

label <- "US Airways Flight 1549
was crash landed into the
Hudson river after striking a 
flock of Canada geese on 
Jan. 15, 2009."

ggplot(wildlife_costs) + 
    geom_point(aes(x = incident_date, y = cost)) + 
    geom_curve(
        mapping = aes(x = ymd('2000-01-01'),
                      xend = ymd('2008-10-01'), 
                      y = 13, yend = 16.4),
        color = 'grey75', size = 0.5, curvature = -0.2,
        arrow = arrow(length = unit(0.01, 'npc'), type = 'closed')) +
    geom_label(
        mapping = aes(x = ymd('1995-01-01'), y = 13,
                      label = label),
        hjust = 0, lineheight = .8) +
    scale_x_date(
        date_breaks = '3 years',
        date_labels = "'%y") + 
    scale_y_continuous(
        labels = scales::dollar,
        expand = expand_scale(mult = c(0, 0.05))) +
    theme_minimal_grid(
        font_family = 'Fira Sans Condensed',
        font_size = 16) + 
    labs(x = 'Year', 
         y = 'Cost of incident repairs ($ millions)', 
         title = 'Repair costs of aircraft impacts with wildlife', 
         subtitle = 'Top 4 airlines', 
         caption = 'Data source: https://wildlife.faa.gov/home')

# gapminder--------------------------------------------------------------------

us_gdp_plot <- ggplot(us_gdp %>% filter(year >= 2007, year <= 2010)) + 
    geom_line(aes(x = year, y = gdp), 
              size = 1) + 
    scale_y_continuous(
        limits = c(0, 16),
        labels = scales::dollar, 
        expand = expand_scale(mult = c(0, 0.05))) +
    theme_minimal_hgrid() + 
    labs(x = NULL, y = 'GDP ($ Trillion)')

us_gdp_plot +
    scale_y_continuous(
        limits = c(14.5, 15.5),
        labels = scales::dollar, 
        expand = expand_scale(mult = c(0, 0.05)))
        
# MPG Box --------------------------------------------------------------

ggplot(mpg, aes(x = displ, y = hwy)) +
    geom_point(aes(fill = as.factor(cyl)), 
               color = 'white', alpha = 0.8,
               size = 3.5, shape = 21) +
    annotate(geom = "rect", 
             xmin = 5, xmax = 7.5,
             ymin = 21, ymax = 28, 
             fill = "grey55", alpha = 0.2) + 
    annotate(geom = "text", 
             x = 5, y = 29,
             label = "Hybrid vehicles", 
             hjust = 0, size = 5) +
    theme_half_open(font_size = 15) +
    labs(x = "Engine displacement", 
         y = "Fuel efficiency (mpg)",
         fill = '# cylinders',
         title = "Vehicle fuel efficiency vs. engine displacement",
         caption = "Data source: U.S. EPA.")

# MPG Color --------------------------------------------------------------

mpg_color <- ggplot(mtcars, aes(x = mpg, y = hp)) +
    geom_point(aes(fill = as.factor(cyl)), 
               color = 'white', alpha = 0.8,
               size = 3.5, shape = 21) +
    theme_half_open(font_size = 15) +
    labs(x = "Fuel efficiency (mpg)", 
         y = "Power (hp)",
         fill = '# cylinders',
         title = "Vehicle fuel efficiency vs. power",
         caption = "Data source: 1974 Motor Trend U.S. magazine.")

mpg_color +
    scale_fill_manual(values = c('#a0522d', '#522da0', '#2da052'))

mpg_color +
    scale_fill_brewer(palette = 'Dark2')

mpg_color +
    scale_fill_viridis(discrete = TRUE)

mpg_color +
    scale_fill_viridis(discrete = TRUE, option = 'inferno')

# MPG Fonts --------------------------------------------------------------

mpg_plot <- ggplot(mtcars) +
    geom_point(aes(x = mpg, y = hp)) +
    theme_minimal_grid(font_family = 'Source Sans Pro', #<<
                       font_size = 15) +
    theme(plot.title = element_text(family = "Roboto Condensed", #<<
                                    size = 20)) +
    labs(x = "Fuel efficiency (mpg)", 
         y = "Power (hp)",
         title = "Vehicle fuel efficiency vs. power",
         caption = "Data source: 1974 Motor Trend U.S. magazine.")


label <- "Higher power engines, 
often come at the expense,
of fuel economy."

mpg_plot +
    geom_label(aes(x = 17, y = 270, label = label),
               lineheight = .8, hjust = 0,
               family = 'Roboto Condensed') #<<

mpg_scale0 <- mpg_plot + 
    scale_x_continuous(limits = c(0, 35)) + 
    scale_y_continuous(limits = c(0, 325))

# LOTR ----------------------------------------------------------------------

lotr_plot <- ggplot(lotr_summary) +
    geom_col(aes(x = Race, y = wordCount, fill = gender),
             width = 0.7, color = 'white') +
    facet_wrap(~gender) +
    scale_y_continuous(
        labels = scales::comma,
        expand = expand_scale(mult = c(0, 0.05))) +
    theme_minimal_hgrid(font_family = 'Roboto Condensed',
                        font_size = 18) +
    theme(legend.position = 'none') +
    panel_border() +
    labs(x = 'Race of character',
         y = 'Number of words spoken',
         title = 'Number of words spoken in LOTR trilogy',
         subtitle = 'by character gender')

lotr_base <- lotr_plot + 
    theme_minimal(base_family = 'Source Sans Pro') + 
    panel_border()

lotr_cowplot <- lotr_plot + 
    theme_minimal_hgrid(font_family = 'Source Sans Pro') + 
    panel_border() 

lotr_cowplot +
    theme(plot.title = element_text(family = 'Roboto Condensed'))

label <- "Across 3 films, 
only 16 words were 
spoken by a 
female hobbit."

ggplot(lotr_summary) +
    geom_col(aes(x = Race, y = wordCount, fill = gender),
             width = 0.7, color = 'white') +
    facet_wrap(~gender) +
    geom_curve(
        data = data.frame(x = 1.2, y = 1200,
                          xend = 1, yend = 200,
                          gender = 'Female'),
        mapping = aes(x = x, y = y, xend = xend, yend = yend),
        color = 'grey75', size = 0.5, curvature = 0.1,
        arrow = arrow(length = unit(0.01, 'npc'), type = 'closed')) +
    geom_label(
        data = data.frame(x = 0.6, y = 2100,
                          label = label, gender = 'Female'),
        mapping = aes(x = x, y = y, label = label),
        hjust = 0, lineheight = .8) +
    scale_y_continuous(
        labels = scales::comma,
        expand = expand_scale(mult = c(0, 0.05))) +
    theme_minimal_hgrid(font_size = 18) +
    theme(legend.position = 'none') +
    panel_border() +
    labs(x = 'Race of character',
         y = 'Number of words spoken',
         title = 'Number of words spoken in LOTR trilogy',
         subtitle = 'by character gender')


# Federal spending -------------------------------------------------------------

ggplot(federal_spending_2017) + 
    geom_col(aes(x = department, y = rd_budget)) + 
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
    coord_flip() + 
    theme_minimal_vgrid() + 
    labs(x = 'Department', 
         y = 'R&D spending ($ Billions)')

ggplot(federal_spending_summary) +
    geom_area(aes(x = year, y = rd_budget, 
                  fill = department)) +
    scale_y_continuous(
        expand = expand_scale(mult = c(0, 0.05))) +
    scale_fill_manual(values = c('grey', 'sienna')) +
    theme_minimal_hgrid() + 
    labs(x = NULL,
         y = 'R&D Budget ($ Billions)', 
         fill = 'Department',
         title = 'Federal R&D spending, 1976 - 2017',
         subtitle = 'Data Source: AAAS Historical Trends')

dod_spending <- ggplot(federal_spending_summary) +
    geom_area(aes(x = year, y = rd_budget, 
                  fill = department)) +
    scale_y_continuous(
        expand = expand_scale(mult = c(0, 0.05)),
        labels = scales::dollar) +
    scale_fill_manual(values = c('grey', 'sienna')) +
    annotate('text', x = 1995, y = 25, label = 'DOD', 
             size = 6, color = 'white',
             family = 'Roboto Condensed') +
    annotate('text', x = 1995, y = 85, label = 'Other', 
             size = 6,
             family = 'Roboto Condensed') +
    theme_minimal_hgrid(font_family = 'Roboto Condensed') + 
    theme(legend.position = 'none') +    
    labs(x = NULL,
         y = 'R&D Budget ($ Billions)', 
         title = 'Federal R&D spending, 1976 - 2017',
         subtitle = 'Data Source: AAAS Historical Trends')

label <- "The Dept. of Defense R&D Budget 
has made up nearly half of all
federal R&D spending"

dod_spending +
    geom_curve(
        aes(x = 1981, xend = 1987, 
            y = 160, yend = 25),
        size = 0.5,
        curvature = 0.1,
        arrow = arrow(length = unit(0.01, "npc"), type = "closed")
    ) +
    geom_label(
        aes(x = 1977, y = 160, label = label),
        hjust = 0, 
        lineheight = .8, 
        family = 'Roboto Condensed'
    )

# Milk line label --------------------------------------------------------

ggplot(milk_region,
    aes(x = year, y = milk_produced,
        color = region)) +
    geom_line(size = 1) +
    geom_text(aes(label = label),
              hjust = 0, nudge_x = 1,
              size = 6) +
    scale_color_manual(values = c(
        'sienna', 'forestgreen', 'dodgerblue', 'orange')) +
    coord_cartesian(clip = 'off') +
    theme_half_open(font_size = 18) +
    theme(legend.position = 'none',
          plot.margin = margin(0.1, 2.7, 0.1, 0.1, "cm")) +
    labs(x = 'Year',
         y = 'Milk produced (billion lbs)',
         title = 'Milk production in four US regions')

# Milk dumbbell label ---------------------------------------------------

milk_top10states <- milk_production %>%
    filter(year == 2017) %>%
    arrange(desc(milk_produced)) %>%
    slice(1:10) %>% 
    mutate(
        milk_produced = milk_produced / 10^9,
        state = fct_reorder(state, milk_produced))

ggplot(milk_top10states) + 
    geom_col(aes(x = state, y = milk_produced)) + 
    coord_flip() +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.05)), 
                       breaks = seq(0, 40, 5)) +
    theme_minimal_vgrid()

milk_summary_dumbbell <- milk_production %>%
    filter(
        year %in% c(1970, 2017),
        state %in% milk_top10states$state) %>%
    mutate(
        # Reorder state variables to follow top 10 states
        state = fct_relevel(state, levels(milk_top10states$state)),
        # Convert year to discrete variable
        year = as.factor(year),
        # Modify the units
        milk_produced = milk_produced / 10^9)

ggplot(milk_summary_dumbbell,
       aes(x = milk_produced, y = state)) +
    geom_line(aes(group = state),
              color = 'lightblue', size = 1) +
    geom_point(aes(color = year), size = 2.5) +
    scale_x_continuous(limits = c(0, 40)) +
    scale_color_manual(values = c('lightblue', 'steelblue')) +
    theme_minimal_vgrid(font_size = 16) +
    theme(axis.line.y = element_blank(),
          axis.ticks = element_blank()) +
    labs(x = 'Milk produced (billion lbs)',
         y = 'State',
         color = 'Year',
         title = 'Top 10 milk producing states')

ggplot(milk_summary_dumbbell,
       aes(x = milk_produced, y = state)) +
    geom_line(aes(group = state),
              color = 'lightblue', size = 1) +
    geom_point(aes(color = year), size = 2.5) +
    geom_text(aes(x = 9, y = 10.5, label = '1970'), 
             color = 'lightblue', size = 5,
             family = 'Roboto Condensed') +
    geom_text(aes(x = 40, y = 10.5, label = '2017'),
             color = 'steelblue', size = 5, 
             family = 'Roboto Condensed') +
    scale_x_continuous(limits = c(0, 40)) +
    scale_color_manual(values = c('lightblue', 'steelblue')) +
    coord_cartesian(clip = 'off', expand = FALSE) +
    theme_minimal_vgrid(font_family = 'Roboto Condensed') +
    theme(axis.line.y = element_blank(),
          axis.ticks = element_blank(),
          legend.position = 'none',
          plot.margin = margin(0.1, 1, 0.1, 0.1, "cm")) +
    labs(x = 'Milk produced (billion lbs)',
         y = 'State',
         color = 'Year',
         title = 'Top 10 milk producing states')

# Aspect ratio ---------------------------------------------------
# From John Rauser's talk: 
# https://www.youtube.com/watch?v=fSgEeI2Xpdc

df <- data.frame(
    ct = c(1e6-10000, 1e6, 1e6+15000),
    dt = as.Date(c("2016-06-01","2016-06-02", "2016-06-03")))

aspect45 <- ggplot(df, aes(dt, ct)) + 
    geom_line(group = 1) + 
    geom_point() + 
    scale_x_date(date_labels = "%b-%d") + 
    theme_minimal_grid() +
    labs(y = NULL)

aspect45 + 
    expand_limits(y = c(800000, 1200000))

aspect45 + 
    expand_limits(x = as.Date(c("2016-05-01", "2016-07-01")))
