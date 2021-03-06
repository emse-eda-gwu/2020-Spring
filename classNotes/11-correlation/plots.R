source('setup.R')
cor_w <- 5
cor_h <- 4

# Galton scatterplot ----------------------------------------------------------

# midparentHeight =  (father + 1.08*mother)/2
galtonCorr <- round(cor(
    GaltonFamilies$childHeight, GaltonFamilies$midparentHeight, 
    method = 'pearson'), 2)
galtonScatterplot <- ggplot(GaltonFamilies) +
    geom_point(aes(x = midparentHeight, y = childHeight),
               size = 0.5, alpha = 0.7) +
    annotate(geom = 'text', x = 64, y = 79, 
             label = str_c('r = ', galtonCorr), hjust = 0,
             size = 5) +
    theme_half_open() +
    labs(x = 'Midparent height (inches)',
         y = 'Child height (inches)')

ggsave(file.path('images', 'plots', 'galtonScatterplot.png'),
       galtonScatterplot, width = 5, height = 4)

# Fitting a line

# Method 1
galtonScatterplotSmooth <- galtonScatterplot + 
    geom_smooth(aes(x = midparentHeight, y = childHeight), 
                method = 'lm', se = FALSE)


ggsave(file.path('images', 'plots', 'galtonScatterplotSmooth.png'),
       galtonScatterplotSmooth, width = 5, height = 4)

# Method 2
galtonFit <- lm(childHeight ~ midparentHeight, data = GaltonFamilies)
summary(galtonFit)
galtonScatterplotAbline <- galtonScatterplot + 
    geom_abline(intercept = galtonFit$coefficients[1],
                slope = galtonFit$coefficients[2], 
                color = 'blue', size = 1)

ggsave(file.path('images', 'plots', 'galtonScatterplotAbline.png'),
       galtonScatterplotAbline, width = 5, height = 4)

# Add equation label, v1
galtonFit <- lm(childHeight ~ midparentHeight, data = GaltonFamilies)
coefs <- round(coef(galtonFit), 2)
eqLabel <- str_c('y = ', coefs[1], ' + ', coefs[2], 'x')
galtonScatterplotEq <- galtonScatterplotSmooth +
    annotate(geom = 'text', x = 64, y = 77, 
             label = eqLabel, hjust = 0,
             size = 5)

ggsave(file.path('images', 'plots', 'galtonScatterplotEq.png'),
       galtonScatterplotEq, width = 5, height = 4)

# Add equation label, v2
formula <- GaltonFamilies$childHeight ~ GaltonFamilies$midparentHeight
galtonScatterplotEq2 <- ggplot(GaltonFamilies, 
    aes(x = midparentHeight, y = childHeight)) +
    geom_point(size = 0.5, alpha = 0.7) +
    geom_smooth(method = 'lm', se = FALSE) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = str_c(..eq.label.., '\n', ..rr.label.., sep = "~~~")), 
                 parse = TRUE) +
    theme_half_open() +
    labs(x = 'Midparent height (inches)',
         y = 'Child height (inches)')

# correlations ----------------------------------------------------------------

cor_df <- data.frame(x = runif(100, 0, 10)) %>%
    mutate(quad       = -(x-5)^2 + rnorm(100, 5, 2),
           vstrong_p  = 2*x + rnorm(100, 0, 2),
           strong_p   = 2*x + rnorm(100, 0, 5),
           moderate_p = 2*x + rnorm(100, 0, 15),
           weak_p     = 2*x + rnorm(100, 0, 35),
           vstrong_n  = -1*vstrong_p,
           strong_n   = -1*strong_p,
           moderate_n = -1*moderate_p,
           weak_n     = -1*weak_p,
           cor_vstrong_p  = round(cor(x, vstrong_p),  2),
           cor_strong_p   = round(cor(x, strong_p),   2),
           cor_moderate_p = round(cor(x, moderate_p), 2),
           cor_weak_p     = round(cor(x, weak_p),     2),
           cor_vstrong_n  = round(cor(x, vstrong_n),  2),
           cor_strong_n   = round(cor(x, strong_n),   2),
           cor_moderate_n = round(cor(x, moderate_n), 2),
           cor_weak_n     = round(cor(x, weak_n),     2))

cor_quad <- ggplot(cor_df) +
    geom_point(aes(x = x, y = quad), size = 1.5, alpha = 0.7) +
    theme_half_open() +
    labs(x = 'x', y = 'y')

cor_vstrong_p <- ggplot(cor_df) +
    geom_point(aes(x = x, y = vstrong_p), size = 1.5, alpha = 0.7) +
    theme_half_open() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(x = 'x', y = 'y',
         title = paste0('r = ', cor_df$cor_vstrong_p[1]))

cor_strong_p <- ggplot(cor_df) +
    geom_point(aes(x = x, y = strong_p), size = 1.5, alpha = 0.7) +
    theme_half_open() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(x = 'x', y = 'y',
         title = paste0('r = ', cor_df$cor_strong_p[1]))

cor_moderate_p <- ggplot(cor_df) +
    geom_point(aes(x = x, y = moderate_p), size = 1.5, alpha = 0.7) +
    theme_half_open() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(x = 'x', y = 'y',
         title = paste0('r = ', cor_df$cor_moderate_p[1]))

cor_weak_p <- ggplot(cor_df) +
    geom_point(aes(x = x, y = weak_p), size = 1.5, alpha = 0.7) +
    theme_half_open() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(x = 'x', y = 'y',
         title = paste0('r = ', cor_df$cor_weak_p[1]))

cor_vstrong_n <- ggplot(cor_df) +
    geom_point(aes(x = x, y = vstrong_n), size = 1.5, alpha = 0.7) +
    theme_half_open() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(x = 'x', y = 'y',
         title = paste0('r = ', cor_df$cor_vstrong_n[1]))

cor_strong_n <- ggplot(cor_df) +
    geom_point(aes(x = x, y = strong_n), size = 1.5, alpha = 0.7) +
    theme_half_open() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(x = 'x', y = 'y',
         title = paste0('r = ', cor_df$cor_strong_n[1]))

cor_moderate_n <- ggplot(cor_df) +
    geom_point(aes(x = x, y = moderate_n), size = 1.5, alpha = 0.7) +
    theme_half_open() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(x = 'x', y = 'y',
         title = paste0('r = ', cor_df$cor_moderate_n[1]))

cor_weak_n <- ggplot(cor_df) +
    geom_point(aes(x = x, y = weak_n), size = 1.5, alpha = 0.7) +
    theme_half_open() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(x = 'x', y = 'y',
         title = paste0('r = ', cor_df$cor_weak_n[1]))

# Combined plots 
cor_p <- plot_grid(cor_weak_p, cor_moderate_p, 
                   cor_strong_p, cor_vstrong_p, ncol = 2)
cor_n <- plot_grid(cor_weak_n, cor_moderate_n, 
                   cor_strong_n, cor_vstrong_n, ncol = 2)

ggsave(file.path('images', 'plots', 'cor_quad.png'),
       cor_quad, width = cor_w, height = cor_h)
ggsave(file.path('images', 'plots', 'cor_vstrong_p.png'),
       cor_vstrong_p, width = cor_w, height = cor_h)
ggsave(file.path('images', 'plots', 'cor_strong_p.png'),
       cor_strong_p, width = cor_w, height = cor_h)
ggsave(file.path('images', 'plots', 'cor_moderate_p.png'),
       cor_moderate_p, width = cor_w, height = cor_h)
ggsave(file.path('images', 'plots', 'cor_weak_p.png'),
       cor_weak_p, width = cor_w, height = cor_h)
ggsave(file.path('images', 'plots', 'cor_vstrong_n.png'),
       cor_vstrong_n, width = cor_w, height = cor_h)
ggsave(file.path('images', 'plots', 'cor_strong_n.png'),
       cor_strong_n, width = cor_w, height = cor_h)
ggsave(file.path('images', 'plots', 'cor_moderate_n.png'),
       cor_moderate_n, width = cor_w, height = cor_h)
ggsave(file.path('images', 'plots', 'cor_weak_n.png'),
       cor_weak_n, width = cor_w, height = cor_h)
ggsave(file.path('images', 'plots', 'cor_n.png'),
       cor_n, width = cor_w*2, height = cor_h*2)
ggsave(file.path('images', 'plots', 'cor_p.png'),
       cor_p, width = cor_w*2, height = cor_h*2)

# outliers ----------------------------------------------------------------

# Make the plot data frames
outliers <- data.frame(x = rnorm(20, 0, 0.5)) %>%
    mutate(y1 = x - rnorm(20, 0, 0.5), 
           y2 = y1, y3 = y1, y4 = y1, y5 = y1, 
           y6 = y1, y7 = y1, y8 = y1, y9 = y1, 
           point = 'base')
points <- expand.grid(data.frame(
    x = c(-5, 0, 5),
    y = c(-5, 0, 5)))
points_df <- as.data.frame(diag(points$y))
names(points_df) <- c('y7', 'y8', 'y9', 'y4', 'y5', 'y6', 'y1', 'y2', 'y3')
for (i in 1:nrow(points_df)) {
    cols <- seq(ncol(points_df))
    cols <- cols[-which(cols == i)]
    points_df[i,cols] <- NA
}
points_df <- bind_cols(data.frame(x = points$x), points_df)
points_df$point <- 'outlier'
outliers <- bind_rows(outliers, points_df) %>% 
    gather(key = 'case', value = 'y', y1:y9) %>% 
    filter(!is.na(y)) %>% 
    group_by(case) %>% 
    mutate(pearson  = round(cor(x, y, method = 'pearson'), 2),
           spearman = round(cor(x, y, method = 'spearman'), 2), 
           case_p   = paste0('r = ', pearson),
           case_s   = paste0('r = ', spearman))

# Create facet labels
case_p <- outliers %>% distinct(case, case_p)
case_p_labels <- case_p$case_p
names(case_p_labels) <- case_p$case
case_s <- outliers %>% distinct(case, case_s)
case_s_labels <- case_s$case_s
names(case_s_labels) <- case_s$case

# Make the plots
outlier_plot <- function(df, labels) {
    plot <- ggplot(df) +
        geom_point(aes(x = x, y = y, color = point),
                   size = 3, alpha = 0.7) +
        facet_wrap(~case, ncol = 3,
                   labeller = labeller(case = labels)) +
        scale_color_manual(values = c('black', 'red')) + 
        scale_x_continuous(limits = c(-6, 6)) + 
        scale_y_continuous(limits = c(-6, 6)) + 
        theme_bw(base_family = 'Roboto Condensed', base_size = 12) +
        panel_border() +
        theme(plot.title = element_text(hjust = 0.5),
              legend.position = 'none',
              strip.text.x = element_text(size = 20, face = 'bold'),
              panel.grid.minor = element_blank()) +
        labs(x = 'x', y = 'y')
    return(plot)
}

pearson_grid  <- outlier_plot(outliers, case_p_labels)
spearman_grid <- outlier_plot(outliers, case_s_labels)
pearson_base  <- outlier_plot(outliers %>% filter(case == 'y5'), case_p_labels)
pearson_1     <- outlier_plot(outliers %>% filter(case == 'y2'), case_p_labels)
pearson_2     <- outlier_plot(outliers %>% filter(case == 'y1'), case_p_labels)

ggsave(file.path('images', 'plots', 'pearson_grid.png'),
       pearson_grid, width = 8, height = 7)
ggsave(file.path('images', 'plots', 'spearman_grid.png'),
       spearman_grid, width = 8, height = 7)
ggsave(file.path('images', 'plots', 'pearson_base.png'),
       pearson_base, width = cor_w, height = cor_h)
ggsave(file.path('images', 'plots', 'pearson1.png'),
       pearson_1, width = cor_w, height = cor_h)
ggsave(file.path('images', 'plots', 'pearson2.png'),
       pearson_2, width = cor_w, height = cor_h)

# Plot summary of comparison 
outlier_compare <- outliers %>% 
    distinct(case, pearson, spearman) %>% 
    gather(key = 'type', value = 'r', pearson:spearman) %>% 
    mutate(type = str_to_title(type)) %>% 
    ggplot(aes(x = '', y = r)) + 
    facet_wrap(~type) +
    geom_boxplot(width = 0.2, color = 'grey42', outlier.shape = NA) +
    geom_jitter(size = 1.7, alpha = 0.7, width = 0.1) + 
    geom_hline(yintercept = 0, color = 'grey42') +
    scale_y_continuous(limits = c(-1, 1), 
                       breaks = c(-1, -0.5, 0, 0.5, 1)) +
    theme_bw(base_size = 12) + 
    labs(x = NULL)

ggsave(file.path('images', 'plots', 'outlier_compare.png'),
       outlier_compare, width = 4, height = 4)

# mtcars corr -----------------------------------------------------------------

mtcarsCorr <- round(cor(mtcars$mpg, 
                        mtcars$hp, method = 'pearson'), 2)
mtcarsScatterplot <- ggplot(mtcars) +
    geom_point(aes(x = mpg, y = hp),
               size = 2, alpha = 0.7) +
    annotate(geom = 'text', x = 28, y = 310, 
             label = str_c('r = ', mtcarsCorr), hjust = 0,
             size = 7) +
    theme_half_open() +
    labs(x = 'Fuel economy (mpg)',
         y = 'Engine power (hp)')

ggsave(file.path('images', 'plots', 'mtcarsScatterplot.png'),
       mtcarsScatterplot, width = 5, height = 4)

# wildlife_impacts ------------------------------------------------------------

wildlife_impacts %>% 
    ggcorr(label = TRUE)

corLab <- round(cor(
    wildlife_impacts$height, wildlife_impacts$speed, 
    use = "complete.obs"), 2)

wildlife_cor <- wildlife_impacts %>% 
    filter(!is.na(speed), !is.na(height)) %>% 
    summarise(pearson = cor(speed, height, method = 'pearson'),
              spearman = cor(speed, height, method = 'spearman'))

ggplot(wildlife_impacts) + 
    geom_point(aes(x = speed, y = height), alpha = 0.7) + 
    annotate(geom = 'text', x = 50, y = 22000, 
             label = str_c('r = ', corLab), 
             hjust = 0, size = 5) +
    theme_minimal_grid() + 
    labs(x = 'Speed (mph)', 
         y = 'Height (ft)')

# msleep ------------------------------------------------------------

# Linear
corLabLinear <- round(cor(
    msleep$bodywt, msleep$brainwt, 
    use = "complete.obs"), 2)

modelLinear <- lm(brainwt ~ bodywt, data = msleep)
coefs <- round(coef(modelLinear), 3)
eqLabel <- str_c('y = ', coefs[1], ' + ', coefs[2], 'x')

ggplot(msleep, aes(x = bodywt, y = brainwt)) + 
    geom_point() + 
    annotate(geom = 'text', x = 100, y = 6, 
             label = str_c('r = ', corLabLinear), 
             hjust = 0, size = 5) + 
    annotate(geom = 'text', x = 100, y = 5.5, 
             label = eqLabel, hjust = 0,
             size = 5) +
    geom_smooth(method = 'lm', se = FALSE) + 
    theme_minimal_grid()

# Log
corLabLog <- round(cor(
    log(msleep$bodywt), log(msleep$brainwt), 
    use = "complete.obs"), 2)

modelLog <- lm(log(brainwt) ~ log(bodywt), data = msleep)
coefs <- round(coef(modelLog), 3)
eqLabel <- str_c('log(y) = ', coefs[1], ' + ', coefs[2], ' log(x)')

ggplot(msleep, aes(x = bodywt, y = brainwt)) + 
    geom_point() + 
    annotate(geom = 'text', x = 0.01, y = 6, 
             label = str_c('r = ', corLabLog), 
             hjust = 0, size = 5) +
    annotate(geom = 'text', x = 0.01, y = 3, 
             label = eqLabel, hjust = 0,
             size = 5) +
    geom_smooth(method = 'lm', se = FALSE) +
    scale_x_log10() +
    scale_y_log10() + 
    theme_minimal_grid()
    
# coronavirus ------------------------------------------------------------

url_confirmed <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv'
confirmed_cases <- read_csv(url_confirmed) %>% 
    gather(key = 'date', value = 'n', -("Province/State":"Long")) %>% 
    janitor::clean_names() %>%
    mutate(date = mdy(date)) %>%
    filter(country_region == 'US') %>% 
    group_by(date, country_region) %>% 
    summarise(n = sum(n)) %>% 
    select(date, n) %>% 
    filter(!is.na(n)) 

ggplot(confirmed_cases) + 
    geom_point(aes(x = date, y = n)) + 
    scale_y_log10()

wildlife_impacts %>% 
    ggcorr(label = TRUE)

corLab <- round(cor(
    wildlife_impacts$height, wildlife_impacts$speed, 
    use = "complete.obs"), 2)

wildlife_cor <- wildlife_impacts %>% 
    filter(!is.na(speed), !is.na(height)) %>% 
    summarise(pearson = cor(speed, height, method = 'pearson'),
              spearman = cor(speed, height, method = 'spearman'))

ggplot(wildlife_impacts) + 
    geom_point(aes(x = speed, y = height), alpha = 0.7) + 
    annotate(geom = 'text', x = 50, y = 22000, 
             label = str_c('r = ', corLab), 
             hjust = 0, size = 5) +
    theme_minimal_grid() + 
    labs(x = 'Speed (mph)', 
         y = 'Height (ft)')

# ggcorr ------------------------------------------------------------------

ggcor_mtcars <- mtcars %>% 
    ggcorr()

ggcor_mtcars_labels <- mtcars %>% 
    ggcorr(label = TRUE, 
           label_size = 3, 
           label_round = 2)

ggcor_mtcars_pearson <- mtcars %>% 
    ggcorr(label = TRUE, 
           label_size = 3, 
           label_round = 2,
           method = c("pairwise", "pearson"))

ggcor_mtcars_spearman <- mtcars %>% 
    ggcorr(label = TRUE, 
           label_size = 3, 
           label_round = 2,
           method = c("pairwise", "spearman"))

ggcor_mtcars_final <- mtcars %>% 
    ggcorr(label = TRUE, 
           label_size = 3, 
           label_round = 2,
           label_color = 'white',
           method = c("pairwise", "spearman"),
           nbreaks = 5, 
           palette = "RdBu")

ggcor_mtcars_colors <- mtcars %>% 
    ggcorr(geom = "blank", label = TRUE) +
    geom_point(size = 10, aes(color = coefficient > 0, 
                              alpha = abs(coefficient) > 0.7)) +
    scale_alpha_manual(values = c("TRUE" = 0.50, "FALSE" = 0)) +
    guides(color = FALSE, alpha = FALSE)

ggsave(file.path('images', 'plots', 'ggcor_mtcars.png'),
       ggcor_mtcars, width = 6, height = 5)

ggsave(file.path('images', 'plots', 'ggcor_mtcars_labels.png'),
       ggcor_mtcars_labels, width = 6, height = 5)

ggsave(file.path('images', 'plots', 'ggcor_mtcars_pearson.png'),
       ggcor_mtcars_pearson, width = 6, height = 5)

ggsave(file.path('images', 'plots', 'ggcor_mtcars_spearman.png'),
       ggcor_mtcars_spearman, width = 6, height = 5)

ggsave(file.path('images', 'plots', 'ggcor_mtcars_final.png'),
       ggcor_mtcars_final, width = 6, height = 5)

ggsave(file.path('images', 'plots', 'ggcor_mtcars_colors.png'),
       ggcor_mtcars_colors, width = 6, height = 5)

# ggpairs ------------------------------------------------------------------

# Correlogram

ggpairs_mtcars <- mtcars %>% 
    select(mpg, cyl, disp, hp, wt) %>% 
    ggpairs()

ggpairs_mtcars_half_open <- mtcars %>% 
    select(mpg, cyl, disp, hp, wt) %>% 
    ggpairs() + 
    theme_half_open()

ggsave(file.path('images', 'plots', 'ggpairs_mtcars.png'),
       ggpairs_mtcars, width = 7, height = 6)

ggsave(file.path('images', 'plots', 'ggpairs_mtcars_half_open.png'),
       ggpairs_mtcars_half_open, width = 7, height = 6)

