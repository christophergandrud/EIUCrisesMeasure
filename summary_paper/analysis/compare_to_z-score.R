# ---------------------------------------------------------------------------- #
# Compare EPFMS to Z-Score
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #


# Load packages
library(repmis)
library(rio)
library(WDI)
library(lubridate)
library(countrycode)
library(DataCombine)
library(dplyr)
library(ggplot2)

# Set working directory of kpca project. Change as needed.
pos_directs <- c('~/git_repositories/EIUCrisesMeasure/',
                 '/git_repositories/EIUCrisesMeasure/')

set_valid_wd(pos_directs)

# Rescale function
range01 <- function(x, na.rm = T){
    (x - min(x, na.rm = na.rm)) / 
        (max(x, na.rm = na.rm) - min(x, na.rm = na.rm))
}

# Import data sets
epfms <- import('https://raw.githubusercontent.com/christophergandrud/EIUCrisesMeasure/master/data/results_kpca_rescaled.csv')

# Other
wdi <- WDI(indicator = c('GFDD.SI.01'), start = 2003) %>% 
    rename(z_score = GFDD.SI.01) %>%
    select(iso2c, year, z_score)

# Create log z-score
wdi$ihs_score <- wdi$z_score %>% ihs

# Convert EPFMS to annual so that it is comparable with the FRT
epfms$year <- epfms$date %>% year
epfms_sum <- epfms %>% group_by(iso2c, year) %>% 
    summarise(mean_stress = mean(C1_ma, na.rm = T)) 

# Merge data series together
comb <- merge(epfms_sum, wdi, by = c('iso2c', 'year'), all.x = T)
comb$country <- countrycode(comb$iso2c, origin = 'iso2c', 
                            destination = 'country.name')
comb <- comb %>% MoveFront('country')

#### Plot Explorations
cor.test(comb$mean_stress, comb$z_score)

ggplot(comb, aes(mean_stress, z_score)) + 
    geom_point() + 
    stat_smooth() + 
    xlab('\nEPFMS Annual Mean') + ylab('Z-Score\n') + 
    theme_bw()

## Per country exploration
comb_sub <- comb %>% select(country, year, mean_stress, z_score)

comb_sub$z_score <- comb_sub$z_score %>% range01
comb_sub$z_score <- 1 - comb_sub$z_score

cor.test(comb_sub$mean_stress, comb_sub$z_score)

comb_sub <- comb_sub %>% tidyr::gather(indicator, value, 3:4)
comb_sub$indicator <- comb_sub$indicator %>% factor(
                        levels = c('mean_stress', 'z_score'),
                        labels = c('EPFMS\n(annual mean)', 'Z-Score\n(rescaled/inverted)\n'))

select_countries_1 <- c('Argentina', 'Australia', 'Austria', 'Belgium',
                        'Brazil','Bulgaria', 'Canada', 'China', 'Croatia',
                        'Czech Republic', 'Denmark', 'Estonia', 'France',
                        'Germany', 'Greece', 'Hungary', 'Iceland',
                        'India', 'Ireland', 'Italy', 'Japan', 
                        'Latvia', 'Lithuania', 'Luxembourg',
                        'Netherlands', 'Nigeria', 'Portugal', 'Russian Federation',
                        'Singapore', 'Slovenia', 'South Africa', 'Spain',
                        'Switzerland', 'Ukraine', 'United Kingdom', 'United States'
)

sub <- comb_sub %>% filter(country %in% select_countries_1)

ggplot(sub, aes(year, value, colour = indicator, group = indicator,
                linetype = indicator)) + 
    facet_wrap(~country) +
    geom_line() + 
    scale_linetype_manual(values = c('solid', 'dashed')) +
    scale_colour_manual(values = c("#D8B70A", "#972D15")) +
    scale_x_continuous(breaks = c(2003, 2010)) +
    xlab('') + ylab('Rescaled Stress Indicators\n') +
    theme_bw() +
    theme(legend.title = element_blank())

ggsave('summary_paper/analysis/figures/compare_to_z-score.pdf', 
       width = 17, height = 15)

#### Test prediction of EPFMS based on Z-Scores ####
comb <- slide(comb, Var = 'mean_stress', TimeVar = 'year', GroupVar = 'iso2c',
              NewVar = 'stress_lag1')
comb <- slide(comb, Var = 'z_score', TimeVar = 'year', GroupVar = 'country',
              NewVar = 'z_lag1', slideBy = -1)

m1 <- lm(mean_stress ~ stress_lag1 + z_lag1 + as.factor(iso2c), data = comb)
