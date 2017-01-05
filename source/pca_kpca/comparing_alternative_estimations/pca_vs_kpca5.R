# ---------------------------------------------------------------------------- #
# PCA Bag of Words compared to KPCA 5 (FinStress)
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Load required packages
library(rio)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
theme_set(theme_bw())

# Set working directory. Change as needed.
setwd('/git_repositories/EIUCrisesMeasure/source/')

# Load FinStress
URL <- 'https://raw.githubusercontent.com/christophergandrud/EIUCrisesMeasure/master/data/FinStress.csv'
finstress_index <- rio::import(URL) %>% select(-iso2c)

# Load PCA bag-of-words first component (rescaled)
pca_bag <- import('pca_kpca/raw_data_output/pca_bag_1stComponent.csv')


######## TODO-EXAMINE NONOVERLAPPING  
comb <- merge(finstress_index, pca_bag, by = c('country', 'date'))
comb <- comb[, c('country', 'date', 'FinStress', 'pca_bag_pc1')]

comb$date <- ymd(comb$date)

# Correlation ------
cor.test(comb$FinStress, comb$pca_bag_pc1)

# Visually compare -------
gathered <- gather(comb, measure, value, 3:4)

# Separate into plots matching LV/RR compare plots
select_countries_1 <- c('Argentina', 'Australia', 'Austria', 'Belgium',
                        'Brazil','Bulgaria', 'Canada', 'China',
                        'Czech Republic', 'Denmark', 'Estonia', 'France',
                        'Germany', 'Greece','Hungary', 'Iceland',
                        'India', 'Ireland', 'Italy', 'Japan'
)

gathered_sub1 <- subset(gathered, country %in% select_countries_1)

ggplot(gathered_sub1, aes(date, value, linetype = measure, 
                          group = measure)) +
    facet_wrap(~country) +
    geom_line(alpha = 0.5) +
    scale_linetype(name = '') +
    xlab('')

select_countries_2 <- c('Kazakhstan', 'Latvia', 'Lithuania', 'Luxembourg',
                        'Mexico', 'Mongolia',
                        'Netherlands', 'Nigeria', 'Portugal', 'Russian Federation',
                        'Singapore', 'Slovenia', 'South Africa', 'Spain',
                        'Switzerland', 'Thailand', 'Ukraine', 'United Kingdom', 'United States',
                        'Uruguay'
)

gathered_sub2 <- subset(gathered, country %in% select_countries_2)

ggplot(gathered_sub2, aes(date, value, linetype = measure, 
                          group = measure)) +
    facet_wrap(~country) +
    geom_line(alpha = 0.5) +
    scale_linetype(name = '') +
    xlab('')
