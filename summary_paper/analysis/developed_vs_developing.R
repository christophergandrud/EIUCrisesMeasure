# ---------------------------------------------------------------------------- #
# Compare FinStress in Developed and Developing
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

library(repmis)
library(DataCombine)
library(rio)
library(dplyr)
library(WDI)
library(lubridate)
library(countrycode)
library(ggplot2)

# Set working directory of kpca project. Change as needed.
pos_directs <- c('~/git_repositories/EIUCrisesMeasure/',
                 '/git_repositories/EIUCrisesMeasure/')

set_valid_wd(pos_directs)

# Import data sets
finstress <- import('data/FinStress.csv')

# World Bank development classification
wdi <- WDI(extra = T, start = 2005, end = 2005) %>% select(iso2c, income) %>%
        filter(income != 'Aggregates') %>% filter(income != 'Not classifed')

wdi$high_income <- 0
wdi$high_income[wdi$income == 'High income: nonOECD'] <- 1
wdi$high_income[wdi$income == 'High income: OECD'] <- 1

wdi$iso3c <- countrycode(wdi$iso2c, origin = 'iso2c', destination = 'iso3c')

comb <- merge(finstress, wdi, by = 'iso3c')

# Kolmogorov-Smirnov Test
comb$year <- year(comb$date)

# All
sub_high <- comb %>% filter(high_income == 1) %>% select(FinStress) %>% as.vector
sub_mid_low <- comb %>% filter(high_income == 0) %>% select(FinStress)

ks.test(sub_high$FinStress, sub_mid_low$FinStress, alternative = 'greater')

# 2005
sub_high_2005 <- comb %>% filter(high_income == 1 & year == 2005) %>%
                select(FinStress)
sub_mid_low_2005 <- comb %>% filter(high_income == 0 & year == 2005) %>%
                select(FinStress)

ks.test(sub_high_2005$FinStress, sub_mid_low_2005$FinStress, alternative = 'greater')

# Load Laeven and Valencia Crisis Binary Data
lv <- import('http://bit.ly/1gacC47')

comb <- merge(comb, lv, by = c('iso2c', 'year'), all.x = T)

# Test if LV crises are at different levels
comb_high_crisis <- comb %>% filter(high_income == 1 & lv_bank_crisis == 1)
comb_low_crisis <- comb %>% filter(high_income == 0 & lv_bank_crisis == 1)

ks.test(comb_high_crisis$FinStress, comb_low_crisis$FinStress, alternative = 'less')

# Prep labels for plot
comb$lv_bank_crisis <- factor(comb$lv_bank_crisis, labels = c('No LV Crisis', 
                                                              'LV Crisis'))

comb$high_income <- factor(comb$high_income, labels = 
                               c('Low & Med. Income', 'High Income'))

# Plot
comb <- comb %>% DropNA(c('high_income', 'lv_bank_crisis'))

# Drop outlier Uruguay
comb <- comb %>% filter(country != 'Uruguay')

annual_mean <- comb %>% group_by(high_income, lv_bank_crisis, year) %>%
                summarise(mean_stress = mean(FinStress, na.rm = T))

ggplot(annual_mean, aes(year, mean_stress, colour = high_income,
                        group = high_income, linetype = high_income)) +
    facet_wrap(~lv_bank_crisis) +
    geom_line() +
    scale_x_continuous(breaks = c(2003, 2005, 2008, 2011)) +
    scale_colour_manual(values = c("#D8B70A", "#972D15")) +
    xlab('') + ylab('Mean Perceived Stress\n') +
    theme_bw() +
    theme(legend.title = element_blank())

ggsave(filename = 'summary_paper/figures/dev_vs_developing.pdf', width = 10, 
       height = 5.6)
