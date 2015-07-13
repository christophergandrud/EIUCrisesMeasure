# ---------------------------------------------------------------------------- #
# Compare EPFMS in Developed and Developing
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

library(repmis)
library(DataCombine)
library(rio)
library(dplyr)
library(WDI)
library(lubridate)
library(ggplot2)

# Set working directory of kpca project. Change as needed.
pos_directs <- c('~/git_repositories/EIUCrisesMeasure/',
                 '/git_repositories/EIUCrisesMeasure/')

set_valid_wd(pos_directs)


# Import data sets
epfms <- import('http://bit.ly/1LFEnhM')

# World Bank development classification
wdi <- WDI(extra = T, start = 2005, end = 2005) %>% select(iso2c, income) %>%
        filter(income != 'Aggregates') %>% filter(income != 'Not classifed') 

wdi$high_income <- 0
wdi$high_income[wdi$income == 'High income: nonOECD'] <- 1
wdi$high_income[wdi$income == 'High income: OECD'] <- 1

comb <- merge(epfms, wdi, by = 'iso2c')

# Kolmogorov-Smirnov Test
comb$year <- year(comb$date)

# All
sub_high <- comb %>% filter(high_income == 1) %>% select(C1_ma) %>% as.vector
sub_mid_low <- comb %>% filter(high_income == 0) %>% select(C1_ma)

ks.test(sub_high$C1_ma, sub_mid_low$C1_ma, alternative = 'greater')

# 2005
sub_high_2005 <- comb %>% filter(high_income == 1 & year == 2005) %>% 
                select(C1_ma)
sub_mid_low_2005 <- comb %>% filter(high_income == 0 & year == 2005) %>% 
                select(C1_ma)

ks.test(sub_high_2005$C1_ma, sub_mid_low_2005$C1_ma, alternative = 'greater')

# Plot
annual_mean <- comb %>% group_by(high_income, year) %>%
                summarise(mean_stress = mean(C1_ma, na.rm = T))

annual_mean$high_income <- factor(annual_mean$high_income, 
                                  labels = c('Low & Med. Income', 'High Income'))

ggplot(annual_mean, aes(year, mean_stress, colour = high_income, 
                        group = high_income, linetype = high_income)) +
    geom_line() +
    scale_x_continuous(breaks = c(2003, 2005, 2008, 2011)) +
    scale_colour_manual(values = c("#D8B70A", "#972D15")) +
    xlab('') + ylab('Mean Perceived Stress\n') +
    theme_bw() +
    theme(legend.title = element_blank())

ggsave(filename = 'summary_paper/analysis/figures/dev_vs_devoloping.pdf')
