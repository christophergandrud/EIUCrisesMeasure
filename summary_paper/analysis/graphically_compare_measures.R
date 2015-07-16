#########################################
# Compare crisis measures
# Christopher Gandrud
# MIT License
#########################################

# Load required packages
library(rio)
library(DataCombine)
library(countrycode)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(repmis)

# Set working directory
pos <- c('/git_repositories/EIUCrisesMeasure/',
         '~/git_repositories/EIUCrisesMeasure/')

set_valid_wd(pos)

# Function to rescale between 0 and 1
range01 <- function(x){(x - min(x))/(max(x) - min(x))}

## Import EIU Percpetions Index
perceptions <- import('data/results_kpca_rescaled.csv')
perceptions$iso2c <- countrycode(perceptions$country, origin = 'country.name',
                           destination = 'iso2c')
perceptions <- perceptions %>% dplyr::select(iso2c, date, C1, C2, C3, C1_ma)
perceptions$date <- ymd(perceptions$date)

perceptions$country <- countrycode(perceptions$iso2c, origin = 'iso2c',
                                   destination = 'country.name')
perceptions$Source <- 'EIU Perceptions Index'

## Import Romer and Romer (2015)
romer_romer <- import('data/alternative_measures/cleaned/rommer_romer.csv')
romer_romer$date <- ymd(romer_romer$date)
romer_romer <- romer_romer %>% select(-country)

romer_romer$rr_rescale <- romer_romer$rr_distress %>%
                            range01()

romer_romer$country <- countrycode(romer_romer$iso2c, origin = 'iso2c',
                                    destination = 'country.name')
romer_romer$Source <- 'Romer/Romer'

## Combine Perceptions Index and Romer and Romer
perceptions <- perceptions %>% select(country, date, Source, C1_ma) %>%
                rename(stress_measure = C1_ma)
romer_romer <- romer_romer %>% select(country, date, Source, rr_rescale) %>%
                rename(stress_measure = rr_rescale)

comb_continuous <- rbind(perceptions, romer_romer) %>%
                    filter(date >= '2003-01-01')

## Load Reinhart and Rogoff
source('data/alternative_measures/reinhart_rogoff.R')
rr_bc <- rr_bc %>% filter(RR_BankingCrisis_start >= '2003-01-01')

rr_bc_start <- rr_bc %>% select(iso2c, RR_BankingCrisis_start) %>%
                rename(start = RR_BankingCrisis_start)
rr_bc_end <- rr_bc %>% select(RR_BankingCrisis_end) %>%
                rename(end = RR_BankingCrisis_end)

rr_bc <- cbind(rr_bc_start, rr_bc_end)
rr_bc$Source <- 'Reinhart/Rogoff'

## Laeven and Valencia
lv <- import('data/alternative_measures/cleaned/laeven_valencia_banking_crisis.csv')

# Assume date is 1 June
lv$date <- sprintf('%s-06-01', lv$year) %>% ymd
lv <- lv %>% select(iso2c, date, lv_bank_crisis)

lv_se <- import('data/alternative_measures/cleaned/laeven_valencia_start_end.csv')
lv_se$Start <- ymd(lv_se$Start)
lv_se$End <- ymd(lv_se$End)
lv_se <- lv_se %>% filter(Start >= '2003-01-01')

lv_se_start <- lv_se %>% select(iso2c, Start) %>%
                rename(start = Start)
lv_se_end <- lv_se %>% select(End) %>%
                rename(end = End)

lv_se <- cbind(lv_se_start, lv_se_end)
lv_se$Source <- 'Laeven/Valencia'

comb_se <- rbind(rr_bc, lv_se)

comb_se$country <- countrycode(comb_se$iso2c, origin = 'iso2c',
                                 destination = 'country.name')

#### Compare to LV ####
compare_to_dummy <- function(data_cont, data_dummy, id) {
    temp_cont <- subset(data_cont, country == id)
    temp_dummy <- subset(data_dummy, country == id)

    if (nrow(temp_dummy) == 0) {
        ggplot(data = temp_cont, aes(date, stress_measure, group = Source,
                                     linetype = Source)) +
            geom_line(alpha = 0.6) +
            stat_smooth(data = subset(temp_cont,
                                      Source == "EIU Perceptions Index"),
                                      aes(date, stress_measure),
                        se = F, colour = 'black', linetype = 'dotted') +
            scale_y_continuous(limits = c(0, 1),
                               breaks = c(0, 0.25, 0.5, 0.75, 1)) +
            scale_linetype_manual(values = c('solid', 'dashed')) +
            ggtitle(id) + xlab('') +
            ylab('Rescaled\nCrisis Measures\n') +
            # ylab('Perceptions of \n Financial Market Conditions\n') +
            theme_bw() +
            theme(legend.position = "none")
    } else if (nrow(temp_dummy)) {
        ggplot() +
            geom_line(data = temp_cont, aes(date, stress_measure,
                                            group = Source,
                                            linetype = Source), alpha = 0.6) +
            stat_smooth(data = subset(temp_cont,
                                      Source == "EIU Perceptions Index"),
                        aes(date, stress_measure),
                        se = F, colour = 'black', linetype = 'dotted') +
            geom_rect(data = temp_dummy, aes(xmin = start, xmax = end,
                                                ymin = -Inf, ymax = Inf,
                                                fill = Source),
                      alpha = 0.4) +
            scale_fill_manual(values = c("#D8B70A", "#972D15")) +
            scale_linetype_manual(values = c('solid', 'dashed')) +
            scale_y_continuous(limits = c(0, 1),
                               breaks = c(0, 0.25, 0.5, 0.75, 1)) +
            ggtitle(id) + xlab('') +
            ylab('Rescaled\nCrisis Measures\n') +
            # ylab('Perceptions of \n Financial Market Conditions\n') +
            theme_bw() +
            theme(legend.position = "none")
    }
}

country_vector <- unique(perceptions$country)
kpca_list <- list()
for (i in country_vector) {
    message(i)
    kpca_list[[i]] <- suppressMessages(
            compare_to_dummy(data_cont = comb_continuous,
                             data_dummy = comb_se,
                             id = i))
}

# Plot selection (1)
select_countries_1 <- c('Argentina', 'Australia', 'Austria', 'Belgium',
                      'Brazil','Bulgaria', 'Canada', 'China',
                      'Czech Republic', 'Denmark', 'Estonia', 'France',
                      'Germany', 'Greece','Hungary', 'Iceland',
                      'India', 'Ireland', 'Italy', 'Japan'
                      )
pdf(file = 'summary_paper/figures/compare_to_lv_rr.pdf', width = 15,
    height = 15)
    do.call(grid.arrange, kpca_list[select_countries_1])
dev.off()

# Plot selection (2)
select_countries_2 <- c('Kazakhstan', 'Latvia', 'Lithuania', 'Luxembourg',
                        'Netherlands', 'Nigeria', 'Portugal', 'Russian Federation',
                        'Singapore', 'Slovenia', 'South Africa', 'Spain',
                        'Switzerland', 'Ukraine', 'United Kingdom', 'United States'
                        )
pdf(file = 'summary_paper/figures/compare_to_lv_rr_2.pdf', width = 15,
    height = 15)
    do.call(grid.arrange, kpca_list[select_countries_2])
dev.off()
