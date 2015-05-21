#########################################
# Compare crisis measures
# Christopher Gandrud
# MIT License
#########################################

# Set working directory
setwd('/git_repositories/EIUCrisesMeasure/')

# Load required packages
library(rio)
library(psData)
library(DataCombine)
library(countrycode)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

# Function to rescale between 0 and 1
range01 <- function(x){(x - min(x))/(max(x) - min(x))}

## Import MIMFS
perceptions <- import('data/results_kpca_rescaled.csv')
perceptions$iso2c <- countrycode(perceptions$country, origin = 'country.name',
                           destination = 'iso2c')
perceptions <- perceptions %>% dplyr::select(iso2c, date, C1, C2, C3, C1_ma)
perceptions$date <- ymd(perceptions$date)

## Import Romer and Romer (2015)
romer_romer <- import('data/alternative_measures/cleaned/rommer_romer.csv')
romer_romer$date <- ymd(romer_romer$date)
romer_romer <- romer_romer %>% select(-country)

romer_romer$rr_rescale <- range01(romer_romer$rr_distress)

## Reinhart and Rogoff
cleaned_data <- list.files('data/alternative_measures/cleaned/')
if (!('reinhart_rogoff.csv' %in% cleaned_data)) {
    rein_rog <- RRCrisisGet()
    rein_rog_sub <- rein_rog %>% select(iso2c, year, RR_BankingCrisis,
                                        RR_StockMarketCrash, RR_CurrencyCrisis)
    rein_rog_sub$date <- sprintf('%s-06-01', rein_rog_sub$year)
    rein_rog_sub$date <- ymd(rein_rog_sub$date)
    rein_rog_sub <- rein_rog_sub %>% select(-year)

    export(rein_rog_sub,
           file = 'data/alternative_measures/cleaned/reinhart_rogoff.csv')
} else if (('reinhart_rogoff.csv' %in% cleaned_data)) {
    rein_rog_sub <- import('data/alternative_measures/cleaned/reinhart_rogoff.csv')
    rein_rog_sub$date <- ymd(rein_rog_sub$date)
}

## Laeven and Valencia
lv <- import('data/alternative_measures/cleaned/laeven_valencia_banking_crisis.csv')

# Assume date is 1 June
lv$date <- sprintf('%s-06-01', lv$year) %>% ymd
lv <- lv %>% select(iso2c, date, lv_bank_crisis)

lv_se <- import('data/alternative_measures/cleaned/laeven_valencia_start_end.csv')
lv_se$Start <- ymd(lv_se$Start)
lv_se$End <- ymd(lv_se$End)
lv_se <- lv_se %>% filter(Start >= '2003-01-01')

# Combine
comb <- merge(perceptions, romer_romer, by = c('iso2c', 'date'), all = T)
comb <- merge(comb, lv, by = c('iso2c', 'date'), all = T)
comb <- merge(comb, rein_rog_sub, by = c('iso2c', 'date'), all = T)
comb$date <- ymd(comb$date)
# Subset to be from 2003 to 2012
comb <- comb %>% filter(date >= '2003-01-01') %>% arrange(iso2c, date)
comb <- comb %>% filter(date < '2012-01-01') %>% arrange(iso2c, date)


comb <- comb %>% group_by(iso2c) %>%
           # mutate(C1 = FillDown(Var = C1)) %>%
            mutate(C1 = FillDown(Var = C2)) %>%
            mutate(rr_rescale = FillDown(Var = rr_rescale)) %>%
            mutate(lv_bank_crisis = FillDown(Var = lv_bank_crisis)) %>%
            mutate(RR_BankingCrisis = FillDown(Var = RR_BankingCrisis)) %>%
            mutate(RR_StockMarketCrash = 
                       FillDown(Var = RR_StockMarketCrash)) %>%
            mutate(RR_CurrencyCrisis = FillDown(Var = RR_CurrencyCrisis))

comb$rr_rescale[comb$date >= '2008-01-01'] <- NA


cor.test(comb$C1, comb$lv_bank_crisis)
cor.test(comb$C2, comb$lv_bank_crisis)

# Reshape
comb_sub <- comb[, c('iso2c', 'date', 'C1', 'lv_bank_crisis', 'rr_rescale',
                 'RR_BankingCrisis')]
comb_gather <- gather(comb_sub, measure, value, 3:ncol(comb_sub))
comb_gather$value <- comb_gather$value %>% as.numeric

#### Compare to LV ####
compare_to_dummy <- function(data_cont, data_dummy, country) {
    temp_cont <- data_cont %>% filter_(iso2c == country)
    temp_dummy <- data_dummy %>% filter_(iso2c == country) 
    
    ggplot() +
        geom_line(data = temp_cont, aes(date, C1_ma)) +
        geom_rect(data = temp_dummy, aes(xmin = Start, xmax = End, 
                                       ymin = -Inf, ymax = Inf), alpha = 0.4) +
        theme_bw()
}








jp <- comb_gather %>% filter(iso2c == 'JP')
ggplot(jp, aes(date, value, colour = measure)) +
        geom_line() +
        theme_bw()

AT <- comb_gather %>% filter(iso2c == 'AT')
ggplot(AT, aes(date, value, colour = measure)) +
    geom_line() +
    theme_bw()

de <- perceptions %>% filter(iso2c == 'DE')
de_lv_se <- lv_se %>% filter(iso2c == 'DE') 
ggplot() +
    geom_line(data = de, aes(date, C1_ma)) +
    geom_rect(data = de_lv_se, aes(xmin = Start, xmax = End, 
                                   ymin = -Inf, ymax = Inf), alpha = 0.4) +
    theme_bw()

us <- perceptions %>% filter(iso2c == 'US')
us_lv_se <- lv_se %>% filter(iso2c == 'US') 

ggplot() +
    geom_line(data = us, aes(date, C1_ma)) +
    geom_rect(data = us_lv_se, aes(xmin = Start, xmax = End, 
                                   ymin = -Inf, ymax = Inf), alpha = 0.4) +
    theme_bw()

uk <- perceptions %>% filter(iso2c == 'GB')
uk_lv_se <- lv_se %>% filter(iso2c == 'GB') 

ggplot() +
    geom_line(data = uk, aes(date, C1_ma)) +
    geom_rect(data = uk_lv_se, aes(xmin = Start, xmax = End, 
                                   ymin = -Inf, ymax = Inf), alpha = 0.4) +
    theme_bw()
