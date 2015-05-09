#########################################
# Compare crisis measures
# Christopher Gandrud
# MIT License
#########################################

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
mifms <- import('https://raw.githubusercontent.com/christophergandrud/EIUCrisesMeasure/master/data/results_kpca.csv')
mifms$iso2c <- countrycode(mifms$country, origin = 'country.name', 
                           destination = 'iso2c')
mifms <- mifms %>% dplyr::select(iso2c, date, C1)
mifms$date <- ymd(mifms$date) 

mifms$C1 <- mifms$C1* -1
mifms$mifms <-  range01(mifms$C1)

## Import Romer and Romer (2015)
romer_romer <- import('https://raw.githubusercontent.com/christophergandrud/EIUCrisesMeasure/master/data/alternative_measures/rommer_romer.csv')
romer_romer$date <- ymd(romer_romer$date)
romer_romer <- romer_romer %>% select(-country)

romer_romer$rr_rescale <- range01(romer_romer$rr_distress)

## Reinhart and Rogoff
rein_rog <- RRCrisisGet()
rein_rog_sub <- rein_rog %>% select(iso2c, year, RR_BankingCrisis, 
                                    RR_StockMarketCrash, RR_CurrencyCrisis)
rein_rog_sub$date <- sprintf('%s-06-01', rein_rog_sub$year)
rein_rog_sub$date <- ymd(rein_rog_sub$date)
rein_rog_sub <- rein_rog_sub %>% select(-year)

## Laeven and Valencia


# Combine
comb <- merge(mifms, romer_romer, by = c('iso2c', 'date'), all = T)
comb <- merge(comb, rein_rog_sub, by = c('iso2c', 'date'), all = T)
# Subset to be from 2003
comb <- comb %>% filter(date >= '2003-01-01') %>% arrange(iso2c, date)

comb <- comb %>% group_by(iso2c) %>% 
            mutate(rr_rescale = FillDown(Var = rr_rescale)) %>%
            mutate(RR_BankingCrisis = FillDown(Var = RR_BankingCrisis)) %>%
            mutate(RR_StockMarketCrash = FillDown(Var = RR_StockMarketCrash)) %>%
            mutate(RR_CurrencyCrisis = FillDown(Var = RR_CurrencyCrisis))

# Reshape
comb <- comb[, c('iso2c', 'date', 'mifms', 'rr_rescale', 'RR_BankingCrisis',
                 'RR_StockMarketCrash')]
comb_gather <- gather(comb, measure, value, 3:ncol(comb))
comb_gather$value <- comb_gather$value %>% as.numeric

#### Compare ####
jp <- comb_gather %>% filter(iso2c == 'JP')
ggplot(jp, aes(date, value, colour = measure)) +
        geom_line() +
        theme_bw()

tr <- comb_gather %>% filter(iso2c == 'TR')
ggplot(tr, aes(date, value, colour = measure)) +
    geom_line() +
    theme_bw()

de <- comb_gather %>% filter(iso2c == 'DE')
ggplot(de, aes(date, value, colour = measure)) +
    geom_line() +
    theme_bw()

us <- comb_gather %>% filter(iso2c == 'US')
ggplot(us, aes(date, value, colour = measure)) +
    geom_line() +
    theme_bw()

