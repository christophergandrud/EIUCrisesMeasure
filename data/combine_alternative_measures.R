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
library(ggplot2)


# Import MIMFS
mifms <- import('https://raw.githubusercontent.com/christophergandrud/EIUCrisesMeasure/master/data/results_kpca.csv')
mifms$iso2c <- countrycode(mifms$country, origin = 'country.name', 
                           destination = 'iso2c')
mifms <- mifms %>% dplyr::select(iso2c, date, C1)


# Import Romer and Romer (2015)
romer_romer <- import('https://raw.githubusercontent.com/christophergandrud/EIUCrisesMeasure/master/data/alternative_measures/rommer_romer.csv')


# Reinhart and Rogoff
rein_rog <- RRCrisisGet()

# Laeven and Valencia


# Combine



#### Compare ####

ggplot(comb, aes(date, rr_distress, color = country)) +
    geom_line() +
    theme_bw()

# Compare to MIFMS

comb <- merge(mifms, comb, by = c('country', 'date'), all = T)


