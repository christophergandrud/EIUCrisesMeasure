# ---------------------------------------------------------------------------- #
# Clean Laeven and Valencia (2013)
# Data downloaded from https://www.imf.org/external/pubs/cat/longres.aspx?sk=26015.0
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Set working directory
setwd('/git_repositories/EIUCrisesMeasure/data/alternative_measures/')

# Load packages
library(rio)
library(dplyr)
library(DataCombine)
library(countrycode)
library(stringr)

# Load data
raw_lv <- import('raw/SYSTEMIC BANKING CRISES DATABASE.xlsx', sheet = 4) %>%
            select(Country, Start, End) %>% .[1:147, ] %>%
            rename(country = Country)

# Re-add in Slovak Republic 1998 start year
raw_lv[118, 2] <- 1998

raw_lv$iso2c <- countrycode(raw_lv$country, origin = 'country.name',
                               destination = 'iso2c')

for (i in names(raw_lv)) {
    raw_lv[, i] <- gsub('../', '', raw_lv[, i])
}

# Create ongoing variable
raw_lv$ongoing <- 0
raw_lv$ongoing[raw_lv$End == 'ongoing'] <- 1

# Replace 'ongoing' with 2012
raw_lv$End[raw_lv$End == 'ongoing'] <- 2011
raw_lv$End <- raw_lv$End %>% as.integer

#### Start-End Only ####
lv_se <- raw_lv %>% select(-ongoing)
lv_se$Start <- sprintf('%s-06-01', lv_se$Start)
lv_se$End <- sprintf('%s-06-01', lv_se$End)

lv_se <- lv_se %>% select(iso2c, Start, End) %>%
            arrange(iso2c, Start)
export(lv_se, 'cleaned/laeven_valencia_start_end.csv')

#### Expand in missing years ####
raw_lv$Start <- raw_lv$Start %>% as.integer

range <- seq(min(raw_lv$Start), max(raw_lv$End), by = 1)

lv_filled <- TimeFill(raw_lv, GroupVar = 'iso2c', StartVar = 'Start',
                      EndVar = 'End', NewTimeVar = 'year',
                      NewVar = 'lv_bank_crisis')

lv_filled <- lv_filled %>% select(iso2c, year, lv_bank_crisis)

export(lv_filled, 'cleaned/laeven_valencia_banking_crisis.csv')
