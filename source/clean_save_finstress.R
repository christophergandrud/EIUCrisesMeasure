# ---------------------------------------------------------------------------- #
# Save cleaned FinStress for public use
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Load packages
library(repmis)
library(rio)
library(dplyr)
library(DataCombine)

# Set working directory
possibles <- c('/git_repositories/EIUCrisesMeasure/')

set_valid_wd(possibles)

# Load raw data 
raw <- import('data/results_kpca_rescaled.csv')

sub <- raw %>% select(iso2c, country, date, C1_ma) %>%
        rename(FinStress = C1_ma)

# Drop NAs created by moving average
sub <- sub %>% DropNA(c('iso2c', 'country', 'date', 'FinStress'))

# Save
export(sub, file = 'data/FinStress.csv')
