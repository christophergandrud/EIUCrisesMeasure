# ----------------------------------------------------------------------------
# Save cleaned FinStress for public use
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Load packages
library(simpleSetup)
library(rio)
library(dplyr)
library(DataCombine)

pkgs <- c('repmis', 'rio', 'dplyr', 'DataCombine', 'TTR')
library_install(pkgs)

# Set working directory
possibles <- c("/git_repositories/EIUCrisesMeasure/",
               "~/git_repositories/EIUCrisesMeasure/")

set_valid_wd(possibles)

# Load raw data
raw <- import("source/pca_kpca/raw_data_output/5_strings/results_kpca_5_rescaled.csv")

# Reverse the scale so that low values indicate less perceived stress
raw$C1_ma <- 1- raw$C1_ma

sub <- raw %>% select(iso3c, country, date, C1_ma) %>% rename(FinStress = C1_ma)

# Drop NAs created by moving average
sub <- sub %>% DropNA(c("iso3c", "country", "date", "FinStress"))

# Save
export(sub, file = "data/FinStress.csv")


## KPCA Second Component -------------------------------------------------------
# Reverse the scale so that low values indicate less perceived stress
# Function to rescale between 0 and 1
n_period = 2

# Find previous periods moving average
sma_mod <- function(x) TTR::SMA(x, n = n_period)
raw <- raw %>% group_by(iso3c) %>%
            dplyr::mutate(C2_ma = sma_mod(C2))

raw$C2_ma <- 1- raw$C2_ma

sub <- raw %>% select(iso3c, country, date, C2_ma) %>%
    rename(c2_finstress_clean = C2_ma)

# Drop NAs created by moving average
sub <- sub %>% DropNA(c("iso3c", "country", "date", "c2_finstress_clean"))

# Save
export(sub, file = "data/kpca_5_c2_cleaned.csv")
