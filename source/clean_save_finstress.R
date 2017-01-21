# ----------------------------------------------------------------------------
# # Save cleaned FinStress for public use Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Load packages
library(setupPkg)

pkgs <- c('repmis', 'rio', 'dplyr', 'DataCombine')
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
