# ---------------------------------------------------------------------------- #
# KPCA Alternative Specification: Only include pre global financial crisis
# observations
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Set working directory. Change as needed.
possible_dir <- c('/git_repositories/EIUCrisesMeasure/',
                  '~/git_repositories/EIUCrisesMeasure/')
repmis::set_valid_wd(possible_dir)

# Run set up script
source('source/pca_kpca/setup/setup.R')

# Subset corpus to be prior to 2008
country_date$date <- ymd(country_date$date)
country_date <- subset(country_date, date < '2008-01-01')
eiu_list <- eiu_list[1:nrow(country_date)]

# Run KPCA
system.time(
    kpca_eiu(eiu_list, country_date,
             out_dir = 'source/pca_kpca/raw_data_output/pre_crisis')
)
