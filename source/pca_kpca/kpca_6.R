# ---------------------------------------------------------------------------- #
# KPCA Alternative Specification: 6 string kernels
# observations
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Set working directory. Change as needed.
possible_dir <- c('/git_repositories/EIUCrisesMeasure/',
                  '~/git_repositories/EIUCrisesMeasure/')
simpleSetup::set_valid_wd(possible_dir)

# Run set up script
source('source/pca_kpca/setup/setup.R')

# Run KPCA
system.time(
    kpca_eiu(eiu_list, country_date, length_spec = 6,
             out_dir = 'source/pca_kpca/raw_data_output/non_5_strings/')
) 
