# ---------------------------------------------------------------------------- #
# KPCA 5 character kernel (FinStress Index)
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Set working directory. Change as needed.
possible_dir <- c('/git_repositories/EIUCrisesMeasure/',
                  '~/git_repositories/EIUCrisesMeasure/')
repmis::set_valid_wd(possible_dir)

# Run set up script
source('source/pca_kpca/setup/setup.R')

# Run KPCA
system.time(
    kpca_eiu(eiu_list, country_date, length_spec = 5, n_period = 2,
             out_dir = 'source/pca_kpca/raw_data_output/5_strings/')
) 
