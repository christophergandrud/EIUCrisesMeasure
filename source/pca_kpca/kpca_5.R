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

# KPCA -------------------------------------------------------------------------
# Kernal length
length_spec = 5

# Number of components
feature_num = 10

# Create string kernels
kernels <- stringdot(type = "spectrum", length = length_spec)


#------ Test, REMOVE ---------
eiu_list <- eiu_list[1:10]
country_date <- country_date[1:10, ]

# Run KPCA
microbenchmark(
    kpca5_out <- kpca(eiu_list, kernel = kernels, features = feature_num)
)

# Extract features -------------------------------------------------------------
kpca5_df <- pcv(kpca5_out) %>% as.data.frame
names(kpca5_df) <- sprintf('C%s', 1:feature_num)

results_kpca5 <- data.frame(country_date, kpca5_df, stirngsAsFactors = FALSE) %>%
    arrange(country, date) %>% select(-stirngsAsFactors)

# Save raw components
export(results_kpca5,
       file = 'source/pca_kpca/raw_data_output/results_kpca_5_raw.csv')

#### Flip scale, rescale, and smooth -------------------------------------------
results_kpca5$date <- ymd(results_kpca5$date)

# Function to rescale between 0 and 1
range01 <- function(x){(x - min(x))/(max(x) - min(x))}

# Components vector
components_names <- names(results_kpca5)[grep('^C[1-9]', names(results_kpca5))]

# Transform Scale
for (i in components_names) {
    results_kpca5[, i] <- range01(results_kpca5[, i])
}

# Find previous periods moving average
n_period = 1 # TEST ONLY, CHANGE TO 2
sma_mod <- function(x) SMA(x, n = n_period)
results_kpca5 <- results_kpca5 %>% group_by(country) %>%
                    mutate(C1_ma = sma_mod(C1))

export(results_kpca5,
       file = 'data/results_kpca_5_rescaled.csv')

# Scree plot to examine model fit
kpca5_eigen <- eig(kpca5_out)
eigen_plot <- data.frame(components = 1:feature_num, eigenvalues = kpca5_eigen)

export(eigen_plot, file = 'data/kpca_5_eigen_10.csv')

plot(eigen_plot[, 1], eigen_plot[, 2], type = 'o')
