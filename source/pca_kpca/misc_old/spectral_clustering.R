# ---------------------------------------------------------------------------- #
# Spectral Clustering comparision to KPCA 5
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Set working directory. Change as needed.
possible_dir <- c('/git_repositories/EIUCrisesMeasure/',
                  '~/git_repositories/EIUCrisesMeasure/')
repmis::set_valid_wd(possible_dir)

# Run set up script
source('source/pca_kpca/setup/setup.R')

# SPECC -------------------------------------------------------------------------
# Kernal length
length_spec = 5

# Number of components
feature_num = 10

# Create string kernels
kernels <- stringdot(type = "spectrum", length = length_spec)

# Run
microbenchmark(
    specc5_out <- specc(eiu_list, kernel = kernels, features = feature_num)
)

# Extract features -------------------------------------------------------------
specc5_df <- pcv(specc5_out) %>% as.data.frame
names(specc5_df) <- sprintf('C%s', 1:feature_num)

results_specc5 <- data.frame(country_date, specc5_df, stirngsAsFactors = FALSE) %>%
    arrange(country, date) %>% select(-stirngsAsFactors)

# Save raw components
export(results_specc5,
       file = 'source/pca_kpca/raw_data_output/results_specc_5_raw.csv')

#### Flip scale, rescale, and smooth -------------------------------------------
results_specc5$date <- ymd(results_specc5$date)

# Function to rescale between 0 and 1
range01 <- function(x){(x - min(x))/(max(x) - min(x))}

# Components vector
components_names <- names(results_specc5)[grep('^C[1-9]', names(results_specc5))]

# Transform Scale
for (i in components_names) {
    results_specc5[, i] <- range01(results_specc5[, i])
}

# Find previous periods moving average
n_period = 2
sma_mod <- function(x) SMA(x, n = n_period)
results_specc5 <- results_specc5 %>% group_by(country) %>%
                    mutate(C1_ma = sma_mod(C1))

export(results_specc5,
       file = 'source/pca_kpca/raw_data_output/results_specc_5_rescaled.csv')

# Scree plot to examine model fit
specc5_eigen <- eig(specc5_out)
eigen_plot <- data.frame(components = 1:feature_num, eigenvalues = specc5_eigen)

export(eigen_plot, file = 'source/pca_kpca/raw_data_output/specc_5_eigen_10.csv')

plot(eigen_plot[, 1], eigen_plot[, 2], type = 'o')
