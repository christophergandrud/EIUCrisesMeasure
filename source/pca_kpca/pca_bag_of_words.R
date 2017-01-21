# ---------------------------------------------------------------------------- #
# PCA Bag of Words comparision to KPCA
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Set working directory. Change as needed.
possible_dir <- c('/git_repositories/EIUCrisesMeasure/',
                  '~/git_repositories/EIUCrisesMeasure/')
repmis::set_valid_wd(possible_dir)

# Run set up script
source('source/pca_kpca/setup/setup.R')

# PCA bag-of-words scaling -----------------------------------------------------

# Convert corpus to a data frame that is useable by princomp
eiu_df <- as.data.frame(eiu_list)
eiu_df <- gather(eiu_df, id, text)

eiu_dfm <- eiu_df %>% corpus %>% dfm

####
# Remove sparse terms--this differs from the KPCA preprocessing, but is
# necessary for PCA to be possible as the number of terms does not exceed the
# number of documents
eiu_dfm <- dfm_trim(eiu_dfm, sparsity = 0.9)
####

eiu_dfm_df <- quanteda::convert(eiu_dfm, to = 'tm')
eiu_dfm_df <- as.data.frame(as.matrix(eiu_dfm_df))

# PCA of sparse bag of words terms
system.time(
    eiu_pca_bag <- prcomp(eiu_dfm_df, center = TRUE, scale. = TRUE)
)

# Examine eigenvalues
# plot(eiu_pca_bag, type = 'l')

# Extract first component and rescale to be between 0 and 1
range01 <- function(x){(x - min(x))/(max(x) - min(x))}

pca_bag_pc1 <- range01(eiu_pca_bag$x[, 1])

pca_stress <- cbind(country_date, pca_bag_pc1) %>% arrange(country, date)

export(pca_stress,
       file = 'source/pca_kpca/raw_data_output/pca_bag_1stComponent.csv')
