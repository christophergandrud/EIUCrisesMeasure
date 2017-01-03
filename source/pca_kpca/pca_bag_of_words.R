# ---------------------------------------------------------------------------- #
# PCA and Wordfish scaling for comparision to KPCA
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Set working directory. Change as needed.
possible_dir <- c('/git_repositories/EIUCrisesMeasure/')
repmis::set_valid_wd(possible_dir)

# Run set up script
source('source/pca_kpca/setup/setup.R')

# PCA bag-of-words scaling -----------------------------------------------------

# Convert corpus to a data frame that is useable by princomp
eiu_df <- as.data.frame(eiu_list)
eiu_df <- gather(eiu_df, id, text)

eiu_dfm <- eiu_df %>% corpus %>% dfm
eiu_dfm_df <- quanteda::convert(eiu_dfm, to = 'tm') 
eiu_dfm_df <- as.data.frame(as.matrix(eiu_dfm_df))

try(
    eiu_pca_bag <- princomp(eiu_dfm_df)
)

# Wordfish bag-of-words scaling ------------------------------------------------
eiu_wordfish <- textmodel(eiu_dfm, model = 'wordfish')

# Save raw output
save.image(file = 'setup/temp/pca_wordfish.rda')
