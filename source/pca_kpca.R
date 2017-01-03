# ---------------------------------------------------------------------------- #
# PCA and KPCA Examinations
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Load packages
library(quanteda)
library(kernlab)
library(repmis)
library(tidyr)
library(microbenchmark)

# Set working directory. Change as needed.
possible_dir <- c('/git_repositories/EIUCrisesMeasure/')
set_valid_wd(possible_dir)

# Load preprocessed data (see source/preprocess_eiu.R)
load('source/preprocessed_data/eiu_texts_from_2003.rda')


# KPCA -------------------------------------------------------------------------
# Kernal length
length_spec = 5

# Number of components
feature_num = 10

# Create string kernels
kernels <- stringdot(type = "spectrum", length = length_spec)

kpca_out <- kpca(eiu_list, kernel = kernels, features = feature_num)


# PCA bag-of-words scaling -----------------------------------------------------

# Convert corpus to a data frame that is useable by princomp
eiu_df <- as.data.frame(eiu_list)
eiu_df <- gather(eiu_df, id, text)

eiu_dfm <- eiu_df %>% corpus %>% dfm
eiu_dfm_df <- convert(eiu_dfm, to = 'tm') 
eiu_dfm_df <- as.data.frame(as.matrix(eiu_dfm_df))

eiu_pca_bag <- princomp(eiu_dfm_df)

# Wordfish bag-of-words scaling ------------------------------------------------

test <- textmodel(eiu_dfm, model = 'wordfish')
