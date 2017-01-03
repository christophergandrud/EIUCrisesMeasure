# ---------------------------------------------------------------------------- #
# PCA and KPCA Examinations
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Load packages
library(quanteda)
library(kernlab)
library(repmis)

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

