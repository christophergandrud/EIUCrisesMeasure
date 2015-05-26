#########################################
# Plot random forest results from C1
# generated from source/kernel_random_forest.R
# Christopher Gandrud
# MIT License
#########################################

# Set working directory
setwd('/git_repositories/EIUCrisesMeasure/')

# Load packages
library(rio)
library(ggplot2)

# Load results
imp <- import('data/')
