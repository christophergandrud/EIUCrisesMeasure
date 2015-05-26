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
library(dplyr)
library(ggplot2)

# Load results
imp <- import('data/random_forest_var_imp_C1.csv')

# Convert to percentage
imp$variable_importance <- imp$variable_importance * 100

# Find 30 top
imp <- imp %>% arrange(desc(variable_importance))
imp <- imp[1:30, ]


# Plot
ggplot(imp, aes(variable_importance, word_stem)) +
    geom_point() +
    theme_bw()
