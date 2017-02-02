#########################################
# Plot random forest results from C1
# generated from source/kernel_random_forest.R
# Christopher Gandrud
# MIT License
#########################################

# Set working directory
library(simpleSetup)

packages <- c('rio', 'dplyr', 'ggplot2')
library_install(packages)


set_valid_wd('/git_repositories/EIUCrisesMeasure/')

# Load results
imp <- import('data/random_forest_var_imp_C1.csv')

# Convert to percentage
imp$variable_importance <- imp$variable_importance * 100

# Find 30 top
imp <- imp %>% arrange(desc(variable_importance))
imp <- imp[1:40, ]

# Plot
ggplot(imp, aes(variable_importance,
                y = reorder(word_stem, variable_importance))) +
    geom_point() +
    ylab('Word Stem\n') + xlab('\n% MSE Increase') +
    theme_bw()

ggsave('summary_paper/figures/rf_stem_importance.pdf', width = 9.6,
       height = 8.5)
