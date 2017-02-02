# ---------------------------------------------------------------------------- #
# Scree Plot for EIU Index
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

#### Load pacakges ####
library(repmis)
library(rio)
library(lubridate)
library(dplyr)
library(ggplot2)
library(gridExtra)

#### Set working directory ####
possibles <- c('/git_repositories/EIUCrisesMeasure/',
              '~/git_repositories/EIUCrisesMeasure/')
set_valid_wd(possibles)


#### Scree plot ####
eigen <- import('source/pca_kpca/raw_data_output/5_strings/kpca_5_eigen_10.csv')

ggplot(eigen, aes(components, eigenvalues)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = c(1, 5, 10)) +
    xlab('\nNumber of Components') + ylab('Eigenvalues\n') +
    theme_bw()

ggsave('summary_paper/figures/scree_plot.pdf', width = 8, height = 8)
