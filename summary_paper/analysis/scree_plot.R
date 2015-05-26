# ---------------------------------------------------------------------------- #
# Scree Plot for EIU Index
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

#### Load pacakges ####
library(rio)
library(lubridate)
library(dplyr)
library(ggplot2)
library(gridExtra)

#### Set working directory ####
setwd('/git_repositories/EIUCrisesMeasure/')

#### Scree plot ####
eigen <- import('data/kpca_eigen_10.csv')

ggplot(eigen, aes(components, eigenvalues)) +
    geom_line() +
    geom_point() +
    scale_x_discrete(breaks = c(1, 5, 10)) +
    xlab('\nNumber of Components') + ylab('Eigenvalues\n') +
    theme_bw()

ggsave('summary_paper/analysis/figures/scree_plot.pdf')
