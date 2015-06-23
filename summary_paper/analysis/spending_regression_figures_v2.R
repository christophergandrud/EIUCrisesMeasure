# ---------------------------------------------------------------------------- #
# Create figures from results of spending_regressions.R
# Version 2
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Load required packages
library(devtools)
library(ggplot2)

# Set working directory. Change as needed.
setwd('/git_repositories/EIUCrisesMeasure/summary_paper/')

# Run regressions
source('analysis/spending_regressions_v2.R')

# Load plot function
source_gist('d270ff55c2ca26286e90')

##### Plot Liabilities/Stress 

ls2 <- plot_me(obj = m2_sc, term1 = 'election_year_11', term2 = 'lpr',
               fitted2 = seq(0, 0.75, by = 0.05)) +
    scale_y_continuous(limits = c(-10, 12)) +
    xlab('\nElectoral Loss Probability') +
    ylab('Marginal Effect of Post-Election Year\n') +

ggsave('analysis/figures/me_liab_stress.pdf')

