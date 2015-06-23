# ---------------------------------------------------------------------------- #
# Create figures from results of spending_regressions.R
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Load required packages
library(dplyr)
library(ggplot2)
library(gridExtra)

# Set working directory. Change as needed.
setwd('/git_repositories/EIUCrisesMeasure/summary_paper/')

# Run regressions
source('analysis/spending_regressions.R')

# Load plot function
source_gist('d270ff55c2ca26286e90')


##### Liabilities/Stress
ls1 <- plot_me(obj = m2_s_t0, term1 = 'election_year1', term2 = 'lpr_1',
        fitted2 = seq(0, 0.75, by = 0.05)) +
    scale_y_continuous(limits = c(-10, 10)) +
    xlab('\nElectoral Loss Probability (lag 1)') +
    ylab('Marginal Effect of Election Year\n') +
    ggtitle('Election Year\n')

ls2 <- plot_me(obj = m2_s_t1, term1 = 'election_year_1', term2 = 'lpr',
               fitted2 = seq(0, 0.75, by = 0.05)) +
    scale_y_continuous(limits = c(-10, 10)) +
    xlab('\nElectoral Loss Probability') +
    ylab('') +
    ggtitle('Post-Election Year\n')

pdf(file = 'analysis/figures/me_liab_stress.pdf', width = 9.3, height = 5)
    grid.arrange(ls1, ls2, ncol = 2)
dev.off()

