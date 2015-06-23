# ---------------------------------------------------------------------------- #
# Create figures from results of spending_regressions.R
# Version 2
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Load required packages
library(devtools)
library(ggplot2)
library(gridExtra)
library(tikzDevice)

# Set working directory. Change as needed.
setwd('/git_repositories/EIUCrisesMeasure/summary_paper/')

# Run regressions
source('analysis/spending_regressions_v2.R')

# Load plot function
source_gist('d270ff55c2ca26286e90')

##### Plot Liabilities/Stress 
plot1 <- plot_me(obj = m3_t0, term1 = 'election_year1', term2 = 'lpr_1',
        fitted2 = seq(0, 0.75, by = 0.05)) +
    scale_y_continuous(limits = c(-10, 10)) +
    xlab('\nElectoral Loss Probability') +
    ylab('Marginal Effect of Election Year\n') +
    ggtitle('Change in Off Trend Spending\n')



plot2 <- plot_me(obj = m3_t1, term1 = 'election_year_11', term2 = 'lpr',
               fitted2 = seq(0, 0.75, by = 0.05)) +
    scale_y_continuous(limits = c(-10, 12)) +
    xlab('\nElectoral Loss Probability') +
    ylab('Marginal Effect of Post-Election Year\n') +
    ggtitle('Change in Off Trend Liabilities\n')


pdf(file = 'analysis/figures/me_stress.pdf', width = 9.3, height = 5)
    grid.arrange(plot1, plot2, ncol = 2)
dev.off()

