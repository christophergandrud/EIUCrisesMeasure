# ---------------------------------------------------------------------------- #
# Application Tables
# Christopher Gandrud
# MIT LICENSE
# ---------------------------------------------------------------------------- #

# Load required packages 
library(devtools)
library(dplyr)
library(DataCombine)
library(stargazer)

# Run regressions
devtools::source_url('http://bit.ly/1Hwrshl')

source('/git_repositories/financial_crisis_fiscal_policy/analysis_data/spending_regressions_v2.R')

# Load plot function
devtools::source_gist('d270ff55c2ca26286e90')

# Reset working directory
setwd('/git_repositories/EIUCrisesMeasure/summary_paper/')

# EPFMS only
m_r2_basic_epfms <- lm(residuals_output_debt ~ residuals_output_debt_1 + 
                     mean_stress, 
                 data = sub_debt)

# Create residuals table
stargazer(m_r1, m_r2_basic, m_r2_basic_epfms, m_r2,
          dep.var.labels = c('Debt', 
                             'Debt Residuals'),
          covariate.labels = c('Debt$_{t-1}$', 
                               'Output Gap',
                               'Debt Resid.$_{t-1}$',
                               'Laeven \\& Valencia Bank Crisis',
                               'EPFMS',
                               'Crisis * EPFMS'),
          omit = 'iso2c', omit.labels = 'country fixed effects',
          float = F,
          df = F,
          font.size = 'footnotesize',
          out = 'tables/debt_residual_regress.tex'
)


##### Plot stress/crisis interaction
plot_me(obj = m_r2, term1 = 'lv_bank_crisis', term2 = 'mean_stress',
        fitted2 = seq(0.3, 0.75, by = 0.05)) +
    scale_y_continuous(limits = c(-10, 15)) +
    xlab('\nPerceptions of Financial Market Stress') +
    ylab('Marginal Effect of Qualitative Banking Crisis\n') +
    ggtitle('Change in Off-Trend Debt (residual of output)\n')

ggsave(filename = 'analysis/figures/lv_epfms.pdf')

