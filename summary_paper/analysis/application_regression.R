# ---------------------------------------------------------------------------- #
# Application Tables
# Christopher Gandrud
# MIT LICENSE
# ---------------------------------------------------------------------------- #

# Load required packages 
library(devtools)
library(dplyr)
library(stargazer)

# Run regressions
source_url('https://raw.githubusercontent.com/christophergandrud/financial_crisis_fiscal_policy/master/analysis_data/spending_regress_tables_v2.R')

# Reset working directory
setwd('/git_repositories/EIUCrisesMeasure/summary_paper/')

# Create residuals table
stargazer(m_r1, m_r2,
          dep.var.labels = c('Debt', 
                             'Debt Residuals'),
          covariate.labels = c('Debt$_{t-1}$', 
                               'Output Gap',
                               'Debt Resid.$_{t-1}$',
                               'EPFMS'),
          omit = 'iso2c', omit.labels = 'country fixed effects',
          float = F,
          omit.stat = c('f', 'ser'),
          font.size = 'footnotesize',
          out = 'tables/debt_residual_regress.tex'
)

