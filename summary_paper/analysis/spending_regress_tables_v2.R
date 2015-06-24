# ---------------------------------------------------------------------------- #
# Create tables from results of spending_regressions.R
# Version 2
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Load required packages
library(dplyr)
library(stargazer)
library(xtable)

# Set working directory. Change as needed.
setwd('/git_repositories/EIUCrisesMeasure/summary_paper/')

# Run regressions
source('analysis/spending_regressions_v2.R')

# Residual Regressions
stargazer(m_r1, m_r1_econ, m_r2, m_r2_econ,
          dep.var.labels = c('Liabilities', 
                             'Econ. Spend', 
                             'Liabilities Resid.',
                             'Econ. Spend Resid.'),
          covariate.labels = c('Liabilities$_{t-1}$', 
                               'Spending$_{t-1}$',
                               'Output Gap',
                               'Liab. Resid.$_{t-1}$',
                               'Econ. Spend Resid.$_{t-1}$',
                               'Perceived Financial Stress'),
          omit = 'iso2c', omit.labels = 'country fixed effects',
          float = F,
          font.size = 'tiny',
          out = 'tables/liab_residual_regress.tex')

# Financail stress Regressions, election year
stargazer(m1_t0, m2_t0, m3_t0, m4_t0, m5_t0,
          dep.var.labels = c('$\\Delta$ Off-Trend Spending',
                             '$\\Delta$ Off-Trend Liabilities'),
          covariate.labels = c('Election Yr.', 'Loss Prob.', 
                               'Econ Ideology', 'Political Constraints',
                               'Fixed FX',
                               'Election Yr. * Loss Prob.'),
          omit = 'iso2c', omit.labels = 'country fixed effects',
          float = F,
          font.size = 'tiny',
          notes = ('Standard errors in parentheses.'),
          out = 'tables/stress_regress_t0.tex'
)

# Financail stress Regressions, post-election year
stargazer(m1_t1, m2_t1, m3_t1, m4_t1, m5_t1,
          dep.var.labels = c('$\\Delta$ Off-Trend Liabilities',
                             '$\\Delta$ Off-Trend Spending'),
          covariate.labels = c('Post-Election Yr.', 'Loss Prob.', 
                               'Econ Ideology', 'Political Constraints',
                               'Fixed FX',
                               'Election Yr. * Loss Prob.'),
          omit = 'iso2c', omit.labels = 'country fixed effects',
          float = F,
          font.size = 'tiny',
          notes = ('Standard errors in parentheses.'),
          out = 'tables/stress_regress_t1.tex'
)

# Country sample
countries <- sub_gov_liab %>% arrange(country, year) %>%
                rename(Country = country) %>%
                select(Country) %>%
                unique %>% as.vector

xtable(countries, caption = 'Regressions Country Sample', 
       label = 'country_sample') %>% 
    print(include.rownames = F,
        size = 'footnotesize',
        caption.placement = 'top',
        file = 'tables/liab_reg_sample.tex')
