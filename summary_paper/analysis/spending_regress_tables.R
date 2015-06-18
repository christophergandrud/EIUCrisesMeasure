# ---------------------------------------------------------------------------- #
# Create tables from results of spending_regressions.R
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
source('analysis/spending_regressions.R')

# Residual Regressions
stargazer(m_r1, m_r2, m_r1_spend, m_r2_spend,
          dep.var.labels = c('$\\Delta$ Liabilities', 
                             '$\\Delta$ Liabilities Resid.',
                             '$\\Delta$ Econ. Spend', 
                             '$\\Delta$ Econ. Spend Resid.'),
          covariate.labels = c('Output Gap', 'Perceived Financial Stress'),
          omit = 'iso2c', omit.labels = 'country fixed effects',
          float = F,
          font.size = 'footnotesize',
          out = 'tables/liab_residual_regress.tex')

# Country sample
countries <- sub_gov_liab %>% arrange(country, year) %>%
                rename(Country = country) %>%
                select(Country) %>%
                unique %>% as.vector

xtable(countries) %>% print(include.rownames = F,
                            caption = 'Country Sample',
                            label = 'country_sample',
                            size = 'footnotesize',
                            file = 'tables/liab_reg_sample.tex')
