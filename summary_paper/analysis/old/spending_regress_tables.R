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
source('analysis/spending_regressions_v2.R')

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

# Financail stress Regressions, election year
stargazer(m1_s_t0, m2_s_t0, m3_s_t0,
          dep.var.labels.include = F,
          covariate.labels = c('Election Yr.', 'Loss Prob. (lag 1)', 
                               'Econ Ideology', 'Political Constraints',
                               'Election Yr. * Loss Prob.'),
          omit = 'iso2c', omit.labels = 'country fixed effects',
          float = F,
          font.size = 'footnotesize',
          out = 'tables/stress_regress_t0.tex'
          )

# Financail stress Regressions, post-election year
stargazer(m1_s_t1, m2_s_t1, m3_s_t1,
          dep.var.labels.include = F,
          covariate.labels = c('Post-Election Yr.', 'Loss Prob.', 
                               'Econ Ideology', 'Political Constraints',
                               'Election Yr. * Loss Prob.'),
          omit = 'iso2c', omit.labels = 'country fixed effects',
          float = F,
          font.size = 'footnotesize',
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
