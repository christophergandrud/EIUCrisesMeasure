# ---------------------------------------------------------------------------- #
# Correlations of a selected number of stems
# Christopher Gandrud
# MIT LICENSE   
# ---------------------------------------------------------------------------- #

# Set working directory
setwd('/git_repositories/EIUCrisesMeasure/')

# Load packages
library(rio)
library(dplyr)
library(DataCombine)
library(xtable)

# Load results
corrs <- import('data/C1_stem_correlations.csv')


# Select stems of interest
stems <- c('^debt$', 'imf', 'assist', 'aid', '^strain', 'boom', 'surplus', 
           'weaker', 'stronger', 'growth', 'rise$')

subbed <- corrs %>% grepl.sub(pattern = stems, Var = 'terms') %>% 
                arrange(desc(correlations))

names(subbed) <- c('Stems', 'Correlations')


xtable(subbed, caption = 'Selection of Word Stems and Correlations with FinStress', 
       label = 'stem_correlations') %>% 
    print(include.rownames = F,
          caption.placement = 'top',
          file = 'summary_paper/tables/stem_correlations.tex')
