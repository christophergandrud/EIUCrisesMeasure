# ---------------------------------------------------------------------------- #
# Create/process document-term matrix
# Christopher Gandrud
# 10 March 2015
# MIT License
# ---------------------------------------------------------------------------- #

# Set working directory for the scrapped pages. Change as needed.
setwd('~/Desktop/eiu/')

# Load packages 
library(dplyr)
library(RTextTools)

# Read in parsed texts
parsed <- RTextTools::read_data('eiu_extracted/', type = 'folder',
                                index = 'eiu_index.csv')

# Create document-term matrix
dt <- create_matrix(parsed, language="english", removeNumbers = T,
                    stemWords = T, removeSparseTerms = 0.998)
