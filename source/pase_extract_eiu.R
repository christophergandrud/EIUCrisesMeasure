# ---------------------------------------------------------------------------- #
# Parse EIU texts and conduct keyword searches
# Christopher Gandrud
# 10 March 2015
# MIT License
# ---------------------------------------------------------------------------- #

# Load required packages
library(XML)
library(rvest)
library(dplyr)

# Read in file
full <- htmlParse('~/Desktop/Germany_April_2006.html')

# Extract headlines and body text
extracted <- xpathSApply(doc = full, path = "//div[@class='headline'] | //body//p")
text <- lapply(extracted, html_text)

# Find/extract nodes containing keywords
keywords <- c('credit', 'loan', 'default', 'balance sheet', 'lend')
contains <- sapply(keywords, function(x) grep(x, text, ignore.case = T)) %>%
                unlist %>% as.vector

test <- text[contains] %>% paste(collapse = '')
