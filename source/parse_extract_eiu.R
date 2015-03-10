# ---------------------------------------------------------------------------- #
# Parse EIU texts and conduct keyword searches
# Christopher Gandrud
# 10 March 2015
# MIT License
# ---------------------------------------------------------------------------- #

# Set working directory for the scrapped pages. Change as needed.
setwd('~/Desktop/eiu/')

# Load required packages
library(XML)
library(rvest)
library(dplyr)
if (!('rio' %in% installed.packages()[, 1]))
    devtools::install_github('leeper/rio', ref = 'fread')
library(rio)

# Keywords to seach/extract for. Modified from Romer and Romer (2015):
# http://eml.berkeley.edu/~cromer/RomerandRomerFinancialCrisesAppendixA.pdf
# NEED TO ADD TO/THINK ABOUT
keywords <- c("bail-out", "bailout", "balance sheet", "bank", "credit",
              "crunch", "default", "financial", "lend", "loan", "squeeze")

# List files
raw_files <- list.files('eiu_raw/')

# Convert file names to txt file names
file_txt <- gsub('html', 'txt', raw_files)

# Convert txt file names to text indices
strip <- c('.html', '_Main_report', '_Main_Report',  '_Updater')
indices <- qdap::mgsub(strip, '', raw_files)

# Create document index
data.frame(file_txt, indices) %>% export(file = 'eiu_index.csv', col.names = F)

#### Parse/Extract ###
for (i in 1:length(raw_files)){
    # Read in file
    full <- htmlParse(sprintf('eiu_raw/%s', raw_files[i]))

    # Extract headlines and body text
    extracted <- xpathSApply(doc = full,
                             path = "//div[@class='headline'] | //body//p")
    text <- lapply(extracted, html_text)

    # Find/extract nodes containing keywords
    contains <- sapply(keywords, function(x) grep(x, text, ignore.case = T)) %>%
                    unlist %>% as.vector
    text_out <- text[contains] %>% paste(collapse = '')

    # Write to file
    writeLines(text_out, sprintf('eiu_extracted/%s', file_txt[i]))
}
