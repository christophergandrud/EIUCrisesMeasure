# ---------------------------------------------------------------------------- #
# Parse EIU texts and conduct keyword searches
# Christopher Gandrud
# 10 March 2015
# MIT License
# ---------------------------------------------------------------------------- #

# Set working directory for the scrapped pages. Change as needed.
setwd('~/Desktop')

# Load required packages
library(XML)
library(rvest)
library(dplyr)

# Keywords to seach/extract for. NEED TO ADD TO/THINK ABOUT
keywords <- c('credit', 'loan', 'default', 'balance sheet', 'lend')

# List files
raw_files <- list.files('eiu_raw/')

for (i in raw_files){
    # Read in file
    full <- htmlParse(sprintf('eiu_raw/%s', i))
    
    # Convert file name to txt
    file_txt <- gsub('html', 'txt', i)
    
    # Extract headlines and body text
    extracted <- xpathSApply(doc = full, 
                             path = "//div[@class='headline'] | //body//p")
    text <- lapply(extracted, html_text)
    
    # Find/extract nodes containing keywords
    contains <- sapply(keywords, function(x) grep(x, text, ignore.case = T)) %>%
                    unlist %>% as.vector
    text_out <- text[contains] %>% paste(collapse = '')
    
    # Write to file
    writeLines(text_out, sprintf('eiu_extracted/%s', file_txt))
}






