# ---------------------------------------------------------------------------- #
# Parse EIU texts and conduct keyword searches
# Christopher Gandrud
# 22 May 2015
# MIT License
# ---------------------------------------------------------------------------- #

# Set working directory for the scrapped pages. Change as needed.
setwd('/Volumes/Gandrud1TB/eiu/')

# Load required packages
library(XML)
library(rvest)
library(dplyr)
library(stringr)
library(lubridate)
library(rio)
## Also requires the installation of qdap

#### Clean file names ##########################################################
# List files
raw_files <- list.files('eiu_raw/')

# Convert HTML file names to standardised txt file names
strip <- c('.html', '_Main_report', '_Main_Report',  '_Updater', '_of_America')
files_clean <- qdap::mgsub(strip, '', raw_files)

year <- gsub("([^_]+)\\_", "", files_clean)
month <- str_split_fixed(files_clean, '_[^_]*$', n = 2)[, 1] %>%
            gsub("([^_]+)\\_", "", .)

country <- str_split_fixed(files_clean, '_[^_]*$', n = 2)[, 1] %>%
            str_split_fixed(., '_[^_]*$', n = 2)
country <- country[, 1] %>% gsub('_', '-', .)

dates <- sprintf('01_%s_%s', month, year) %>% dmy()

file_txt <- sprintf('%s_%s.txt', dates, country)


#### Parse/Extract #############################################################
# Keywords to seach/extract for. Modified from Romer and Romer (2015):
# http://eml.berkeley.edu/~cromer/RomerandRomerFinancialCrisesAppendixA.pdf
# NEED TO ADD TO/THINK ABOUT
keywords <- c("bail-out", "bailout", "balance sheet", "bank", "credit",
              "crunch", "default", "financial", "lend", "loan", "squeeze")

for (i in 1:length(file_txt)) {
    # Read in file
    message(raw_files[i])
    full <- htmlParse(sprintf('eiu_raw/%s', raw_files[i]))

    if (!is.null(full)) {
        # Extract headlines and body text
        extracted <- xpathSApply(doc = full,
                                path = "//div[@class='headline'] | //body//p")
    }
    if (!is.null(extracted)) {
        text <- lapply(extracted, html_text)

        # Find/extract nodes containing keywords
        contains <- sapply(keywords, 
                           function(x) grep(x, text, ignore.case = T)) %>%
                    unlist %>% as.vector
        text_out <- text[contains] %>% paste(collapse = '')

        # Write to file
        writeLines(text_out, sprintf('eiu_extracted/%s', file_txt[i]))
    }
}
