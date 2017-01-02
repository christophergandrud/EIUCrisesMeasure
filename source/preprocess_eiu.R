# ---------------------------------------------------------------------------- #
# Pre-Process texts
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Load packages
if (!('quanteda' %in% installed.packages()[, 1])) 
    devtools::install_github('christophergandrud/quanteda')
library(quanteda)
if (!('readtext' %in% installed.packages()[, 1])) 
    devtools::install_github('kbenoit/readtext')
library(readtext)
library(repmis)
library(lubridate)
library(dplyr)
library(stringr)

# Set working directory of parsed texts. Change as needed.
pos_directs <- c('~/Desktop/eiu/eiu_extracted/',
                 '/Volumes/Gandrud1TB/eiu/eiu_extracted/',
                 '/Volumes/SAMSUNG128/data/eiu/eiu_extracted/')

set_valid_wd(pos_directs)

# Create date-country labels
date_country <- list.files() %>% gsub('\\.txt', '', .) %>%
                    str_split_fixed('_', n = 2) %>%
                    as.data.frame(stringsAsFactors = F)
date_country[, 2] <- gsub('-', ' ', date_country[, 2])
names(date_country) <- c('date', 'country')
date_country$date <- ymd(date_country$date)

# Load corpus and preprocess
texts_df <- readtext(file = list.files())

# Apply clean row names 
texts_df <- cbind(texts_df, date_country)

# Remove texts from before 2003 due to inconsistent format
texts_df_2003 <- subset(texts_df, date >= '2003-01-01')
texts_2003_texts <- data.frame(texts_df_2003[, 1], stringsAsFactors = FALSE)

# Create corpus
eiu_corpus <- corpus(texts_2003_texts, text_field = 1)

# Add document metadata
docvars(eiu_corpus, 'date') <- texts_df_2003$date
docvars(eiu_corpus, 'country') <- texts_df_2003$country


# Preprocess and convert to document-feature matrix
eiu_token <- tokenize(eiu_corpus, stem = TRUE, removeNumbers = TRUE, 
               removePunct = TRUE, removeSeparators = TRUE, removeSymbols = TRUE)

eiu_token <- toLower(eiu_token)
eiu_token <- removeFeatures(eiu_token, stopwords('english'))




clean_corpus_full <- Corpus(DirSource()) %>%
    tm_map(removeWords,
           stopwords(kind = "SMART"), mc.cores = 2) %>%
    tm_map(stemDocument, mc.cores = 2) %>%
    tm_map(stripWhitespace) %>%
    # tm_map(content_transformer(tolower), mc.cores = 1) %>%
    tm_map(removePunctuation, mc.cores = 2) %>%
    tm_map(removeNumbers, mc.cores = 2) 
