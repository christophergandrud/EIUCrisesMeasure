# ---------------------------------------------------------------------------- #
# Pre-Process texts
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Load packages
## Currently requires custom quanteda version due to a bug in the
## official release.
library(simpleSetup)
pkgs <- c('lubridate', 'dplyr', 'stringr', 'countrycode')
library_install(pkgs)

if (!('quanteda' %in% installed.packages()[, 1]))
    devtools::install_github('kbenoit/quanteda')
library(quanteda)
if (!('readtext' %in% installed.packages()[, 1]))
    devtools::install_github('kbenoit/readtext')
library(readtext)

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
date_country$iso3c <- countrycode(date_country$country,
                                  origin = 'country.name',
                                  destination = 'iso3c',
                                  warn = TRUE)

# Load corpus and preprocess
texts_df <- readtext(file = list.files())

# Apply clean row names
texts_df <- cbind(texts_df, date_country)

# Remove non-countries
texts_df <- subset(texts_df, !is.na(iso3c))

# Remove texts from before 2003 due to inconsistent format
texts_df_2003 <- subset(texts_df, date >= '2003-01-01')
texts_2003_texts <- data.frame(texts_df_2003[, 1],
                               stringsAsFactors = FALSE)

# Create corpus
eiu_corpus <- corpus(texts_2003_texts, text_field = 1)

# Add document metadata
docvars(eiu_corpus, 'date') <- texts_df_2003$date
docvars(eiu_corpus, 'country') <- texts_df_2003$country

# Preprocess and convert to document-feature matrix
eiu_token <- quanteda::tokenize(eiu_corpus, removeNumbers = TRUE,
               removePunct = TRUE, removeSeparators = TRUE,
               removeSymbols = TRUE, removeHyphens = TRUE,
               removeTwitter = TRUE, removeURL = TRUE,
               verbose = TRUE)

# For unknown reasons these functions do not work within tokenize
eiu_token <- toLower(eiu_token)
eiu_token <- removeFeatures(eiu_token, stopwords('SMART'))
eiu_token <- tokens_wordstem(eiu_token)

# Find documents with fewer than 5 tokens
length_spec <- 5 # Assigns token length
keep_vec <- vector()
for (i in 1:length(eiu_token)) {
    more_length <- length(eiu_token[[i]]) > length_spec
    if (isTRUE(more_length)) keep_vec <- c(keep_vec, i)
}

# Collapse into a list of character vectors for each document
eiu_list <- lapply(eiu_token, paste, collapse = ' ')

# Remove documents with fewer than 5 tokens
eiu_list <- eiu_list[keep_vec]
eiu_ids <- texts_df_2003[keep_vec, c('date', 'iso3c')]
names(eiu_list) <- paste(eiu_ids[, 'iso3c'], eiu_ids[, 'date'],
                         sep = '_')

# Save preprocessed corpus in the git repository. Change as needed.
pos_directs_git <- c('/git_repositories/EIUCrisesMeasure/source/',
                    '~/git_repositories/EIUCrisesMeasure/source/')
set_valid_wd(pos_directs_git)

save(eiu_list, file = 'pca_kpca/preprocessed_data/eiu_texts_from_2003.rda')
