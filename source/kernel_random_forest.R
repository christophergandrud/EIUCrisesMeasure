# ---------------------------------------------------------------------------- #
# Compare Kernal PCA to Stem Frequency
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Set working directory of parsed texts. Change as needed.
# setwd('/Volumes/Gandrud1TB/eiu/eiu_extracted/')
setwd('~/Desktop/eiu/eiu_extracted/')

# Load packages
library(tm)
library(SnowballC)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(rio)
library(randomForestSRC)

# Function to count the number of words in a string
wordcount <- function(x) sapply(gregexpr("\\W+", x), length) + 1

# Load corpus
clean_corpus_full <- Corpus(DirSource()) %>%
                    tm_map(removeWords, stopwords('english'), mc.cores = 1) %>%
                    tm_map(stemDocument, mc.cores = 1) %>%
                    tm_map(stripWhitespace) %>%
                    # tm_map(content_transformer(tolower), mc.cores = 1) %>%
                    tm_map(removePunctuation, mc.cores = 1) %>%
                    tm_map(removeNumbers, mc.cores = 1)

# Kernal length
length_spec = 5

clean_corpus_full <- clean_corpus_full %>% as.list

clean_corpus <- clean_corpus_full[keep_vec] %>% as.VCorpus %>%
                    DocumentTermMatrix
term_freq <- inspect(removeSparseTerms(clean_corpus, 0.9)) %>% as.data.frame

## Drop countries with fewer than 5 observations
# Create date-country labels
date_country <- row.names(term_freq) %>% gsub('\\.txt', '', .) %>%
    str_split_fixed('_', n = 2) %>%
    as.data.frame(stringsAsFactors = F)
date_country[, 2] <- gsub('-', ' ', date_country[, 2])
names(date_country) <- c('date_date', 'country_country') # date and country are terms

term_freq <- cbind(date_country, term_freq)

#### Download KPCA results ####
kpca <- import('~/git_repositories/EIUCrisesMeasure/data/results_kpca_rescaled.csv')

# Create matching corpus
kpca_included <- kpca %>% select(date, country)
names(kpca_included) <- c('date_date', 'country_country')
term_freq <- merge(kpca_included, term_freq,
                   by = c('country_country', 'date_date'),
                   all.x = T) %>%
                select(-country_country, -date_date)

#### Combine ####
cor_pca <- function(var) {
    temp_function <- function(x) cor(kpca[, var], x)
    corrs <- apply(term_freq, 2, temp_function)
    corrs <- data.frame(terms = names(term_freq), correlations = corrs)
    corrs_sorted <- corrs %>% arrange(desc(correlations))
    return(corrs_sorted)
}

c1_cor <- cor_pca('C1')
c2_cor <- cor_pca('C2')
c3_cor <- cor_pca('C3')

#### Random forest ####
setwd('~/git_repositories/EIUCrisesMeasure/')

comb <- cbind(kpca$C1, term_freq)
comb <- dplyr::rename(comb, C1 = `kpca$C1`)

addq <- function(x) paste0("`", x, "`")

form <- paste('C1 ~', paste(addq(names(term_freq)), collapse = ' + ')) %>%
          as.formula

rfsrc_c1 <- rfsrc(form, data = comb)

# Plot variable importance
plot.rfsrc(rfsrc_c1, plots.one.page = F)

# Save results to a data frame
imp <- rfsrc_c1$importance %>% as.data.frame
imp$stem <- row.names(imp)
names(imp) <- c('variable_importance', 'word_stem')
imp <- imp %>% select(word_stem, variable_importance)

export(imp, file = 'data/random_forest_var_imp_C1.csv')

plot.variable(rfsrc_c1)
