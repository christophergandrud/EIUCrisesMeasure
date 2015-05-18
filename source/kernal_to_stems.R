# ---------------------------------------------------------------------------- #
# Compare Kernal PCA to Stem Frequency
# Christopher Gandrud
# 18 May 2015
# MIT License
# ---------------------------------------------------------------------------- #

# Set working directory of parsed texts. Change as needed.
setwd('/Volumes/Gandrud1TB/eiu/eiu_extracted/')

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

# Function to count the number of words in a string
wordcount <- function(x) sapply(gregexpr("\\W+", x), length) + 1

# Create date-country labels
date_country <- list.files() %>% gsub('\\.txt', '', .) %>%
    str_split_fixed('_', n = 2) %>% 
    as.data.frame(stringsAsFactors = F)
date_country[, 2] <- gsub('-', ' ', date_country[, 2])
names(date_country) <- c('date', 'country')
date_country$date <- ymd(date_country$date)


# Load corpus
clean_corpus_full <- Corpus(DirSource()) %>%
  tm_map(removeWords, stopwords('english'), mc.cores = 1) %>%
  tm_map(stemDocument, mc.cores = 1) %>%
  tm_map(stripWhitespace) %>%
  # Results correspond to priors much more closely when case is retained
  tm_map(content_transformer(tolower), mc.cores = 1) %>%
  tm_map(removePunctuation, mc.cores = 1) %>%
  tm_map(removeNumbers, mc.cores = 1) 

# Kernal length
length_spec = 5

clean_corpus_full <- clean_corpus_full %>% as.list

# Keep texts that have more words than the kernal length
keep_vec <- vector()
for (i in 1:length(clean_corpus_full)) {
    temp <- clean_corpus_full[[i]]$content
    more_length <- wordcount(temp) > length_spec
    if (isTRUE(more_length)) keep_vec <- c(keep_vec, i)
}

clean_corpus <- clean_corpus_full[keep_vec] %>% as.VCorpus %>%
                    DocumentTermMatrix
term_freq <- inspect(removeSparseTerms(clean_corpus, 0.9)) %>% as.data.frame

# rm(clean_corpus_full)

#### Download KPCA results ####
kpca <- import('https://raw.githubusercontent.com/christophergandrud/EIUCrisesMeasure/master/data/results_kpca.csv')

#### Combine #### 
cor(kpca$C1, term_freq[, 1])
cor_pca <- function(x) cor(kpca$C1, x)
corrs <- apply(term_freq, 2, cor_pca)
corrs <- data.frame(terms = names(term_freq), correlations = corrs)
corrs_sorted <- corrs %>% arrange(desc(correlations))

#### Random forest test ####
library(randomForest)

comb <- cbind(kpca$C1, term_freq)
comb <- dplyr::rename(comb, C1 = `kpca$C1`)

addq <- function(x) paste0("`", x, "`")

form <- paste('C1 ~', paste(addq(names(term_freq)[sample(1:ncol(term_freq), 350)]), collapse = ' + ')) %>% 
          as.formula

rf1 <- randomForest(form, data = comb, importance = T, do.trace = 100)

term_names <- rownames(importance(rf1))
term_importance <- cbind.data.frame(term_names, importance(rf1))
term_importance <- term_importance %>% arrange(desc(`%IncMSE`))
