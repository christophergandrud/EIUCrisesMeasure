library(tm)
library(SnowballC)
library(dplyr)
library(kernlab)

# Set working directory of parsed texts
setwd('~/Desktop/eiu/eiu_extracted/')

# Load corpus
clean_corpus <- Corpus(DirSource()) %>%
                    tm_map(stripWhitespace) %>%
                  #  tm_map(tolower, mc.cores = 1) %>%
                    tm_map(removePunctuation, mc.cores = 1) %>%
                    tm_map(removeNumbers, mc.cores = 1) %>%
                  #  tm_map(removeSparseTerms, 0.98, mc.cores = 1) %>%
                    tm_map(removeWords, stopwords('english'), mc.cores = 1) %>%
                    tm_map(stemDocument, mc.cores = 1)

clean_corpus <- clean_corpus %>% as.list

# Create string kernels
kernals <- stringdot(type = "spectrum", length = 5)

# Test spectral clustering
clusters_out <- specc(clean_corpus, centers = 2, kernel = kernals)

# Match to file names 
results <- data.frame(files = list.files(), cluster = clusters_out@.Data)
