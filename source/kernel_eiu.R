library(tm)
library(SnowballC)
library(dplyr)
library(kernlab)

# Load corpus
clean_corpus <- Corpus(DirSource('~/Desktop/eiu/eiu_extracted/')) %>%
                    tm_map(removePunctuation) %>%
                    tm_map(removeNumbers) %>%
                    tm_map(removeWords, stopwords('english'), mc.cores = 1) %>%
                    tm_map(stemDocument, mc.cores = 1)

clean_corpus <- clean_corpus %>% as.list

# Create string kernels
kernals <- stringdot(type = "spectrum", length = 5)

# Test spectral clustering
test <- specc(clean_corpus, centers = 2, kernel = kernals)
