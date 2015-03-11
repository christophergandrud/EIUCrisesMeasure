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
clusters_out <- specc(clean_corpus, centers = 3, kernel = kernals)


## Plot
library(stringr)
library(ggplot2)

# Match to file names 
date_country <- list.files() %>% gsub('\\.txt', '', .) %>% 
                    str_split_fixed('_', n = 2) 

results <- data.frame(date_country, cluster = clusters_out@.Data, 
                      stringsAsFactors = F) %>%
            arrange(X2, X1)
names(results) <- c('date', 'country', 'cluster')

results$date <- ymd(results$date)

results <- filter(results, country != 'Australia')

ggplot(results, aes(date, cluster, group = country, colour = country)) +
        facet_grid(country ~.) +
        geom_line() +
        theme_bw()


