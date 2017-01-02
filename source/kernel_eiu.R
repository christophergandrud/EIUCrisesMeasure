# ---------------------------------------------------------------------------- #
# Pre-Process texts/Kernel methods
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Load packages
library(tm)
library(SnowballC)
library(dplyr)
library(kernlab)
library(stringr)
library(lubridate)
library(rio)
library(TTR)
library(countrycode)
library(DataCombine)
library(repmis)

# Set working directory of parsed texts. Change as needed.
pos_directs <- c('~/Desktop/eiu/eiu_extracted/',
                    '/Volumes/Gandrud1TB/eiu/eiu_extracted/',
                    '/Volumes/SAMSUNG128/data/eiu/eiu_extracted/')

set_valid_wd(pos_directs)

# Function to count the number of words in a string
wordcount <- function(x) sapply(gregexpr("\\W+", x), length) + 1

# Create date-country labels
date_country <- list.files() %>% gsub('\\.txt', '', .) %>%
    str_split_fixed('_', n = 2) %>%
    as.data.frame(stringsAsFactors = F)
date_country[, 2] <- gsub('-', ' ', date_country[, 2])
names(date_country) <- c('date', 'country')
date_country$date <- ymd(date_country$date)

# Load corpus and preprocess
clean_corpus_full <- Corpus(DirSource()) %>%
                    tm_map(removeWords,
                           stopwords(kind = "SMART")) %>%
                    tm_map(stemDocument) %>%
                    tm_map(stripWhitespace) %>%
                    # tm_map(content_transformer(tolower), mc.cores = 1) %>%
                    tm_map(removePunctuation) %>%
                    tm_map(removeNumbers)
# Save corpus
writeCorpus(x = clean_corpus_full, path = '~/Desktop/pre_processed_eiu_corpus')

# Kernal length
length_spec = 5

clean_corpus_full <- clean_corpus_full %>% as.list

# Keep texts that have more words than the kernal length
keep_vec <- vector()
for (i in 1:length(clean_corpus_full)) {
    clean_corpus_full[[i]]$content <- clean_corpus_full[[i]]$content %>%
                                        paste(collapse = '')
    temp <- clean_corpus_full[[i]]$content
    more_length <- wordcount(temp) > length_spec
    if (isTRUE(more_length)) keep_vec <- c(keep_vec, i)
}

clean_corpus <- clean_corpus_full[keep_vec]
#(clean_corpus_full)

date_country <- date_country[keep_vec, ]

# Create string kernels
kernels <- stringdot(type = "spectrum", length = length_spec)

# Number of components
feature_num = 10

# Estimate
kpca_out <- kpca(clean_corpus, kernel = kernels, features = feature_num)

kpca_df <- pcv(kpca_out) %>% as.data.frame
names(kpca_df) <- sprintf('C%s', 1:feature_num)

results_kpca <- data.frame(date_country, kpca_df, stirngsAsFactors = F) %>%
                    arrange(country, date) %>% select(-stirngsAsFactors)

#### Save ####
# Clean up country name and add in iso2c code
results_kpca$country <- gsub('%28', ' ', results_kpca$country)
results_kpca$country <- gsub('%29', '', results_kpca$country)
results_kpca$iso2c <- countrycode(results_kpca$country,
                            origin = 'country.name', destination = 'iso2c')
results_kpca <- results_kpca %>% filter(!is.na(iso2c))

results_kpca <- results_kpca %>% MoveFront(c('iso2c', 'country',
                        'date'))
export(results_kpca,
       file = '~/git_repositories/EIUCrisesMeasure/data/results_kpca_raw.csv')

#### Flip scale, rescale, and smooth ####
results_kpca$date <- ymd(results_kpca$date)

# Function to rescale between 0 and 1
range01 <- function(x){(x - min(x))/(max(x) - min(x))}

# Components vector
components_names <- names(results_kpca)[grep('^C[1-9]', names(results_kpca))]

# Transform Scale
for (i in components_names) {
    # results_kpca[, i] <- results_kpca[, i] * -1
    results_kpca[, i] <- range01(results_kpca[, i])
}

## Drop countries with fewer than 5 observations
results_kpca$fake <- 1
results_kpca <- results_kpca %>% group_by(country) %>%
                mutate(obs_sum = sum(fake)) %>%
                filter(obs_sum > 5) %>% select(-fake, -obs_sum)

# Find previous periods moving average
sma_mod <- function(x) SMA(x, n = 2)
results_kpca <- results_kpca %>% group_by(country) %>%
                mutate(C1_ma = sma_mod(C1))

export(results_kpca,
       file = '~/git_repositories/EIUCrisesMeasure/data/results_kpca_rescaled.csv')

# Scree plot to examine model fit
kpca_eigen <- eig(kpca_out)
eigen_plot <- data.frame(components = 1:feature_num, eigenvalues = kpca_eigen)

export(eigen_plot, file = '~/git_repositories/EIUCrisesMeasure/data/kpca_eigen_10.csv')

plot(eigen_plot[, 1], eigen_plot[, 2], type = 'o')

# ---------------------------------------------------------------------------- #

#### Test spectral clustering ####
# clusters_out <- specc(clean_corpus, centers = 2, kernel = kernals)

# Create output data frame
# results_cluster <- data.frame(date_country, cluster = clusters_out@.Data,
#                      stringsAsFactors = F) %>%
#                      arrange(country, date)

# Plot results
# ggplot(results_cluster, aes(date, as.factor(cluster), group = country,
#                    colour = country)) +
#        facet_grid(country ~ .) +
#        geom_line() +
#        scale_color_brewer(palette = 'Set1') +
#        xlab('') + ylab('') +
#        theme_bw()

#### Kernel PCA ################################################################


#### Find change points ####
# devtools::source_url('https://raw.githubusercontent.com/christophergandrud/FedChangePointNote/master/paper/source/e.divGG.R')

# kpca_changepoint <- list()
# for (i in unique(date_country$country)) {
#     message(i)
#     temp_data <- subset(results_kpca, country == i)
#     temp_plot <- e.divGG(data = temp_data, Vars = 'C1',
#                                      TimeVar = 'date', min.size = 6) +
#                                 ggtitle(i)
#     kpca_changepoint[[i]] <- temp_plot
# }

# do.call(grid.arrange, kpca_changepoint)
