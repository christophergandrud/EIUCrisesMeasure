# ---------------------------------------------------------------------------- #
# Pre-Process texts/Examine kernel methods
# Christopher Gandrud
# 19 May 2015
# MIT License
# ---------------------------------------------------------------------------- #

# Set working directory of parsed texts. Change as needed.
setwd('/Volumes/Gandrud1TB/eiu/eiu_extracted/')

# Load packages
library(tm)
library(SnowballC)
library(dplyr)
library(kernlab)
library(stringr)
library(lubridate)
library(ggplot2)
library(gridExtra)
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

clean_corpus <- clean_corpus_full[keep_vec]
rm(clean_corpus_full)

date_country <- date_country[keep_vec, ]

# Create string kernels
kernals <- stringdot(type = "spectrum", length = length_spec)

#### Test spectral clustering ##################################################
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
# Number of components
feature_num = 7

# Estimate
kpca_out <- kpca(clean_corpus, kernal = kernals, features = feature_num)

kpca_df <- pcv(kpca_out) %>% as.data.frame
names(kpca_df) <- sprintf('C%s', 1:feature_num)

results_kpca <- data.frame(date_country, kpca_df, stirngsAsFactors = F) %>%
                    arrange(country, date) %>% select(-stirngsAsFactors)

#### Save ####
export(results_kpca,
       file = '~/git_repositories/EIUCrisesMeasure/data/results_kpca_raw.csv')

#### Flip scale, rescale, and smooth ####
# Temporary
results_kpca <- import('/git_repositories/EIUCrisesMeasure/data/results_kpca.csv')
results_kpca$date <- ymd(results_kpca$date)

# Function to rescale between 0 and 1
range01 <- function(x){(x - min(x))/(max(x) - min(x))}

# Components vector
components_names <- names(results_kpca)[grep('^C[1-9]', names(results_kpca))]

# Flip scale
for (i in components_names) {
    results_kpca[, i] <- results_kpca[, i] * -1
    results_kpca[, i] <- range01(results_kpca[, i])
}

#### Plot results ####
kpca_plotter <- function(indvidual, data = results_kpca){
    temp_data <- subset(data, country == indvidual)
    indv <- ggplot(temp_data, aes(date, C1, group = country)) +
                geom_line(alpha = 0.3) +
                stat_smooth(se = F, colour = 'black') +
                geom_hline(yintercept = 0, linetype = 'dotted') +
                scale_y_continuous(limits = c(0, 1),
                                   breaks = c(0, 0.25, 0.5, 0.75, 1)) +
                xlab('') + ggtitle(indvidual) +
                ylab('') +
                theme_bw()
    return(indv)
}

kpca_list <- list()
for (i in unique(results_kpca$country)[50:60]) {
    message(i)
    kpca_list[[i]] <- suppressMessages(kpca_plotter(indvidual = i))
}

do.call(grid.arrange, kpca_list)

# Find change points
devtools::source_url('https://raw.githubusercontent.com/christophergandrud/FedChangePointNote/master/paper/source/e.divGG.R')

kpca_changepoint <- list()
for (i in unique(date_country$country)) {
    message(i)
    temp_data <- subset(results_kpca, country == i)
    temp_data$C1 <- temp_data$C1 * -1
    temp_plot <- e.divGG(data = temp_data, Vars = 'C1',
                                     TimeVar = 'date', min.size = 6) +
                                ggtitle(i)
    kpca_changepoint[[i]] <- temp_plot
}

do.call(grid.arrange, kpca_changepoint)

# Scree plot to examine model fit
kpca_eigen <- eig(kpca_out)
eigen_plot <- data.frame(components = 1:feature_num, eigenvalues = kpca_eigen)
plot(eigen_plot[, 1], eigen_plot[, 2], type = 'o')
