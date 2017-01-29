# ---------------------------------------------------------------------------- #
# Compare Kernal PCA to Stem Frequency
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Load packages
library(simpleSetup)

# Requires devtools::install_github('christophergandrud/quanteda')

pkgs <- c('parallel', 'randomForestSRC', 'repmis', 'stringr')
library_install(pkgs)

# Set number of coures for random forests.
cores_custom <- detectCores() - 1
# Note randomForestSRC must be correctly configured
options(rf.cores = cores_custom, mc.cores = cores_custom)

# Set working directory. Change as needed.
possible_dir <- c('/git_repositories/EIUCrisesMeasure/',
                  '~/git_repositories/EIUCrisesMeasure/')
simpleSetup::set_valid_wd(possible_dir)

# Run set up script
source('source/pca_kpca/setup/setup.R')

# Function to count the number of words in a string
wordcount <- function(x) sapply(gregexpr("\\W+", x), length) + 1

# Run set up script
source('source/pca_kpca/setup/setup.R')

# PCA bag-of-words scaling -----------------------------------------------------

# Convert corpus to a data frame that is useable by princomp
eiu_df <- as.data.frame(eiu_list)
eiu_df <- gather(eiu_df, id, text)

eiu_dfm <- eiu_df %>% corpus %>% dfm

####
# Remove sparse terms
eiu_dfm <- dfm_trim(eiu_dfm, sparsity = 0.9)
####

eiu_dfm_df <- quanteda::convert(eiu_dfm, to = 'tm')
term_freq <- as.data.frame(as.matrix(eiu_dfm_df))

## Uniquely identify country and date var names
country_date <- rename(country_date, x_country = country)
country_date <- rename(country_date, x_date = date)

term_freq <- cbind(country_date, term_freq)

#### Load KPCA results ####
kpca <- import('source/pca_kpca/raw_data_output/5_strings/results_kpca_5_rescaled.csv')

# Reverse the scale so that low values indicate less perceived stress
kpca$C1_ma <- 1 - kpca$C1_ma

# Create matching corpus
kpca_included <- kpca %>% select(date, country)
names(kpca_included) <- c('x_date', 'x_country')
term_freq <- merge(kpca_included, term_freq,
                   by = c('x_country', 'x_date'),
                   all.x = T) %>%
                select(-x_country, -x_date, -iso3c)

#### Combine ####
cor_pca <- function(var) {
    temp_function <- function(x) cor(kpca[, var], x)
    corrs <- base::apply(term_freq, 2, temp_function)
    corrs <- data.frame(terms = names(term_freq), correlations = corrs)
    corrs_sorted <- corrs %>% arrange(desc(correlations))
    return(corrs_sorted)
}

c1_cor <- cor_pca('C1')
c2_cor <- cor_pca('C2')
c3_cor <- cor_pca('C3')

export(c1_cor, file = 'data/C1_stem_correlations.csv')

#### Random forest -------------------------------------------------------------
comb <- cbind(kpca$C1, kpca$C2, term_freq) %>%
            dplyr::rename(C1 = `kpca$C1`) %>%
            dplyr::rename(C2 = `kpca$C2`)

addq <- function(x) paste0("`", x, "`")

form_c1 <- paste('C1 ~', paste(addq(names(term_freq)), collapse = ' + ')) %>%
          as.formula
form_c2 <- paste('C2 ~', paste(addq(names(term_freq)), collapse = ' + ')) %>%
    as.formula

rfsrc_c1 <- rfsrc(form_c1, data = comb, importance = TRUE)
rfsrc_c2 <- rfsrc(form_c2, data = comb, importance = TRUE)

# Plot variable importance
plot.rfsrc(rfsrc_c1, plots.one.page = F)
plot.rfsrc(rfsrc_c2, plots.one.page = F)


# Save results to a data frame
extract_importance <- function(x){
    imp <- x %>% as.data.frame
    imp$stem <- row.names(imp)
    names(imp) <- c('variable_importance', 'word_stem')
    imp <- imp %>% select(word_stem, variable_importance)
    return(imp)
}

imp_c1 <- extract_importance(rfsrc_c1$importance)
imp_c2 <- extract_importance(rfsrc_c2$importance)

# remove possessive
imp_c1 <- subset(imp_c1, word_stem != "'")
imp_c2 <- subset(imp_c2, word_stem != "'")

export(imp_c1, file = 'data/random_forest_var_imp_C1.csv')
export(imp_c2, file = 'data/random_forest_var_imp_C2.csv')


plot.variable(rfsrc_c1)


