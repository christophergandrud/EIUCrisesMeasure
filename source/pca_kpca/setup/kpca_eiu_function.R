#' Function to conduct kernel principal component analysis on EIU texts
#' and process the results
#'
#' @param corpus a list where each element is a character vector EIU document.
#' @param country_date a data frame with three columns named \code{country},
#' \code{iso3c}, and \code{date} identifying each document in \code{corpus}.
#' Note: must be in the same order and of the same length as \code{corpus}.
#' Date must be in YYYY-MM-DD format.
#' @param length_spec an integer specifying the number of characters in each
#' kernel string.
#' @param feature_num an integer specifying the number of components.
#' @param n_period an integer specifying the number of periods to create 
#' moving averages of the first component.
#' @param out_dir a character string specifying where to save the data outputs.

kpca_eiu <- function(corpus, country_date, length_spec = 5, feature_num = 10,
                     n_period = 2, out_dir)
{
    if (missing(out_dir)) stop('A directory to save the outputs in must be specified.',
                               call. = FALSE)
    
    if (length(corpus) != nrow(country_date)) 
        stop('corpus and country_date must have the same number of observations',
             call. = FALSE)
    
    # Create string kernels
    kernels <- stringdot(type = "spectrum", length = length_spec)

    # Run KPCA
    message('Running KPCA . . .')
    microbenchmark(
        kpca_out <- kpca(corpus, kernel = kernels, features = feature_num)
    )

    # Extract features ---------------------------------------------------------
    kpca5_df <- pcv(kpca_out) %>% as.data.frame
    names(kpca5_df) <- sprintf('C%s', 1:feature_num)

    results_kpca5 <- data.frame(country_date, kpca5_df, 
                                stirngsAsFactors = FALSE) %>%
                        arrange(country, date) %>% select(-stirngsAsFactors)

    # Save raw components
    dir_raw_component <- sprintf('%s/results_kpca_%s_raw.csv', out_dir, length_spec)
    export(results_kpca5, file = dir_raw_component)

    #### Flip scale, rescale, and smooth ---------------------------------------
    message('Rescaling . . .')
    results_kpca5$date <- ymd(results_kpca5$date)

    # Function to rescale between 0 and 1
    range01 <- function(x){(x - min(x))/(max(x) - min(x))}

    # Components vector
    components_names <- names(results_kpca5)[grep('^C[1-9]', 
                                                  names(results_kpca5))]

    # Transform Scale
    for (i in components_names) {
        results_kpca5[, i] <- range01(results_kpca5[, i])
    }

    # Find previous periods moving average
    sma_mod <- function(x) SMA(x, n = n_period)
    results_kpca5 <- results_kpca5 %>% group_by(country) %>%
        mutate(C1_ma = sma_mod(C1))

    # Save rescaled index (e.g. FinStress)
    dir_rescaled <- sprintf('%s/results_kpca_%s_rescaled.csv', out_dir, length_spec)
    export(results_kpca5,
           file = dir_rescaled)

    # Scree plot to examine model fit
    message('Eigenvalues . . .')
    kpca5_eigen <- eig(kpca_out)
    eigen_plot <- data.frame(components = 1:feature_num, 
                             eigenvalues = kpca5_eigen)

    dir_eigen <- sprintf('%s/kpca_%s_eigen_%s.csv', out_dir, length_spec, 
                            feature_num)
    export(eigen_plot, file = dir_eigen)

    plot(eigen_plot[, 1], eigen_plot[, 2], type = 'o')
}
