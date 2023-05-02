# Generated from scoreLR.Rmd: do not edit by hand

#' Plot ROC curve for selected method
#' @importFrom ggplot2 ggplot geom_line aes
#' @importFrom dplyr full_join
#' @importFrom assertthat assert_that
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @param data Dataset
#' @param method Method to calculate SLR options include c("IgnoreDependence", "StrictIndependentSet", "AverageFeatures", "MultipleKDE")
#' @param num_runs number of runs to run slr results
#' @param alpha transparency parameter from ggplot2
#' @param NUM_SETS number of sets to use for multiple kde method
#' @param seed  Set the seed of R‘s random number generator
#' @returns ROC plot of selected method
#' @export
# only one method at a time works
# alpha is a transparency parameter from ggplot2
plot_slr_roc <- function(data, method = "AverageFeatures", num_runs = 200, 
                         alpha = 0.1, NUM_SETS = 10, seed = NULL) {
  # check that only one method is inputted
  assertthat::assert_that(assertthat::are_equal(length(method), 1))
  
  roc <- lapply(1:num_runs, 
                function(i) {
                  roc <- slr_results(data, unknown = NULL, method = method, 
                                     NUM_SETS = NUM_SETS, seed = seed)[[1]]$ROC_values
                  data.frame(rep = rep(i, nrow(roc)), tpr = roc$tpr, 
                             fpr = roc$fpr)
                  }
                )
  roc1 <- Reduce(function(x, y) {full_join(x,y, by = c("rep", "tpr", "fpr"))}, 
                 roc)
  
  roc1 %>%
    ggplot(aes(x = .data$fpr, y = .data$tpr, group=.data$rep)) + 
    geom_line(alpha=alpha)
  
}

