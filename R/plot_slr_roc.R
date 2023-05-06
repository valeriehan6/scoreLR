# Generated from scoreLR.Rmd: do not edit by hand

#' Plot ROC curve for selected method
#' @importFrom ggplot2 ggplot geom_line aes theme_bw
#' @importFrom dplyr full_join
#' @importFrom assertthat assert_that
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @description 
#' This function returns a plot of the ROC curves produced by `num_runs` times of running [slr_results()]. Note that this function only supports plotting one method at a time.
#' @param data data frame containing columns named `source1`, `source2`, `dep1`,  and `dep2`. Any remaining columns must be scores. There may be up to five scores. 
#' @param p training/testing split percentage. Should be between 0 and 1. 
#' @param method string. Options are "IgnoreDependence", "StrictIndependentSet", "AverageFeatures", and "MultipleKDE".
#' @param num_runs number of times to run `slr_results`.
#' @param alpha transparency parameter from `ggplot2` package.
#' @param NUM_SETS number of sets to use for multiple KDE method.
#' @param seed  seed of R‘s random number generator.
#' @returns plot of selected method.
#' @examples 
#' plot_slr_roc(shoedata, p = 0.75, method = "StrictIndependentSet", 
#'              num_runs = 3, alpha = 0.5)
#' @seealso [ignore_dep()] for "IgnoreDependence" method, [strict_indep_set()] for "StrictIndependentSet" method, [avg_features()] for "AverageFeatures" method, and [multiple_kde()] for "MultipleKDE" method. 
#' @export
plot_slr_roc <- function(data, p, method = "AverageFeatures", num_runs = 200, 
                         alpha = 0.1, NUM_SETS = 10, seed = NULL) {
  
  # check that only one method is inputted
  assertthat::assert_that(assertthat::are_equal(length(method), 1))
  
  set.seed(seed)
  
  roc <- lapply(1:num_runs, 
                function(i) {
                  data_split <- dep_split(data, p)
                  roc <- slr_results(data_split, unknown = NULL, method = method, 
                                     NUM_SETS = NUM_SETS)[[1]]$ROC_values
                  
                  tryCatch(data.frame(rep = rep(i, nrow(roc)), tpr = roc$tpr, 
                                      fpr = roc$fpr),
                    #if an error occurs, tell me the error
                    error=function(e) {
                      message('An Error Occurred')
                      print(roc)
                      print(e)
                    },
                    #if a warning occurs, tell me the warning
                    warning=function(w) {
                      message('A Warning Occurred')
                      print(roc)
                      print(w)
                      return(NA)
                    }
                  )
                  
                  }
                )
  roc1 <- Reduce(function(x, y) {full_join(x,y, by = c("rep", "tpr", "fpr"))}, 
                 roc)
  
  roc1 %>%
    ggplot(aes(x = .data$fpr, y = .data$tpr, group=.data$rep)) + 
    geom_line(alpha=alpha) %>%
    return() + 
    theme_bw()
  
}

