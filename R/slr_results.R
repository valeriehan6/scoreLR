# Generated from scoreLR.Rmd: do not edit by hand

#' Results of one or more method of computing the SLR
#' @importFrom dplyr filter select_if
#' @importFrom magrittr %>%
#' @importFrom assertthat assert_that has_name
#' @importFrom rlang .data
#' @description 
#' This function returns the results of one or more method of computing the SLR using the method functions in the package.
#' @param data data frame containing columns named `source1`, `source2`, `dep1`,  `dep2`, and `train`. The `train` column should be a logical vector with `TRUE` denoting the training set and `FALSE` the test set. Any remaining columns must be scores. There may be up to five scores. 
#' @param unknown data frame of scores from an unknown case. Column names should match the variable names set for scores.
#' @param method vector of methods to calculate the SLR. Options include "IgnoreDependence", "StrictIndependentSet", "AverageFeatures", and "MultipleKDE".
#' @param NUM_SETS number of sets to use for multiple KDE method.
#' @param seed  seed for R's random number generator.
#' @returns A list of the output for each of the methods in `method`. Each element of the list is a list containing the following components:
#' \item{KM_SLR}{SLRs for `KM_test`.}
#' \item{KNM_SLR}{SLRs for `KNM_test`.}
#' \item{threshold}{optimal threshold calculated using `KM_train` and `KNM_train`.}
#' \item{new_SLR}{SLRs for `unknown`.}
#' \item{ROC_values}{data frame containing columns the true positive rate `tpr` and false positive rate `fpr` computed using `KM_test` and `KNM_test`.}
#' @examples 
#' # Set up
#' shoedata_split <- dep_split(shoedata, 0.75)
#' unknown <- data.frame(clique_size = c(5, 8), med_dist_euc = c(1.9, 1.1), 
#'                       input_overlap = c(.01, 0.26))
#' 
#' res_all <- slr_results(shoedata_split, unknown) # all methods
#' res_def <- slr_results(shoedata_split, unknown,
#'             c("IgnoreDependence", "StrictIndependentSet")) # default methods
#' res_prop <- slr_results(shoedata_split, unknown, 
#'             c("AverageFeatures", "MultipleKDE")) # proposed methods
#' @seealso [ignore_dep()] for "IgnoreDependence" method, [strict_indep_set()] for "StrictIndependentSet" method, [avg_features()] for "AverageFeatures" method, and [multiple_kde()] for "MultipleKDE" method.
#' @export
slr_results <- function(data, unknown = NULL,
                        method = c("IgnoreDependence", "StrictIndependentSet", 
                                   "AverageFeatures", "MultipleKDE"), 
                        NUM_SETS = 10, seed = NULL) {

  assertthat::assert_that(assertthat::has_name(data, c("source1", "dep1", "source2", "dep2")))
  
  if(!("train" %in% colnames(data))){
    data$train <- rep(TRUE, nrow(data))
  }
  
  assertthat::assert_that(ncol(data) <= 10, msg = "Error: the number of score columns must be less than or equal to 5")
  assertthat::assert_that(ncol(data %>% select(-c(.data$source1, .data$dep1, .data$source2, .data$dep2, .data$train))) == ncol(data %>% select(-c(.data$source1, .data$dep1, .data$source2, .data$dep2, .data$train)) %>% select_if(is.numeric)), msg = "Error: at least one of the score columns is not numeric")
  
  # match.arg to select method
  method1 <- match.arg(method, choices = c("IgnoreDependence", "StrictIndependentSet",
                                          "AverageFeatures", "MultipleKDE"), 
                      several.ok = TRUE) # should there be error if one method is ok and one isn't?
  
  if(length(method) != length(method1)){warning("At least one of the methods is not recognized")}
  
  ## KM
  KM_train <- data %>% filter(.data$source1 == .data$source2 & .data$train == TRUE) 
  KM_test <- data %>% filter(.data$source1 == .data$source2 & .data$train == FALSE) 
  
  ## KNM
  # Filter data to rows where source1 != source2
  KNM_train <- data %>% filter(.data$source1 != .data$source2 & .data$train == TRUE)
  KNM_test <- data %>% filter(.data$source1 != .data$source2 & .data$train == FALSE)
  
  output <- list()
  
  if("IgnoreDependence" %in% method1){
    output$IgnoreDependence <- ignore_dep(KM_train, KM_test, KNM_train, 
                                          KNM_test, unknown)
  }
  if("StrictIndependentSet" %in% method1){
    output$StrictIndependentSet <- strict_indep_set(KM_train, KM_test, KNM_train, KNM_test, 
                                                    unknown=unknown, seed=seed)
  }
  if("AverageFeatures" %in% method1){
    output$AverageFeatures <- avg_features(KM_train, KM_test, KNM_train, 
                                           KNM_test, unknown)
  }
  if("MultipleKDE" %in% method1){
    output$MultipleKDE <- multiple_kde(KM_train, KM_test, KNM_train, 
                                       KNM_test, NUM_SETS, unknown)
  }
  
  return(output)
}
