# Generated from scoreLR.Rmd: do not edit by hand

#' Results of one or more method of computing the SLR
#' @importFrom dplyr filter select_if
#' @importFrom magrittr %>%
#' @importFrom assertthat assert_that has_name
#' @importFrom rlang .data
#' @param data Dataset
#' @param unknown a data frame of scores from an unknown case. Column names should match the variable names set for scores.
#' @param method Method to calculate SLR Method to calculate SLR options include c("IgnoreDependence", "StrictIndependentSet", "AverageFeatures", "MultipleKDE")
#' @param NUM_SETS number of sets to use for multiple kde method
#' @param seed  Set the seed of R‘s random number generator
#' @returns slr results
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
