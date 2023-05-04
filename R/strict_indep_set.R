# Generated from scoreLR.Rmd: do not edit by hand

#' Strict independent set
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom ks Hpi hpi kde
#' @importFrom ROCR prediction performance
#' @importFrom rlang .data
#' @description 
#' This function estimates the SLR function using the strict independent set method, computing the numerator and denominator density using only an independent set from `KM_train` and `KNM_train`, respectively. The function then produces various output based on `KM_test`, `KNM_test`, and `unknown`.
#' 
#' `KM_train`, `KM_test`, `KNM_train`, `KNM_train` should all be data frames containing columns named `source1`, `source2`, `dep1`, and `dep2`. Any remaining columns must be scores. There may be up to five scores. 
#' @param KM_train known matches (KM) from training dataset.
#' @param KM_test known matches (KM) from testing dataset.
#' @param KNM_train known non matches (KNM) from training dataset.
#' @param KNM_test known non matches (KNM) from testing dataset.
#' @param unknown data frame of scores from an unknown case. Column names should match the variable names set for scores.
#' @param seed seed for R's random number generator.
#' @returns A list containing the following components:
#' \itemize{
#'   \item KM_SLR - SLRs for `KM_test`.
#'   \item KNM_SLR - SLRs for `KNM_test`.
#'   \item threshold - optimal threshold calculated using `KM_train` and `KNM_train`.
#'   \item new_SLR - SLRs for `unknown`.
#'   \item ROC_values - data frame containing columns the true positive rate `tpr` and false positive rate `fpr` computed using `KM_test` and `KNM_test`.
#' }
#' @examples 
#' library(dplyr)
#' # Set up data
#' shoedata_split <- dep_split(shoedata, 0.75)
#' KM_train <- shoedata_split %>% filter(source1 == source2 & train == TRUE)
#' KM_test <- shoedata_split %>% filter(source1 == source2 & train == FALSE)
#' KNM_train <- shoedata_split %>% filter(source1 != source2 & train == TRUE)
#' KNM_test <- shoedata_split %>% filter(source1 != source2 & train == FALSE)
#' unknown <- data.frame(clique_size = c(5, 8), med_dist_euc = c(1.9, 1.1), 
#'                       input_overlap = c(.01, 0.26))
#' 
#' res <- strict_indep_set(KM_train, KM_test, KNM_train, KNM_test, unknown)
#' @export
strict_indep_set <- function(KM_train, KM_test, KNM_train, KNM_test, 
                             unknown=NULL, seed=NULL) {
  
  assertthat::assert_that(assertthat::has_name(KM_train, c("source1", "dep1", "source2", "dep2")))
  assertthat::assert_that(assertthat::has_name(KM_test, c("source1", "dep1", "source2", "dep2")))
  assertthat::assert_that(assertthat::has_name(KNM_train, c("source1", "dep1", "source2", "dep2")))
  assertthat::assert_that(assertthat::has_name(KNM_test, c("source1", "dep1", "source2", "dep2")))
  
  if(!("train" %in% colnames(KM_train))){
    KM_train$train <- rep(TRUE, nrow(KM_train))
  }
  
  if(!("train" %in% colnames(KM_test))){
    KM_test$train <- rep(FALSE, nrow(KM_test))
  }
  
  if(!("train" %in% colnames(KNM_train))){
    KNM_train$train <- rep(TRUE, nrow(KNM_train))
  }
  
  if(!("train" %in% colnames(KNM_test))){
    KNM_test$train <- rep(FALSE, nrow(KNM_test))
  }
  
  KM_train <- indep_km_set(KM_train, seed=seed)
  KNM_train <- indep_knm_set(KNM_train, seed=seed)
  
  # KM
  SURF_KM <- KM_train %>%
    select(-c(.data$source1, .data$dep1, .data$source2, .data$dep2, .data$train)) %>%
    data.matrix()
  
  SURF_KM_test <- KM_test %>%
    select(-c(.data$source1, .data$dep1, .data$source2, .data$dep2, .data$train)) %>%
    data.matrix()
  
  # KNM 
  SURF_KNM <- KNM_train %>%
    select(-c(.data$source1, .data$dep1, .data$source2, .data$dep2, .data$train)) %>%
    data.matrix()
  
  SURF_KNM_test <- KNM_test %>%
    select(-c(.data$source1, .data$dep1, .data$source2, .data$dep2, .data$train)) %>%
    data.matrix()
  
  # Make sure column names match
  assertthat::assert_that(assertthat::are_equal(colnames(SURF_KM), colnames(SURF_KM_test)), 
              assertthat::are_equal(colnames(SURF_KM), colnames(SURF_KNM)), 
              assertthat::are_equal(colnames(SURF_KM), colnames(SURF_KNM_test))) 
  
  
  # Fit the bandwidth
  if(ncol(SURF_KM) == 1){
    Hpi_SURF_KM <- hpi(x=SURF_KM)
  }else{
    Hpi_SURF_KM <- Hpi(x=SURF_KM)
  }
  
  if(ncol(SURF_KNM) == 1){
    Hpi_SURF_KNM <- hpi(x=SURF_KNM)
  }else{
    Hpi_SURF_KNM <- Hpi(x=SURF_KNM)
  }
  
  # Calculating SLRs for Testing Data
  if (nrow(SURF_KM_test) > 0) {
    ## KM numerator and denominator for SLR
    KM_num <- kde(x=SURF_KM, H=Hpi_SURF_KM, eval.points = SURF_KM_test)$estimate
    KM_denom <- kde(x=SURF_KNM, H=Hpi_SURF_KNM, eval.points = SURF_KM_test)$estimate
    KM_SLR_test <- data.frame(KM_num/KM_denom)
    names(KM_SLR_test) <- c("SLR")
  } else {
    KM_SLR_test = NA
  }
   
  if (nrow(SURF_KNM_test) > 0) {
    ## KNM numerator and denominator for SLR
    KNM_num <- kde(x=SURF_KM, H=Hpi_SURF_KM, eval.points = SURF_KNM_test)$estimate
    KNM_denom <- kde(x=SURF_KNM, H=Hpi_SURF_KNM, eval.points = SURF_KNM_test)$estimate
    KNM_SLR_test <- data.frame(KNM_num/KNM_denom)
    names(KNM_SLR_test) <- c("SLR")
  } else {
    KNM_SLR_test = NA
  }
  
  
  # Calculating ROC curve for Testing Data
  if ((nrow(SURF_KM_test) > 0) && (nrow(SURF_KNM_test) > 0)) {
    SLR_test <- rbind(KM_SLR_test, KNM_SLR_test)
    labels <- c(rep("KM", nrow(KM_SLR_test)), rep("KNM", nrow(KNM_SLR_test)))
    pred <- prediction(SLR_test, labels)
    roc <- performance(pred, "tpr", "fpr")
    roc_df <- data.frame(tpr = roc@x.values[[1]], 
                         fpr = roc@y.values[[1]])
  } else {
    roc_df = NA
  }
   
  
  # Calculating Threshold for Classification (using only training data)
  
  # KM numerator and denominator for SLR
  KM_num <- kde(x=SURF_KM, H=Hpi_SURF_KM, eval.points = SURF_KM)$estimate
  KM_denom <- kde(x=SURF_KNM, H=Hpi_SURF_KNM, eval.points = SURF_KM)$estimate
  KM_SLR_train <- data.frame(KM_num/KM_denom)
  names(KM_SLR_train) <- c("SLR")
  
  # KNM numerator and denominator for SLR
  KNM_num <- kde(x=SURF_KM, H=Hpi_SURF_KM, eval.points = SURF_KNM)$estimate
  KNM_denom <- kde(x=SURF_KNM, H=Hpi_SURF_KNM, eval.points = SURF_KNM)$estimate
  KNM_SLR_train <- data.frame(KNM_num/KNM_denom)
  names(KNM_SLR_train) <- c("SLR")
  
  thresh <- opt_thresh(KM_SLR_train, KNM_SLR_train)
  
  # Results - outputted
  
  # Calculating SLR for Unknown Case - outputted
  if (!is.null(unknown)) {
    scores <- colnames(SURF_KM)
    new_cases <- unknown %>% 
      select(all_of(scores)) %>%
      data.matrix()
    
    num <- kde(x=SURF_KM, H=Hpi_SURF_KM, eval.points = new_cases)$estimate
    denom <- kde(x=SURF_KNM, H=Hpi_SURF_KNM, eval.points = new_cases)$estimate
    new_cases_SLR <- data.frame(num/denom)
    names(new_cases_SLR) <- c("SLR")
  } else {
    new_cases_SLR <- NA
  }
  
  
  # Output
  output <- list("KM_SLR" = KM_SLR_test, "KNM_SLR" = KNM_SLR_test,
                 "threshold" = thresh, "new_SLR" = new_cases_SLR,
                 "ROC_values" = roc_df)
  
  return(output)
}
