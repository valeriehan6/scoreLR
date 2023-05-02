# Generated from scoreLR.Rmd: do not edit by hand

#' ignore dependence
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom ks Hpi hpi kde
#' @importFrom ROCR prediction performance
#' @importFrom rlang .data
#' @param KM_train Known matches from training dataset
#' @param KM_test Known matches from testing dataset
#' @param KNM_train Known non matches from training dataset
#' @param KNM_test Known non matches from testing dataset
#' @param unknown a data frame of scores from an unknown case. Column names should match the variable names set for scores.
#' @returns slr evaluation for ignore dependence method
#' @export
ignore_dep <- function(KM_train, KM_test, KNM_train, KNM_test, unknown = NULL) {
  
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
  
  SURF_KNM_test <- KNM_test %>% # this should be test, right?
    select(-c(.data$source1, .data$dep1, .data$source2, .data$dep2, .data$train)) %>%
    data.matrix()
  
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
  # KM numerator and denominator for SLR
  if (nrow(SURF_KM_test) > 0) {
  KM_num <- kde(x=SURF_KM, H=Hpi_SURF_KM, eval.points = SURF_KM_test)$estimate
  KM_denom <- kde(x=SURF_KNM, H=Hpi_SURF_KNM, eval.points = SURF_KM_test)$estimate
  KM_SLR_test <- data.frame(KM_num/KM_denom)
  names(KM_SLR_test) <- c("SLR")
  } else {
    KM_SLR_test = NA
  }
  
  # KNM numerator and denominator for SLR
  if (nrow(SURF_KNM_test) > 0) {
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
