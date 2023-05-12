# Generated from scoreLR.Rmd: do not edit by hand

#' Average features
#' @importFrom dplyr group_by summarize select all_of n across
#' @importFrom tidyselect where
#' @importFrom magrittr %>%
#' @importFrom ROCR prediction performance
#' @importFrom rlang .data
#' @description 
#' This function estimates the SLR function using the average features method, averaging each score over pairs of sources and computing kernel density estimation (KDE) on averaged features. The densities are estimated with `KM_train` and `KNM_train` and the function produces various output based on `KM_test`, `KNM_test`, and `unknown`.
#' 
#' `KM_train`, `KM_test`, `KNM_train`, `KNM_train` should all be data frames containing columns named `source1`, `source2`, `dep1`, and `dep2`. Any remaining columns must be scores. There may be up to five scores. 
#' @param KM_train known matches (KM) from training dataset.
#' @param KM_test known matches (KM) from testing dataset.
#' @param KNM_train known non matches (KNM) from training dataset.
#' @param KNM_test known non matches (KNM) from testing dataset.
#' @param unknown data frame of scores from an unknown case. Column names should match the variable names set for scores.
#' @returns A list containing the following components:
#' \item{KM_SLR}{SLRs for `KM_test`.}
#' \item{KNM_SLR}{SLRs for `KNM_test`.}
#' \item{threshold}{optimal threshold calculated using `KM_train` and `KNM_train`.}
#' \item{new_SLR}{SLRs for `unknown`.}
#' \item{ROC_values}{data frame containing columns the true positive rate `tpr` and false positive rate `fpr` computed using `KM_test` and `KNM_test`.}
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
#' res <- avg_features(KM_train, KM_test, KNM_train, KNM_test, unknown)
#' @export
avg_features <- function(KM_train, KM_test, KNM_train, KNM_test, unknown = NULL) {
  
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
  ## Create average features df
  avg_KM_train <- KM_train %>%
    group_by(.data$source1) %>% 
    summarize(across(-c("dep1", "source2", "dep2", "train"), mean), 
            n = n(), .groups="drop")
  
  avg_KM_test <- KM_test %>%
    group_by(.data$source1) %>% 
    summarize(across(-c("dep1", "source2", "dep2", "train"), mean), 
            n = n(), .groups="drop")
  
  SURF_KM <- avg_KM_train %>%
    select(-c("source1", "n")) %>% 
    data.matrix()
  
  SURF_KM_test <- avg_KM_test %>%
    select(-c("source1", "n")) %>%
    data.matrix()
  
  # KNM
  ## Create average features df
  avg_KNM_train <- KNM_train %>%
    group_by(.data$source1, .data$source2) %>% 
    summarize(across(-c("dep1", "dep2", "train"), mean), 
            n = n(), .groups="drop")
  
  avg_KNM_test <- KNM_test %>%
    group_by(.data$source1, .data$source2) %>% 
    summarize(across(-c("dep1", "dep2", "train"), mean), 
            n = n(), .groups="drop")

  SURF_KNM <- avg_KNM_train %>%
    select(-c("source1", "source2", "n")) %>% 
    data.matrix()
  
  SURF_KNM_test <- avg_KNM_test %>%
    select(-c("source1", "source2", "n")) %>%
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
  ## KM numerator and denominator for SLR
  if (nrow(SURF_KM_test) > 0) {
  KM_num <- kde(x=SURF_KM, H=Hpi_SURF_KM, eval.points = SURF_KM_test,
                  density=T)$estimate
  KM_denom <- kde(x=SURF_KNM, H=Hpi_SURF_KNM, eval.points = SURF_KM_test,
                  density=T)$estimate
  KM_SLR_test <- data.frame(KM_num/KM_denom)
  names(KM_SLR_test) <- c("SLR")
  } else {
    KM_SLR_test = NA
  }
  
  ## KNM numerator and denominator for SLR
  if (nrow(SURF_KNM_test) > 0) {
  KNM_num <- kde(x=SURF_KM, H=Hpi_SURF_KM, eval.points = SURF_KNM_test,
                  density=T)$estimate
  KNM_denom <- kde(x=SURF_KNM, H=Hpi_SURF_KNM, eval.points = SURF_KNM_test,
                  density=T)$estimate
  KNM_SLR_test <- data.frame(KNM_num/KNM_denom)
  names(KNM_SLR_test) <- c("SLR")
  } else {
    KNM_SLR_test = NA
  }
  
  # Calculating ROC curve for Testing Data
  if ((nrow(SURF_KM_test) > 0) && (nrow(SURF_KNM_test) > 0)) {
    SLR_test <- rbind(KM_SLR_test, KNM_SLR_test)
    labels <- c(rep("KM", nrow(KM_SLR_test)), rep("KNM", nrow(KNM_SLR_test)))
    
    if (length(unique(labels)) != 2) warning("labels = ", (unique(labels)))
    
    roc_df <- tryCatch({
      pred <- prediction(SLR_test, labels)
      roc <- performance(pred, "tpr", "fpr")
      data.frame(tpr = roc@x.values[[1]], fpr = roc@y.values[[1]])
      }, error = function(e) {
               NA
               print(paste0("ROC_values not calculated", 
                     nrow(SURF_KM_test), ", ", nrow(SURF_KNM_test)))
             })
  } else {
    print(paste0("ROC_values not calculated", 
                 nrow(SURF_KM_test), ", ", nrow(SURF_KNM_test)))
    roc_df <- NA
  }
  
  # Calculating Threshold for Classification (using only training data)
  
  # KM numerator and denominator for SLR
  KM_num <- kde(x=SURF_KM, H=Hpi_SURF_KM, eval.points = SURF_KM,
                  density=T)$estimate
  KM_denom <- kde(x=SURF_KNM, H=Hpi_SURF_KNM, eval.points = SURF_KM,
                  density=T)$estimate
  KM_SLR_train <- data.frame(KM_num/KM_denom)
  names(KM_SLR_train) <- c("SLR")
  
  # KNM numerator and denominator for SLR
  KNM_num <- kde(x=SURF_KM, H=Hpi_SURF_KM, eval.points = SURF_KNM,
                  density=T)$estimate
  KNM_denom <- kde(x=SURF_KNM, H=Hpi_SURF_KNM, eval.points = SURF_KNM,
                  density=T)$estimate
  KNM_SLR_train <- data.frame(KNM_num/KNM_denom)
  names(KNM_SLR_train) <- c("SLR")
  
  thresh <- opt_thresh(KM_SLR_train, KNM_SLR_train)
  
  # Calculating SLR for Unknown Case - outputted
  if (!is.null(unknown)) {
    scores <- colnames(SURF_KM)
    new_cases <- unknown %>% 
      select(all_of(scores)) %>%
      data.matrix()
    
    num <- kde(x=SURF_KM, H=Hpi_SURF_KM, eval.points = new_cases,
                  density=T)$estimate
    denom <- kde(x=SURF_KNM, H=Hpi_SURF_KNM, eval.points = new_cases,
                  density=T)$estimate
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
