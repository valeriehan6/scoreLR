# Generated from scoreLR.Rmd: do not edit by hand

#' Multiple independent sets
#' @importFrom dplyr arrange group_by count select
#' @importFrom magrittr %>%
#' @importFrom ROCR prediction performance
#' @importFrom rlang .data
#' @description 
#' This function estimates the SLR function using the multiple independent sets method. The kernel density estimate (KDE) of numerator and denominator density is computed on multiple independent sets and the estimated densities are averaged to compute the SLR. The independent sets used to estimate the SLR function are from `KM_train` and `KNM_train`. The function then produces various output based on `KM_test`, `KNM_test`, and `unknown`.
#' 
#' `KM_train`, `KM_test`, `KNM_train`, `KNM_train` should all be data frames containing columns named `source1`, `source2`, `dep1`, and `dep2`. Any remaining columns must be scores. There may be up to five scores. 
#' @param KM_train known matches (KM) from training dataset.
#' @param KM_test known matches (KM) from testing dataset.
#' @param KNM_train known non matches (KNM) from training dataset.
#' @param KNM_test known non matches (KNM) from testing dataset.
#' @param NUM_SETS number of independent sets to use.
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
#' res <- multiple_kde(KM_train, KM_test, KNM_train, KNM_test, 5, unknown)
#' @export
multiple_kde <- function(KM_train, KM_test, KNM_train, KNM_test, NUM_SETS = 10, 
                         unknown = NULL) {
  
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
  
  sorted_KM_features <- KM_train %>%
    arrange(.data$source1) 
  
  nshoes <- KM_train %>% group_by(.data$source1) %>% count() %>% nrow() 
  
  npairs <- (KM_train %>% group_by(.data$source1) %>% count())$n 
  
  ### function to return indices of independent KM pairs
  KM_indep_set <- function() {
    KM_rows <- list()
    start_index <- 1
    
    for (shoe in 1:nshoes) {
      pair <- sample.int(npairs[shoe], 1)
      
      KM_rows[[length(KM_rows)+1]] <- pair-1 + start_index
      
      start_index <- start_index + npairs[shoe]
    }
    
    KM_rows <- unlist(KM_rows)
    
    return(KM_rows)
  }
  
  KM_indep_sets <- replicate(NUM_SETS, KM_indep_set(), simplify=FALSE)
  
  sorted_KNM_features <- KNM_train %>%
    arrange(.data$source1, .data$source2) 
  
  nshoes <- KNM_train %>% group_by(.data$source1, .data$source2) %>% count() %>% nrow() 
  
  npairs <- (KNM_train %>% group_by(.data$source1, .data$source2) %>% count())$n 

  
  ### function to return indices of independent KNM pairs
  KNM_indep_set <- function() {
    KNM_rows <- list()
    start_index <- 1
    
    for (shoe in 1:nshoes) {
      pair <- sample.int(npairs[shoe], 1)
      
      KNM_rows[[length(KNM_rows)+1]] <- pair-1 + start_index
      
      start_index <- start_index + npairs[shoe]
    }
    
    KNM_rows <- unlist(KNM_rows)
    
    return(KNM_rows)
  }
  
  KNM_indep_sets <- replicate(NUM_SETS, KNM_indep_set(), simplify=FALSE)
  
  # Calculating SLRs for Testing Data
  ## need to store result
  if (nrow(KM_test) > 0) {
    KM_nums <- list()
    KM_denoms <- list()
  } else {
    KM_SLR_test <- NA
  }
  
  if (nrow(KNM_test) > 0) {
    KNM_nums <- list()
    KNM_denoms <- list()
  } else {
    KNM_SLR_test <- NA
  }
  
  KM_nums_train <- list()
  KM_denoms_train <- list()
  KNM_nums_train <- list()
  KNM_denoms_train <- list()
  
  if (!is.null(unknown)) {
    new_nums <- list()
    new_denoms <- list()
  } else {
    new_cases_SLR <- NA
  }
  
  
  for (i in 1:NUM_SETS) {
    KM_train <- sorted_KM_features[KM_indep_sets[[i]],]
    KNM_train <- sorted_KNM_features[KNM_indep_sets[[i]],]
    
    SURF_KM <- KM_train %>%
      select(-c("source1", "dep1", "source2", "dep2", "train")) %>% 
      data.matrix()
    SURF_KNM <- KNM_train %>%
      select(-c("source1", "dep1", "source2", "dep2", "train")) %>% 
      data.matrix()
    SURF_KM_test <- KM_test %>%
        select(-c("source1", "dep1", "source2", "dep2", "train")) %>% 
        data.matrix()
    SURF_KNM_test <- KNM_test %>%
        select(-c("source1", "dep1", "source2", "dep2", "train")) %>% 
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
    
    # Calculating numerators and denominators for Testing data
    if (nrow(KM_test) > 0) {
      
      KM_num <- kde(x=SURF_KM, H=Hpi_SURF_KM, eval.points = SURF_KM_test,
                  density=T)$estimate
      KM_denom <- kde(x=SURF_KNM, H=Hpi_SURF_KNM, eval.points = SURF_KM_test,
                  density=T)$estimate
      KM_nums[[length(KM_nums) + 1]] <- KM_num
      KM_denoms[[length(KM_denoms) + 1]] <- KM_denom
    } 
    if (nrow(KNM_test) > 0) {
      
      KNM_num <- kde(x=SURF_KM, H=Hpi_SURF_KM, eval.points = SURF_KNM_test,
                  density=T)$estimate
      KNM_denom <- kde(x=SURF_KNM, H=Hpi_SURF_KNM, eval.points = SURF_KNM_test,
                  density=T)$estimate
      KNM_nums[[length(KNM_nums) + 1]] <- KNM_num
      KNM_denoms[[length(KNM_denoms) + 1]] <- KNM_denom
    }
    
    
    # Calculating Threshold for Classification (using only training data)
    
    # KM numerator and denominator for SLR
    KM_num <- kde(x=SURF_KM, H=Hpi_SURF_KM, eval.points = SURF_KM,
                  density=T)$estimate
    KM_denom <- kde(x=SURF_KNM, H=Hpi_SURF_KNM, eval.points = SURF_KM,
                  density=T)$estimate
    
    # KNM numerator and denominator for SLR
    KNM_num <- kde(x=SURF_KM, H=Hpi_SURF_KM, eval.points = SURF_KNM,
                  density=T)$estimate
    KNM_denom <- kde(x=SURF_KNM, H=Hpi_SURF_KNM, eval.points = SURF_KNM,
                  density=T)$estimate
    
    KM_nums_train[[length(KM_nums_train) + 1]] <- KM_num
    KM_denoms_train[[length(KM_denoms_train) + 1]] <- KM_denom
    KNM_nums_train[[length(KNM_nums_train) + 1]] <- KNM_num
    KNM_denoms_train[[length(KNM_denoms_train) + 1]] <- KNM_denom
    
    
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
      
        
      new_nums[[length(new_nums) + 1]] <- num
      new_denoms[[length(new_denoms) + 1]] <- denom
      
    }
    
  }
  
  # Testing data
  if (nrow(KM_test) > 0) {
    avg_KM_nums <- colMeans(do.call(rbind,KM_nums))
    avg_KM_denoms <- colMeans(do.call(rbind,KM_denoms))
    KM_SLR_test <- data.frame(avg_KM_nums/avg_KM_denoms)
    names(KM_SLR_test) <- c("SLR")
  }
  if (nrow(KNM_test) > 0) {
    avg_KNM_nums <- colMeans(do.call(rbind,KNM_nums))
    avg_KNM_denoms <- colMeans(do.call(rbind,KNM_denoms))
    KNM_SLR_test <- data.frame(avg_KNM_nums/avg_KNM_denoms)
    names(KNM_SLR_test) <- c("SLR")
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
               print(paste0("ROC_values not calculated", 
                     nrow(SURF_KM_test), ", ", nrow(SURF_KNM_test)))
        return(NA)
             })
  } else {
    print(paste0("ROC_values not calculated", 
                 nrow(SURF_KM_test), ", ", nrow(SURF_KNM_test)))
    roc_df <- NA
  }
  
  # Training data
  avg_KM_nums_train <- colMeans(do.call(rbind,KM_nums_train))
  avg_KM_denoms_train <- colMeans(do.call(rbind,KM_denoms_train))
  avg_KNM_nums_train <- colMeans(do.call(rbind,KNM_nums_train))
  avg_KNM_denoms_train <- colMeans(do.call(rbind,KNM_denoms_train))
  
  KM_SLR_train <- data.frame(avg_KM_nums_train/avg_KM_denoms_train)
  names(KM_SLR_train) <- c("SLR")
  
  KNM_SLR_train <- data.frame(avg_KNM_nums_train/avg_KNM_denoms_train)
  names(KNM_SLR_train) <- c("SLR")
  thresh <- opt_thresh(KM_SLR_train, KNM_SLR_train)
  
  
  # Calculating SLR for Unknown Case - outputted
  if (!is.null(unknown)) {
    avg_nums <- colMeans(do.call(rbind,new_nums))
    avg_denoms <- colMeans(do.call(rbind,new_denoms))
    
    new_cases_SLR <- data.frame(avg_nums/avg_denoms)
    names(new_cases_SLR) <- c("SLR")
  } 
  
  
  # Output
  output <- list("KM_SLR" = KM_SLR_test, "KNM_SLR" = KNM_SLR_test,
                 "threshold" = thresh, "new_SLR" = new_cases_SLR,
                 "ROC_values" = roc_df)
  
  return(output)
}
