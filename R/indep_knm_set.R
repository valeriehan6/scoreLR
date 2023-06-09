# Generated from scoreLR.Rmd: do not edit by hand

#' Independent known non match set
#' @importFrom dplyr group_by count arrange 
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @description This function returns an independent set of known non matches (KNM) using `seed`.
#' @param KNM_train known non matches (KNM) from training dataset.
#' @param seed seed for R's random number generator.
#' @returns dataframe with same columns as `KNM_train` only containing an independent KM set of the training data.
indep_knm_set <- function(KNM_train, seed=NULL) {
  set.seed(seed)
  
  # NOTE: indep not generalized to other KNM samples
  nsources <- KNM_train %>% group_by(.data$source1, .data$source2) %>% count() %>% nrow()
  
  npairs <- (KNM_train %>% group_by(.data$source1, .data$source2) %>% count())$n
  
  sorted_KNM_train <- KNM_train %>%
    arrange(.data$source1, .data$source2) 
  
  
  KNM_rows <- rep(NA, nsources)
  start_index <- 1
  
  for (source in 1:nsources) {
    pair <- sample.int(npairs[source], 1)
    
    KNM_rows[source] <- pair-1 + start_index
    
    start_index <- start_index + npairs[source]
  }
  
  indep_KNM_pairs <- sorted_KNM_train[KNM_rows, ]
  
  return(indep_KNM_pairs)
}

