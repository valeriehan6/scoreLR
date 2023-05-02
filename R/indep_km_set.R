# Generated from scoreLR.Rmd: do not edit by hand

#' independent known match set
#' @importFrom dplyr group_by arrange count
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @param KM_train Known matches from training dataset
#' @param seed Random seed
#' @returns independent KM set of the training data using `seed`
#' Returns an independent KM set of the training data using `seed`
indep_km_set <- function(KM_train, seed=NULL) {
  set.seed(seed)
  
  nsources <- KM_train %>% group_by(.data$source1) %>% count() %>% nrow()
  
  npairs <- (KM_train %>% group_by(.data$source1) %>% count())$n
  
  sorted_KM_train <- KM_train %>%
    arrange(.data$source1) 
  
  KM_rows <- rep(NA, nsources)
  start_index <- 1
  
  for (source in 1:nsources) {
    pair <- sample.int(npairs[source], 1)
    
    KM_rows[source] <- pair-1 + start_index
    
    start_index <- start_index + npairs[source]
  }
  
  indep_KM_pairs <- sorted_KM_train[KM_rows, ]
  
  return(indep_KM_pairs)
}

