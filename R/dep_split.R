# Generated from scoreLR.Rmd: do not edit by hand

#' Test/train split
#' @importFrom dplyr group_by summarize select filter mutate n inner_join anti_join
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @description This function creates a column assigning rows to either the training or testing set. Approximately p*100% of the KNM dependencies are assigned to the training set.
#' Note that this assumes that the KM and KNM data include the same sources/dependencies and that KNM data always includes pairs of dependencies in the same order.
#' @param data data frame containing columns named `source1`, `source2`, `dep1`, and `dep2`. If `source1==source2`, this should indicate a known match (KM) pair, and if `source1!=source2`, this should indicate a known non-match (KNM) pair. The columns `dep1` and `dep2` should indicate dependence that you'd like the training/testing split to respect.
#' @param p training/testing split percentage. Should be between 0 and 1. 
#' @param seed seed for R's random number generator.
#' @returns a data frame with the same columns as the input `data` data frame with an additional `train` column. 
#' @examples 
#' head(dep_split(shoedata, .75))
#' @export
dep_split <- function(data, p, seed=NULL) {
  KM_df <- data %>%
    filter(.data$source1 == .data$source2)
  KNM_df <- data %>%
    filter(.data$source1 != .data$source2)
  
  # Get KNM pairs of dependencies
  knm_pairs <- KNM_df %>%
    group_by(.data$dep1, .data$dep2) %>%
    summarize(n= n(), .groups="drop") %>%
    select(-n)
  
  # Sample p*100% of indices of knm_pairs 
  if (p > .5) {
    size <- floor(nrow(knm_pairs)*p)
  } else {
    size <- ceiling(nrow(knm_pairs)*p)
  }
  
  incl_indices <- sample.int(nrow(knm_pairs), size = size)
  # Note: this may not result in p*100% of sources put into training data
  #       Does not take into account data with different sampling of KNM pairs
  #       (e.g. all KNM pairs)
  
  # Get the sources corresponding to the indices
  incl_sources <- c(knm_pairs[incl_indices,]$dep1, knm_pairs[incl_indices,]$dep2)
  
  # KM pairs
  KM_train <- KM_df %>% filter(.data$dep1 %in% incl_sources)
  KM_test <- KM_df %>% filter(!(.data$dep1 %in% incl_sources))
  
  # KNM pairs
  KNM_train <- KNM_df %>%
    inner_join(knm_pairs[incl_indices,], by = c("dep1", "dep2"))
  KNM_test <- KNM_df %>%
    anti_join(knm_pairs[incl_indices,], by = c("dep1", "dep2"))
  
  # if (nrow(KM_train)==0) {
  #   warning("KM training set has 0 rows. You may want to re-run dep_split().")
  # }
  # if (nrow(KNM_train)==0) {
  #   warning("KNM training set has 0 rows. You may want to re-run dep_split().")
  # }
  # if (nrow(KM_test)==0) {
  #   warning("KM test set has 0 rows. You may want to re-run dep_split().")
  # }
  # if (nrow(KNM_test)==0) {
  #   warning("KNM test set has 0 rows. You may want to re-run dep_split().")
  # }
  
  training <- rbind(KM_train, KNM_train) %>%
    mutate(train = TRUE)
  
  testing <- rbind(KM_test, KNM_test) %>%
    mutate(train = FALSE)
  
  
  return(rbind(training, testing))

}
