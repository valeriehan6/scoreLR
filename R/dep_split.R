# Generated from scoreLR.Rmd: do not edit by hand

#' Test/train split
#' @importFrom dplyr group_by summarize select filter mutate n
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @description This function creates a column assigning rows to either the training or testing set. p*100% of the KNM pairs of dependencies are assigned to the training set.
#' @param data Dataframe containing columns named `source1`, `source2`, `dep1`, and `dep2`. If `source1==source2`, this should indicate a known match (KM) pair, and if `source1!=source2`, this should indicate a known non-match (KNM) pair. The columns `dep1` and `dep2` should indicate dependence that you'd like the training/testing split to respect. 
#' @param p Training/Testing split percentage. Should be between 0 and 1. 
#' @param seed Seed for R's random number generator.
#' @returns A dataframe with the same columns as the input `data` dataframe with an additional `train` column. 
#' @examples 
#' dep_split(shoedata, .75)
#' @export
dep_split <- function(data, p, seed=NULL) {
  KM_df <- data %>%
    filter(.data$source1 == .data$source2)
  KNM_df <- data %>%
    filter(.data$source1 != .data$source2)
  
  # Get KNM pairs of sources
  knm_pairs <- KNM_df %>%
    group_by(.data$dep1, .data$dep2) %>%
    summarize(n= n(), .groups="drop") %>%
    select(-n)

  set.seed(seed)
  
  # Sample p*100% of indices of knm_pairs 
  incl_indices <- sample.int(nrow(knm_pairs), size=round(nrow(knm_pairs)*p))
  # Note: this may not result in p*100% of sources put into training data
  #       Does not take into account data with different sampling of KNM pairs
  #       (e.g. all KNM pairs)
  
  # Get the sources corresponding to the indices
  incl_sources <- c(knm_pairs[incl_indices,]$dep1, knm_pairs[incl_indices,]$dep2)
  
  training <- rbind(KM_df %>%
                      filter(.data$dep1 %in% incl_sources),
                    KNM_df %>%
                      filter(.data$dep1 %in% incl_sources)) %>%
    mutate(train = TRUE)
  
  testing <- rbind(KM_df %>%
                      filter(!(.data$dep1 %in% incl_sources)),
                    KNM_df %>%
                      filter(!(.data$dep1 %in% incl_sources))) %>%
    mutate(train = FALSE)
  
  return(rbind(training, testing))

}
