# Generated from scoreLR.Rmd: do not edit by hand

#' Optimal threshold
#' @description 
#' This function estimates the optimal threshold for the provided SLRs. Optimality here is defined as minimizing the distance between the true positive and true negative rates.
#' @importFrom stats optimize
#' @param KM_SLR_train Vector of SLRs from known matches from the training dataset.
#' @param KNM_SLR_train Vector of SLRs from known non-matches from the training dataset.
#' @returns Optimal threshold for SLR. 
opt_thresh <- function(KM_SLR_train, KNM_SLR_train) {
  
  func <- function(thresh) {
    tp <- (KM_SLR_train > thresh)
    tn <- (KNM_SLR_train < thresh)
    tpr = sum(tp)/length(tp)
    tnr = sum(tn)/length(tn)
    
    return(abs(tpr-tnr))
  }
  
  upper <- min(10000, max(KM_SLR_train), max(KNM_SLR_train))
  lower <- max(1/10000, min(KM_SLR_train), min(KNM_SLR_train))
  res <- optimize(func, c(lower,upper))
  
  return(res$minimum)
}

# gives threshold for input_overlap based on estimated KDEs
# thresh is output from opt_thresh function

