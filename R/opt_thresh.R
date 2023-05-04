# Generated from scoreLR.Rmd: do not edit by hand

#' Optimal threshold
#' @description 
#' This function estimates the optimal threshold for the provided SLRs. Optimality here is defined as minimizing the distance between the true positive and true negative rates.
#' @importFrom stats optimize
#' @param KM_SLR_train vector of SLRs from known matches from the training dataset.
#' @param KNM_SLR_train vector of SLRs from known non-matches from the training dataset.
#' @returns optimal threshold for SLR. 
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
  
  # if (res$convergence != 0) {
  # print(paste("ERROR: Convergence code is", res$convergence))
  # } else {
  # return(res$par)
  # }
  return(res$minimum)
}

