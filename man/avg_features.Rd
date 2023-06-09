% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/avg_features.R
\name{avg_features}
\alias{avg_features}
\title{Average features}
\usage{
avg_features(KM_train, KM_test, KNM_train, KNM_test, unknown = NULL)
}
\arguments{
\item{KM_train}{known matches (KM) from training dataset.}

\item{KM_test}{known matches (KM) from testing dataset.}

\item{KNM_train}{known non matches (KNM) from training dataset.}

\item{KNM_test}{known non matches (KNM) from testing dataset.}

\item{unknown}{data frame of scores from an unknown case. Column names should match the variable names set for scores.}
}
\value{
A list containing the following components:
\item{KM_SLR}{SLRs for \code{KM_test}.}
\item{KNM_SLR}{SLRs for \code{KNM_test}.}
\item{threshold}{optimal threshold calculated using \code{KM_train} and \code{KNM_train}.}
\item{new_SLR}{SLRs for \code{unknown}.}
\item{ROC_values}{data frame containing columns the true positive rate \code{tpr} and false positive rate \code{fpr} computed using \code{KM_test} and \code{KNM_test}.}
}
\description{
This function estimates the SLR function using the average features method, averaging each score over pairs of sources and computing kernel density estimation (KDE) on averaged features. The densities are estimated with \code{KM_train} and \code{KNM_train} and the function produces various output based on \code{KM_test}, \code{KNM_test}, and \code{unknown}.

\code{KM_train}, \code{KM_test}, \code{KNM_train}, \code{KNM_train} should all be data frames containing columns named \code{source1}, \code{source2}, \code{dep1}, and \code{dep2}. Any remaining columns must be scores. There may be up to five scores.
}
\examples{
library(dplyr)
# Set up data
shoedata_split <- dep_split(shoedata, 0.75)
KM_train <- shoedata_split \%>\% filter(source1 == source2 & train == TRUE)
KM_test <- shoedata_split \%>\% filter(source1 == source2 & train == FALSE)
KNM_train <- shoedata_split \%>\% filter(source1 != source2 & train == TRUE)
KNM_test <- shoedata_split \%>\% filter(source1 != source2 & train == FALSE)
unknown <- data.frame(clique_size = c(5, 8), med_dist_euc = c(1.9, 1.1), 
                      input_overlap = c(.01, 0.26))

res <- avg_features(KM_train, KM_test, KNM_train, KNM_test, unknown)
}
