
<!-- [![R-CMD-check](https://github.com/valeriehan6/scoreLR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/valeriehan6/scoreLR/actions/workflows/R-CMD-check.yaml) -->
<!-- [![Coverage status](https://codecov.io/gh/valeriehan6/scoreLR/branch/main/graph/badge.svg)](https://codecov.io/github/valeriehan6/scoreLR?branch=main) -->

# scoreLR <img src="man/figures/logo.png" align="right" height="139" />

<!-- # scoreLR -->

This package computes SLRs using various methods that address the
dependence between the data and compares the various methods.

## Installation

You can install the development version of scoreLR like so:

``` r
devtools::install_github("https://github.com/valeriehan6/scoreLR")
```

## Load Library

``` r
library(scoreLR)
```

## Example

How do you use this package?

Read a dataset in that follows these requirements:

``` r
data(shoedata)
```

Then, create a train/test split that accounts for the dependencies using
the `dep_split` function:

``` r
shoedata_split <- dep_split(shoedata, 0.8, 20230413)


KM_train <- shoedata_split %>% filter(source1 == source2 & train == TRUE) 
KM_test <- shoedata_split %>% filter(source1 == source2 & train == FALSE)

KNM_train <- shoedata_split %>% filter(source1 != source2 & train == TRUE)
KNM_test <- shoedata_split %>% filter(source1 != source2 & train == FALSE)
```

Calculate SLRs based on various methods:

``` r
ignore_dep(KM_train, KM_test, KNM_train, KNM_test)
strict_indep_set(KM_train, KM_test, KNM_train, KNM_test)
avg_features(KM_train, KM_test, KNM_train, KNM_test)
multiple_kde(KM_train, KM_test, KNM_train, KNM_test)
```

You can do it all in one step with the `slr_results` function:

``` r
slr_results(shoedata_split)
```

## Source

Han, Valerie. “Classification performance of score-based likelihood
ratios when data exhibit various degrees of dependence.” (2022).
