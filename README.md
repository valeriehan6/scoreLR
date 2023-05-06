
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

Read a dataset in that has the following columns:

-   `source1`: shoe identifier for the first shoe, left (L) or right (R)
-   `dep1`: pair identifier for the first shoe
-   `source2`: shoe identifier for the second shoe, left (L) or right
    (R)
-   `dep2`: pair identifier for the second shoe
-   up to five columns of similarity/dissimilarity scores. These do not
    have to be named something specific.

The following shoeprint dataset is included in the package:

``` r
data(shoedata)
```

Create a train/test split that accounts for the dependencies using the
`dep_split` function:

``` r
shoedata_split <- dep_split(shoedata, 0.8, 20230413)

head(shoedata_split)
```

    ##   source1 dep1 source2 dep2 clique_size med_dist_euc input_overlap train
    ## 1     1_L    1     1_L    1           6      1.81890         0.218  TRUE
    ## 2     1_L    1     1_L    1           6      1.41041         0.200  TRUE
    ## 3     1_L    1     1_L    1           6      1.80102         0.102  TRUE
    ## 4     1_L    1     1_L    1           8      1.08331         0.258  TRUE
    ## 5     1_L    1     1_L    1           5      1.93781         0.008  TRUE
    ## 6     1_L    1     1_L    1          12      0.91193         0.400  TRUE

Calculate SLRs based on various methods:

``` r
# Split data first
KM_train <-  dplyr::filter(shoedata_split, source1 == source2 & train == TRUE)
KM_test <- dplyr::filter(shoedata_split, source1 == source2 & train == FALSE)
KNM_train <- dplyr::filter(shoedata_split, source1 != source2 & train == TRUE)
KNM_test <- dplyr::filter(shoedata_split, source1 == source2 & train == FALSE)

# method functions
ignore_dep(KM_train, KM_test, KNM_train, KNM_test)
strict_indep_set(KM_train, KM_test, KNM_train, KNM_test)
avg_features(KM_train, KM_test, KNM_train, KNM_test)
multiple_kde(KM_train, KM_test, KNM_train, KNM_test)
```

You can do it all in one step with the `slr_results` function after
doing that train/test split.

``` r
slr_results(shoedata_split)
```

## Source

Han, Valerie. “Classification performance of score-based likelihood
ratios when data exhibit various degrees of dependence.” (2022).
