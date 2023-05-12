# Generated from scoreLR.Rmd: do not edit by hand  
testthat::test_that("shoedata is properly formatted", {
  # Attach data
  data(shoedata)
  
  # Correct type
  testthat::expect_type(shoedata, "list")
  testthat::expect_s3_class(shoedata, c("sf","tbl_df", "tbl", "data.frame"))
  
  # Correct dimensions
  testthat::expect_equal(dim(shoedata), c(926, 7))
  
  # Correct column names
  testthat::expect_named(shoedata, c("source1", "dep1", "source2", "dep2", 
                           "clique_size", "med_dist_euc", "input_overlap"))
})

testthat::test_that("dep_split works", {
  data(shoedata)
  
  train.8 <- dep_split(data = shoedata, p = 0.8, seed = 585)
  
  # Correct type
  testthat::expect_type(train.8, "list")
  testthat::expect_s3_class(train.8, c("sf","tbl_df", "tbl", "data.frame"))
  
  # Correct dimensions
  testthat::expect_equal(dim(train.8), c(nrow(shoedata), ncol(shoedata) + 1))
  
  # Correct column names
  testthat::expect_named(train.8, c("source1", "dep1", "source2", "dep2", 
                                    "clique_size", "med_dist_euc", 
                                    "input_overlap", "train"))
  
  # train column is logical
  testthat::expect_type(train.8$train, "logical")
  
  # train column has no NAs
  testthat::expect_false(any(is.na(train.8$train)))
  
  # check if train and test overlap on same pairs of deps
  num_overlap <- train.8 %>%
    filter(source1 != source2) %>%
    group_by(dep1, dep2) %>%
    summarize(num_unique = length(unique(train))) %>%
    filter(num_unique > 1) %>% 
    nrow()
  
  testthat::expect_equal(num_overlap, 0)
  
  # there are the correct number of T's in the train column
  train.0 <- dep_split(data = shoedata, p = 0, seed = 585)
  num_T <- sum(train.0$train)
  testthat::expect_equal(num_T, 0)
  
  train.1 <- dep_split(data = shoedata, p = 1, seed = 585)
  num_T <- sum(train.1$train)
  testthat::expect_equal(num_T, nrow(shoedata))
  
})

testthat::test_that("opt_thresh works", {
  KM_SLR_train <- seq(-10, 10, .01)
  KNM_SLR_train <- seq(-5, 5, .01)
  
  thresh <- opt_thresh(KM_SLR_train, KNM_SLR_train)
  
  # should get equal tpr and tnr at 0
  testthat::expect_equal(thresh, 0, tolerance = 0.01)
})

testthat::test_that("ignore_dep works", {
  # unknown_NA = T means that we expect new_SLR to be NA
  test.method <- function(res, new_SLR_NA = T) {
    testthat::expect_type(res, "list")
    testthat::expect_named(res, c("KM_SLR", "KNM_SLR", "threshold", "new_SLR", 
                                  "ROC_values"))
    testthat::expect_false(any(res$KM_SLR<0))
    testthat::expect_false(any(res$KNM_SLR<0))
    testthat::expect_type(res$threshold, "double")
    if (new_SLR_NA) {
      testthat::expect_true(is.na(res$new_SLR))
    } else {
      testthat::expect_type(res$new_SLR, "list")
    }
    testthat::expect_false(any(res$ROC_values$tpr < 0 | res$ROC_values$tpr > 1))
    testthat::expect_false(any(res$ROC_values$fpr < 0 | res$ROC_values$fpr > 1))
  }
  
  #data(shoedata)
  shoedata_split <- dep_split(data = shoedata, p = 0.8, seed = 585)
  shoedata_unknown <- shoedata
  shoedata_unknown$train <- rep(TRUE, nrow(shoedata_unknown))

  # Multivariate case (3 scores)
  KM_train <- shoedata_split %>% filter(source1 == source2 & train == TRUE) 
  KM_test <- shoedata_split %>% filter(source1 == source2 & train == FALSE) 
  KNM_train <- shoedata_split %>% filter(source1 != source2 & train == TRUE)
  KNM_test <- shoedata_split %>% filter(source1 != source2 & train == FALSE)
  
  ignore_dep_output_mult <- ignore_dep(KM_train, KM_test, KNM_train, KNM_test, unknown = NULL)
  test.method(ignore_dep_output_mult, new_SLR_NA = T)
  
  # Univariate case (1 score)
  KM_train_uni <- KM_train %>% 
    select(source1, dep1, source2, dep2, input_overlap, train)
  KM_test_uni <- KM_test %>% 
    select(source1, dep1, source2, dep2, input_overlap, train)
  KNM_train_uni <- KNM_train %>% 
    select(source1, dep1, source2, dep2, input_overlap, train)
  KNM_test_uni <- KNM_test %>% 
    select(source1, dep1, source2, dep2, input_overlap, train)
  
  ignore_dep_output_uni <- ignore_dep(KM_train_uni, KM_test_uni, 
                                      KNM_train_uni, KNM_test_uni, unknown = NULL)
  test.method(ignore_dep_output_uni, new_SLR_NA = T)
  
  # Multivariate with unknown parameter
  ignore_dep_output_unknown_mult <- ignore_dep(KM_train, KM_test, KNM_train, 
                                               KNM_test, unknown = shoedata_unknown)
  test.method(ignore_dep_output_unknown_mult, new_SLR_NA = F)
  
  # Univariate with unknown parameter
  ignore_dep_output_unknown_uni <- ignore_dep(KM_train_uni, KM_test_uni, 
                                              KNM_train_uni, KNM_test_uni, 
                                              unknown = shoedata_unknown)
  test.method(ignore_dep_output_unknown_uni, new_SLR_NA = F)
  
  
})

testthat::test_that("indep_km_set works", {
  data(shoedata)
  KM_train <- shoedata %>% filter(source1 == source2)
  
  km_set <- indep_km_set(KM_train, seed=585)
  
  # Correct type
  testthat::expect_type(km_set, "list")
  testthat::expect_s3_class(km_set, c("sf","tbl_df", "tbl", "data.frame"))
  
  # Correct dimensions
  testthat::expect_equal(ncol(km_set), ncol(KM_train))

  # Correct column names
  testthat::expect_named(km_set, colnames(KM_train))
  
  
  
  num_shoes <- length(unique(KM_train$source1))
  
  # Check that the number of rows is correct
  testthat::expect_equal(nrow(km_set), num_shoes)
  
  # Check that the number of shoes is correct
  testthat::expect_equal(length(unique(km_set$source1)), num_shoes)
  
})

testthat::test_that("indep_knm_set works", {
  data(shoedata)
  KNM_train <- shoedata %>% filter(source1 != source2)
  
  knm_set <- indep_knm_set(KNM_train, seed=585)
  
  # Correct type
  testthat::expect_type(knm_set, "list")
  testthat::expect_s3_class(knm_set, c("sf","tbl_df", "tbl", "data.frame"))
  
  # Correct dimensions
  testthat::expect_equal(ncol(knm_set), ncol(KNM_train))

  # Correct column names
  testthat::expect_named(knm_set, colnames(KNM_train))
  
  
  
  num_pairs <- KNM_train %>% group_by(source1, source2) %>% count() %>% nrow()
  
  # Check that the number of rows is correct
  testthat::expect_equal(nrow(knm_set), num_pairs)
  
  # Check that the number of pairs of shoes is correct
  knm_set_pairs <- knm_set %>% group_by(source1, source2) %>% count() %>% nrow()
  testthat::expect_equal(knm_set_pairs, num_pairs)
  
})

testthat::test_that("strict_indep_set works", {
  # unknown_NA = T means that we expect new_SLR to be NA
  test.method <- function(res, new_SLR_NA = T) {
    testthat::expect_type(res, "list")
    testthat::expect_named(res, c("KM_SLR", "KNM_SLR", "threshold", "new_SLR", 
                                  "ROC_values"))
    testthat::expect_false(any(res$KM_SLR<0))
    testthat::expect_false(any(res$KNM_SLR<0))
    testthat::expect_type(res$threshold, "double")
    if (new_SLR_NA) {
      testthat::expect_true(is.na(res$new_SLR))
    } else {
      testthat::expect_type(res$new_SLR, "list")
    }
    testthat::expect_false(any(res$ROC_values$tpr < 0 | res$ROC_values$tpr > 1))
    testthat::expect_false(any(res$ROC_values$fpr < 0 | res$ROC_values$fpr > 1))
  }
  
  #data(shoedata)
  shoedata_split <- dep_split(data = shoedata, p = 0.8, seed = 585)
  shoedata_unknown <- shoedata
  shoedata_unknown$train <- rep(TRUE, nrow(shoedata_unknown))

  # Multivariate case (3 scores)
  KM_train <- shoedata_split %>% filter(source1 == source2 & train == TRUE) 
  KM_test <- shoedata_split %>% filter(source1 == source2 & train == FALSE) 
  KNM_train <- shoedata_split %>% filter(source1 != source2 & train == TRUE)
  KNM_test <- shoedata_split %>% filter(source1 != source2 & train == FALSE)
  
  strict_indep_set_output_mult <- strict_indep_set(KM_train, KM_test, KNM_train, KNM_test, unknown = NULL, seed = 585)
  test.method(strict_indep_set_output_mult, new_SLR_NA = T)
  
  # Univariate case (1 score)
  KM_train_uni <- KM_train %>% 
    select(source1, dep1, source2, dep2, input_overlap, train)
  KM_test_uni <- KM_test %>% 
    select(source1, dep1, source2, dep2, input_overlap, train)
  KNM_train_uni <- KNM_train %>% 
    select(source1, dep1, source2, dep2, input_overlap, train)
  KNM_test_uni <- KNM_test %>% 
    select(source1, dep1, source2, dep2, input_overlap, train)
  
  strict_indep_set_output_uni <- strict_indep_set(KM_train_uni, KM_test_uni, 
                                      KNM_train_uni, KNM_test_uni, unknown = NULL, seed = 585)
  test.method(strict_indep_set_output_uni, new_SLR_NA = T)
  
  # Multivariate with unknown parameter
  strict_indep_set_output_unknown_mult <- strict_indep_set(KM_train, KM_test, KNM_train, 
                                               KNM_test, unknown = shoedata_unknown, seed = 585)
  test.method(strict_indep_set_output_unknown_mult, new_SLR_NA = F)
  
  # Univariate with unknown parameter
  strict_indep_set_output_unknown_uni <- strict_indep_set(KM_train_uni, KM_test_uni, 
                                              KNM_train_uni, KNM_test_uni, 
                                              unknown = shoedata_unknown, seed = 585)
  test.method(strict_indep_set_output_unknown_uni, new_SLR_NA = F)
  
  
})

testthat::test_that("avg_features works", {
  # unknown_NA = T means that we expect new_SLR to be NA
  test.method <- function(res, new_SLR_NA = T) {
    testthat::expect_type(res, "list")
    testthat::expect_named(res, c("KM_SLR", "KNM_SLR", "threshold", "new_SLR", 
                                  "ROC_values"))
    testthat::expect_false(any(res$KM_SLR<0))
    testthat::expect_false(any(res$KNM_SLR<0))
    testthat::expect_type(res$threshold, "double")
    if (new_SLR_NA) {
      testthat::expect_true(is.na(res$new_SLR))
    } else {
      testthat::expect_type(res$new_SLR, "list")
    }
    testthat::expect_false(any(res$ROC_values$tpr < 0 | res$ROC_values$tpr > 1))
    testthat::expect_false(any(res$ROC_values$fpr < 0 | res$ROC_values$fpr > 1))
  }
  
  #data(shoedata)
  shoedata_split <- dep_split(data = shoedata, p = 0.8, seed = 585)
  shoedata_unknown <- shoedata
  shoedata_unknown$train <- rep(TRUE, nrow(shoedata_unknown))

  # Multivariate case (3 scores)
  KM_train <- shoedata_split %>% filter(source1 == source2 & train == TRUE) 
  KM_test <- shoedata_split %>% filter(source1 == source2 & train == FALSE) 
  KNM_train <- shoedata_split %>% filter(source1 != source2 & train == TRUE)
  KNM_test <- shoedata_split %>% filter(source1 != source2 & train == FALSE)
  
  avg_features_output_mult <- avg_features(KM_train, KM_test, KNM_train, KNM_test, unknown = NULL)
  test.method(avg_features_output_mult, new_SLR_NA = T)
  
  # Univariate case (1 score)
  KM_train_uni <- KM_train %>% 
    select(source1, dep1, source2, dep2, input_overlap, train)
  KM_test_uni <- KM_test %>% 
    select(source1, dep1, source2, dep2, input_overlap, train)
  KNM_train_uni <- KNM_train %>% 
    select(source1, dep1, source2, dep2, input_overlap, train)
  KNM_test_uni <- KNM_test %>% 
    select(source1, dep1, source2, dep2, input_overlap, train)
  
  avg_features_output_uni <- avg_features(KM_train_uni, KM_test_uni, 
                                      KNM_train_uni, KNM_test_uni, unknown = NULL)
  test.method(avg_features_output_uni, new_SLR_NA = T)
  
  # Multivariate with unknown parameter
  avg_features_output_unknown_mult <- avg_features(KM_train, KM_test, 
                                                   KNM_train, KNM_test, 
                                                   unknown = shoedata_unknown)
  test.method(avg_features_output_unknown_mult, new_SLR_NA = F)
  
  # Univariate with unknown parameter
  avg_features_output_unknown_uni <- avg_features(KM_train_uni, KM_test_uni, 
                                                  KNM_train_uni, KNM_test_uni, 
                                                  unknown = shoedata_unknown)
  test.method(avg_features_output_unknown_uni, new_SLR_NA = F)
})

testthat::test_that("multiple_kde works", {
  # unknown_NA = T means that we expect new_SLR to be NA
  test.method <- function(res, new_SLR_NA = T) {
    testthat::expect_type(res, "list")
    testthat::expect_named(res, c("KM_SLR", "KNM_SLR", "threshold", "new_SLR", 
                                  "ROC_values"))
    testthat::expect_false(any(res$KM_SLR<0))
    testthat::expect_false(any(res$KNM_SLR<0))
    testthat::expect_type(res$threshold, "double")
    if (new_SLR_NA) {
      testthat::expect_true(is.na(res$new_SLR))
    } else {
      testthat::expect_type(res$new_SLR, "list")
    }
    testthat::expect_false(any(res$ROC_values$tpr < 0 | res$ROC_values$tpr > 1))
    testthat::expect_false(any(res$ROC_values$fpr < 0 | res$ROC_values$fpr > 1))
  }
  
  #data(shoedata)
  shoedata_split <- dep_split(data = shoedata, p = 0.8, seed = 585)
  shoedata_unknown <- shoedata
  shoedata_unknown$train <- rep(TRUE, nrow(shoedata_unknown))

  # Multivariate case (3 scores)
  KM_train <- shoedata_split %>% filter(source1 == source2 & train == TRUE) 
  KM_test <- shoedata_split %>% filter(source1 == source2 & train == FALSE) 
  KNM_train <- shoedata_split %>% filter(source1 != source2 & train == TRUE)
  KNM_test <- shoedata_split %>% filter(source1 != source2 & train == FALSE)
  
  multiple_kde_output_mult <- multiple_kde(KM_train, KM_test, KNM_train, KNM_test, 
                                           NUM_SETS = 12, 
                                           unknown = NULL)
  test.method(multiple_kde_output_mult, new_SLR_NA = T)
  
  # Univariate case (1 score)
  KM_train_uni <- KM_train %>% 
    select(source1, dep1, source2, dep2, input_overlap, train)
  KM_test_uni <- KM_test %>% 
    select(source1, dep1, source2, dep2, input_overlap, train)
  KNM_train_uni <- KNM_train %>% 
    select(source1, dep1, source2, dep2, input_overlap, train)
  KNM_test_uni <- KNM_test %>% 
    select(source1, dep1, source2, dep2, input_overlap, train)
  
  multiple_kde_output_uni <- multiple_kde(KM_train_uni, KM_test_uni, 
                                      KNM_train_uni, KNM_test_uni, NUM_SETS = 12,
                                      unknown = NULL)
  test.method(multiple_kde_output_uni, new_SLR_NA = T)
  
  multiple_kde_output_unknown <- multiple_kde(KM_train, KM_test, KNM_train, KNM_test, 
                                          NUM_SETS = 12, unknown = shoedata_unknown)
  test.method(multiple_kde_output_unknown, new_SLR_NA = F)
  
  
})

testthat::test_that("slr_results works", {
  # data(shoedata)
  shoedata_split <- dep_split(data = shoedata, p = 0.8, seed = 585)
  testthat::expect_error(slr_results(shoedata_split[,-1])) # check column names
  
  ones <- rep(1, nrow(shoedata_split))
  twos <- rep(2, nrow(shoedata_split))
  threes <- rep(3, nrow(shoedata_split))
  shoedata_split_big <- cbind(shoedata_split, ones, twos, threes)
  testthat::expect_error(slr_results(shoedata_split_big)) # check number of columns
  
  shoedata_split_factor <- shoedata_split
  shoedata_split_factor$clique_size <- as.factor(shoedata_split_factor$clique_size)
  testthat::expect_error(slr_results(shoedata_split_factor)) # check that score columns are numeric
  
  testthat::expect_error(slr_results(shoedata_split, method = "OLS")) # check wrong method
  testthat::expect_warning(slr_results(shoedata_split, 
                                       method = c("AverageFeatures", "OLS"))) # check for warning
  
  slr_results_output_mult <- slr_results(shoedata_split)
  testthat::expect_type(slr_results_output_mult, "list")
  testthat::expect_named(slr_results_output_mult, 
                         c("IgnoreDependence", "StrictIndependentSet",
                           "AverageFeatures", "MultipleKDE" ))
  
  
})


testthat::test_that("plot_slr_roc works", {
  testthat::expect_error(plot_slr_roc(shoedata, p = .5,
                                      method = c("IgnoreDependence", 
                                                 "AverageFeatures")))
  testthat::expect_type(plot_slr_roc(shoedata, p = .5,
                                     method = "IgnoreDependence", num_runs = 5), 
                        "list")
})

