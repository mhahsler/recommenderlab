# Changes in version 1.0.5 (09/16/2023)

## Bugfixes
* changed parameter name in interestMeasure().
* fixed issue with adding a single interest measure.
* fixed bug in row/colSums call for Matrix (reported by Mikael Jagan).

# Changes in version 1.0.4 (06/20/2023)

## Bugfixes
* added missing rmse function for funkSVD man page.
* test-recom.R: removed extra comma.

# Changes in version 1.0.3 (01/20/2023)

## New Features
* evaluationScheme now drops users with too few ratings with a warning.
* evaluationScheme creation is now faster for realRatingMatrix.

## Bugfixes
* Fixed issues with ratingMatrix with missing dimnames.
* UBCF does now also work for users with fewer than n nearest neighbors.

# Changes in version 1.0.2 (08/17/2022)

## Internal Changes
* Preparations for changes in coercion for Matrix 1.4.2

# Changes in version 1.0.1 (06/17/2022)

## Bugfixes
* Fixed similarity() and dissimilarity() after changes for Cosine in package proxy 
  (reported by Artur Gramacki).
* dropNA now always creates a dgCMatrix.

# Changes in version 1.0.0 (05/27/2022)

## Bugfixes
* calcPredictionAccuracy now works with negative values for given (all-but-x). A negative value produces an error with instructions. 
* We require now proxy version >= 0.4-26 which fixed a conversion bug for cosine similarity.
* RECOM_AR now respects already know items (code provided by gregreich).
* evaluate: keepModel = TRUE now works (bug reported by gregreich). 
* Recom_SVD: fixed issue with missing values set to zero (bug reported by jpbrooks@vcu.edu)

## Changes
* Ratings of zero are now fully supported. We use .Machine$double.xmin to represent 0 in 
  sparse matices. zapsmall() can be used to change them back to 0.
* topNList has now a method c() to combine multiple lists.
* RECOM_AR: Ratings are now equal to quality measure used for ranking.
* HYBRIDRECOMMENDER: add "max" and "min" aggregation.
* removeKnownRatings is now sparse.
* RECOM_RANDOM now has parameter range to specify the rating range.

# Changes in version 0.2-7 (04/26/2021)

## New Features
* The MovieLense data set includes now also user meta information.

## Changes
* getConfusionMatrix() is deprecated. Use getResults() instead.
* added an example for how to evaluate hybrid recommenders.
* calcPredicition now also reports N.
* calcPredicition now stores the list length for multiple top-N lists as a column called n in the result (instead of using rownames). 

## Bugfixes
* UBCF for binary data: Fixed normalization for option weighted (reported by bhawwash).
* Fixed problems with less than k neighbors (reported by weiy6).
* Fixed incorrect description of comparisons in vignette.

# Changes in version 0.2-6 (06/16/2020)

## New Features
* ratingMatrix gained method hasRatings.
* Recommender gained method "HYBRID" to create hybrid recommenders. Now hybrid recommenders can also be used in evaluate().
* similarity gained parameters min_matching and min_predictive.

## Bugfixes
* predict for Recommender RANDOM now uses the correct user ids in the prediction (reported by aliko-str). 
* fixed weight bug in Recommender UBCF (reported by aliko-str). 
* Recommender UBCF now removes self-matches if item ids are specified in newdata. Specifying data in predict is no longer necessary. (reported by aliko-str). 
* HybridRecommender now handles NAs in predictions correctly (was handled as 0).

# Changes in version 0.2-5 (08/27/2019)

## Changes
* predict with type "ratingMatrix" now returns predictions for the known ratings instead of replacing them with the known values.
* Recommender methods Popular, AR and RERECOMMENDER now also return ratings for binary data (and thus can be used for HybridRecommender).
* Added a LIBMF-based recommender.

## Bugfixes
* evaluationScheme with negative numbers for given (all-but-x scheme) now works even if there are no given items left (reported by philippschmalen).

# Changes in version 0.2-4 (03/23/2019)

## Bugfixes
* Fixed bug in denormalization by column with z-score (reported by jackyrx).
* Fixed bug in predict with type "ratingMatrix" where known values were not denormalized (reported by MounirHader).

# Changes in version 0.2-3 (06/19/2018)

## Bugfixes
* Fixed bug in ALS_implicit (reported by equalise).
* getData for binaryRatingMatrix data with type "known" and "unknown" 
    preserves now user ids/rownames (reported by Kasia Kulma).
* predict for HybridRecommender now retains user IDs (reported by homodigitus).
* Removed warning about using drop in subsetting ratingMatrices (reported by donnydongchen).

# Changes in version 0.2-2 (04/05/2017)

## Bugfixes
* predict for IBCF now returns top-N lists correctly.
* (cross) dissimilarity for binary data now returns the correct data 
    type (reported by inkrement). 

# Changes in version 0.2-1 (09/15/2016)

## New Features
* Added recommender method ALS and ALS_implicit based on latent factors 
    and alternating least squares (contributed by Bregt Verreet).
* Changes in recommendation method AR: Default for maxlen is now 3 to 
    find more specific rules. Parameters measure and decreasing for 
    sorting the rule base are now called sort_measure and sort_decreasing.
    New parameter apriori_control can be used to pass a control list to
    apriori in arules.
* The registry now has a reference field.

## Bugfixes
* Fixed bug in method IBCF with n being ignored in 
    predict (reported by Giorgio Alfredo Spedicato).

# Changes in version 0.2-0 (05/31/2016)

* Added recommender RERECOMMEND to recommend highly rated items again (e.g.,
    movies to watch again).
* Added a hybrid recommender (HybridRecommender).
* realRatingMatrix supports now subset assignment with [.
* RECOM_POPULAR now shows the parameters in the registry. 
* RECOM_RANDOM produced now random ratings from the estimated distribution of
  the available recommendations (from a normal distribution with the user's
  means and standard deviation).
* predict now checks if newdata (number of items) is compatible with the model.  
* getTopNLists and bestN gained a randomized argument to increase prediction
  diversity.
* Added getRatings method for topNList.

# Changes in version 0.1-9 (05/18/2016)

* FIX: rownames of newdata are now preserved in prediction output.
* We use testthat now.
* Normalization now can be done on rows and columns at the same time.
* SVD with column-mean imputation now folds in new users.
* Added Funk SVD (funkSVD and recommender SVDF).
* Added function error measures: MAE, MSE, RMSE, frobenius (norm).
* Jester5k contains now the jokes.
* MovieLense contains now movie meta information.
* topNLists now also contains ratings.
* Removed obsolete PCA-based recommender.

# Changes in version 0.1-8 (12/17/2015)

* Fixed several problems in the vignette.
* predict for realRatingMatrix accepts now type = "ratingMatrix" to returns
  a completed rating matrix.
* Negative values for given in evaluationScheme implement all-but-given 
  evaluation.
* Method "SVD" used now EM-based approximation from package bcv.

# Changes in version 0.1-7 (7/23/2015)

* NAMESPACE now imports non standard R packages.

# Changes in version 0.1-5 (8/18/2014)

* Fixed NAMESPACE problems.
* Evaluation of ratings is now better integrated into evaluate.
* binarize keeps now dimnames.

# Changes prior to 0.1-4 (1/11/2013)

* Many.

# Alpha version 0.1-0 (1/23/2010)
