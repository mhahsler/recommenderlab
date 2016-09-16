# Changes in version 0.2-1 (09/15/2016)

* Changes in recommendation method AR: Default for maxlen is now 3 to 
    find more specific rules. Parameters measure and decreasing for 
    sorting the rule base are now called sort_measure and sort_decreasing.
    New parameter apriori_control can be used to pass a control list to
    apriori in arules.
* The registry now has a reference field.
* Added recommender method ALS and ALS_implicit based on latent factors 
    and alternating least squares (contributed by Bregt Verreet).
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
