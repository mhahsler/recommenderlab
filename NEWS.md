# Changes in version 0.1-8.1 (xx/xx/2015)

* FIX: rownames of newdata are now preserved in prediction output.
* We use testthat now.
* normalization now can be done on rows and columns at the same time.
* SVD now folds in new users.

# Changes in version 0.1-8 (12/17/2015)

* fixed several problems in the vignette.
* predict for realRatingMatrix accepts now type = "ratingMatrix" to returns
  a completed rating matrix.
* Negative values for given in evaluationScheme implement all-but-given 
  evaluation.
* method "SVD" used now EM-based approximation from package bcv.

# Changes in version 0.1-7 (7/23/2015)

* NAMESPACE now imports non standard R packages.

# Changes in version 0.1-5 (8/18/2014)

* Fixed NAMESPACE problems.
* evaluation of ratings is now better integrated into evaluate.
* binarize keeps now dimnames.

# Changes prior to 0.1-4 (1/11/2013)

* many

# Alpha version 0.1-0 (1/23/2010)
