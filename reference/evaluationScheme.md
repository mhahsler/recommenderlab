# Creator Function for evaluationScheme

Creates an evaluationScheme object from a data set. The scheme can be a
simple split into training and test data, k-fold cross-evaluation or
using k independent bootstrap samples.

## Usage

``` r
evaluationScheme(data, ...)

# S4 method for class 'ratingMatrix'
evaluationScheme(data, method="split",
    train=0.9, k=NULL, given, goodRating = NA)
```

## Arguments

- data:

  data set as a ratingMatrix.

- method:

  a character string defining the evaluation method to use (see
  details).

- train:

  fraction of the data set used for training.

- k:

  number of folds/times to run the evaluation (defaults to 10 for
  cross-validation and bootstrap and 1 for split).

- given:

  single number of items given for evaluation or a vector of length of
  data giving the number of items given for each observation. Negative
  values implement all-but schemes. For example, `given = -1` means
  all-but-1 evaluation.

- goodRating:

  numeric; threshold at which ratings are considered good for
  evaluation. E.g., with `goodRating=3` all items with actual user
  rating of greater or equal 3 are considered positives in the
  evaluation process. Note that this argument is only used if the
  ratingMatrix is a of subclass realRatingMatrix!

- ...:

  further arguments.

## Details

`evaluationScheme` creates an evaluation scheme (training and test data)
with `k` runs and one of the following methods:

`"split"` randomly assigns the proportion of objects specified by
`train` to the training set and the rest is used for the test set.

`"cross-validation"` creates a k-fold cross-validation scheme. The data
is randomly split into k parts and in each run k-1 parts are used for
training and the remaining part is used for testing. After all k runs
each part was used as the test set exactly once.

`"bootstrap"` creates the training set by taking a bootstrap sample
(sampling with replacement) of size `train` times number of users in the
data set. All objects not in the training set are used for testing.

For evaluation, Breese et al. (1998) introduced the four experimental
protocols called Given 2, Given 5, Given 10 and All-but-1. During
testing, the Given x protocol presents the algorithm with only x
randomly chosen items for the test user, and the algorithm is evaluated
by how well it is able to predict the withheld items. For All-but-x, the
algorithm sees all but x withheld ratings for the test user. `given`
controls x in the evaluations scheme. Positive integers result in a
Given x protocol, while negative values produce a All-but-x protocol.

If a user does not have enough ratings to satisfy `given`, then the user
is dropped from the evaluation with a warning.

## Value

Returns an object of class `"evaluationScheme"`.

## References

Kohavi, Ron (1995). "A study of cross-validation and bootstrap for
accuracy estimation and model selection". Proceedings of the Fourteenth
International Joint Conference on Artificial Intelligence, pp.
1137-1143.

Breese JS, Heckerman D, Kadie C (1998). "Empirical Analysis of
Predictive Algorithms for Collaborative Filtering." In Uncertainty in
Artificial Intelligence. Proceedings of the Fourteenth Conference, pp.
43-52.

## See also

[`getData`](http://michael.hahsler.net/recommenderlab/reference/evaluationScheme-class.md),
[`evaluationScheme`](http://michael.hahsler.net/recommenderlab/reference/evaluationScheme-class.md),
[`ratingMatrix`](http://michael.hahsler.net/recommenderlab/reference/ratingMatrix-class.md).

## Examples

``` r
data("MSWeb")

MSWeb10 <- sample(MSWeb[rowCounts(MSWeb) >10,], 50)
MSWeb10
#> 50 x 285 rating matrix of class ‘binaryRatingMatrix’ with 683 ratings.

## simple split with 3 items given
esSplit <- evaluationScheme(MSWeb10, method="split",
        train = 0.9, k=1, given=3)
esSplit
#> Evaluation scheme with 3 items given
#> Method: ‘split’ with 1 run(s).
#> Training set proportion: 0.900
#> Good ratings: NA
#> Data set: 50 x 285 rating matrix of class ‘binaryRatingMatrix’ with 683 ratings.

## 4-fold cross-validation with all-but-1 items for learning.
esCross <- evaluationScheme(MSWeb10, method="cross-validation",
        k=4, given=-1)
esCross
#> Evaluation scheme using all-but-1 items
#> Method: ‘cross-validation’ with 4 run(s).
#> Good ratings: NA
#> Data set: 50 x 285 rating matrix of class ‘binaryRatingMatrix’ with 683 ratings.
```
