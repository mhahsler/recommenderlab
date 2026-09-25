# Calculate the Prediction Error for a Recommendation

Calculate prediction accuracy. For predicted ratings MAE (mean average
error), MSE (means squared error) and RMSE (root means squared error)
are calculated. For topNLists various binary classification metrics are
returned (e.g., precision, recall, TPR, FPR).

## Usage

``` r
calcPredictionAccuracy(x, data, ...)

# S4 method for class 'realRatingMatrix,realRatingMatrix'
calcPredictionAccuracy(x, data, byUser = FALSE, ...)

# S4 method for class 'topNList,realRatingMatrix'
calcPredictionAccuracy(x, data, byUser = FALSE,
  given = NULL, goodRating = NA, ...)

# S4 method for class 'topNList,binaryRatingMatrix'
calcPredictionAccuracy(x, data, byUser = FALSE,
  given = NULL, ...)
```

## Arguments

- x:

  Predicted items in a "topNList" or predicted ratings as a
  "realRatingMatrix"

- data:

  Observed true ratings for the users as a "RatingMatrix". The users
  have to be in the same order as in `x`.

- byUser:

  logical; Should the accuracy measures be reported for each user
  individually instead of being averaged over all users?

- given:

  how many items were given to create the predictions. If the data comes
  from an evaluation scheme that usses all-but-x (i.e., a negative value
  for `give`), then a vector with the number of items actually given for
  each prediction needs to be supplied. This can be optained from the
  evaluation scheme `es` via `getData(es, "given")`.

- goodRating:

  If `x` is a "topNList" and `data` is a "realRatingMatrix" then
  `goodRating` is used as the threshold for determining what rating in
  `data` is considered a good rating.

- ...:

  further arguments.

## Details

The function calculates the accuracy of predictions compared to the
observed true ratings (`data`) averaged over the users. Use
`byUser = TRUE` to get the results for each user.

If both, the predictions are numeric ratings (i.e. a
"realRatingMatrix"), then the error measures RMSE, MSE and MAE are
calculated.

If the predictions are a "topNList", then the entries of the confusion
matrix (true positives TP, false positives FP, false negatives FN and
true negatives TN) and binary classification measures like precision,
recall, TPR and FPR are calculated. If data is a "realRatingMatrix",
then `goodRating` has to be specified to identify items that should be
recommended (i.e., have a rating of goodRating or more). Note that you
need to specify the number of items given to the recommender to create
predictions. The number of predictions by user (N) is the total number
of items in the data minus the number of given items. The number of TP
is limited by the size of the top-N list. Also, since the counts for TP,
FP, FN and TN are averaged over the users (unless `byUser = TRUE` is
used), they will not be whole numbers.

If the ratings are a "topNList" and the observed data is a
"realRatingMatrix" then `goodRating` is used to determine what rating in
`data` is considered a good rating for calculating binary classification
measures. This means that an item in the topNList is considered a true
positive if it has a rating of `goodRating` or better in the observed
data.

## Value

Returns a vector with the appropriate measures averaged over all users.
For `byUser=TRUE`, a matrix with a row for each user is returned.

## See also

[`topNList`](http://michael.hahsler.net/recommenderlab/reference/topNList-class.md),
[`binaryRatingMatrix`](http://michael.hahsler.net/recommenderlab/reference/binaryRatingMatrix-class.md),
[`realRatingMatrix`](http://michael.hahsler.net/recommenderlab/reference/realRatingMatrix-class.md).

## References

Asela Gunawardana and Guy Shani (2009). A Survey of Accuracy Evaluation
Metrics of Recommendation Tasks, Journal of Machine Learning Research
10, 2935-2962.

## Examples

``` r
### recommender for real-valued ratings
data(Jester5k)

## create 90/10 split (known/unknown) for the first 500 users in Jester5k
e <- evaluationScheme(Jester5k[1:500, ], method = "split", train = 0.9,
    k = 1, given = 15)
e
#> Evaluation scheme with 15 items given
#> Method: ‘split’ with 1 run(s).
#> Training set proportion: 0.900
#> Good ratings: NA
#> Data set: 500 x 100 rating matrix of class ‘realRatingMatrix’ with 36702 ratings.

## create a user-based CF recommender using training data
r <- Recommender(getData(e, "train"), "UBCF")

## create predictions for the test data using known ratings (see given above)
p <- predict(r, getData(e, "known"), type = "ratings")
p
#> 50 x 100 rating matrix of class ‘realRatingMatrix’ with 4192 ratings.

## compute error metrics averaged per user and then averaged over all
## recommendations
calcPredictionAccuracy(p, getData(e, "unknown"))
#>      RMSE       MSE       MAE 
#>  4.589385 21.062454  3.626658 
head(calcPredictionAccuracy(p, getData(e, "unknown"), byUser = TRUE))
#>            RMSE      MSE      MAE
#> u15241 3.292553 10.84091 2.472732
#> u3000  5.716413 32.67738 4.880794
#> u16962 4.957826 24.58004 4.013013
#> u9595  6.359003 40.43692 5.542118
#> u21612 4.917760 24.18436 4.186380
#> u15987 4.194666 17.59522 3.429465

## evaluate topNLists instead (you need to specify given and goodRating!)
p <- predict(r, getData(e, "known"), type = "topNList")
p
#> Recommendations as ‘topNList’ with n = 10 for 50 users. 
calcPredictionAccuracy(p, getData(e, "unknown"), given = 15, goodRating = 5)
#>         TP         FP         FN         TN          N  precision     recall 
#>  2.9000000  7.1000000 11.1400000 63.8600000 85.0000000  0.2900000  0.2383243 
#>        TPR        FPR 
#>  0.2383243  0.0982760 

## evaluate a binary recommender
data(MSWeb)
MSWeb10 <- sample(MSWeb[rowCounts(MSWeb) >10,], 50)

e <- evaluationScheme(MSWeb10, method="split", train = 0.9,
    k = 1, given = 3)
e
#> Evaluation scheme with 3 items given
#> Method: ‘split’ with 1 run(s).
#> Training set proportion: 0.900
#> Good ratings: NA
#> Data set: 50 x 285 rating matrix of class ‘binaryRatingMatrix’ with 683 ratings.

## create a user-based CF recommender using training data
r <- Recommender(getData(e, "train"), "UBCF")

## create predictions for the test data using known ratings (see given above)
p <- predict(r, getData(e, "known"), type="topNList", n = 10)
p
#> Recommendations as ‘topNList’ with n = 10 for 5 users. 

calcPredictionAccuracy(p, getData(e, "unknown"), given = 3)
#>           TP           FP           FN           TN            N    precision 
#>   0.40000000   9.60000000  11.40000000 260.60000000 282.00000000   0.04000000 
#>       recall          TPR          FPR 
#>   0.04040404   0.04040404   0.03554174 
calcPredictionAccuracy(p, getData(e, "unknown"), given = 3, byUser = TRUE)
#>   TP FP FN  TN   N precision     recall        TPR        FPR
#> 0  1  9  8 264 282       0.1 0.11111111 0.11111111 0.03296703
#> 1  0 10 10 262 282       0.0 0.00000000 0.00000000 0.03676471
#> 2  0 10 14 258 282       0.0 0.00000000 0.00000000 0.03731343
#> 3  1  9 10 262 282       0.1 0.09090909 0.09090909 0.03321033
#> 4  0 10 15 257 282       0.0 0.00000000 0.00000000 0.03745318
```
