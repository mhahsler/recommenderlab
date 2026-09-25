# Funk SVD for Matrices with Missing Data

Implements matrix decomposition by the stochastic gradient descent
optimization popularized by Simon Funk to minimize the error on the
known values. This function is used by the recommender method "SVDF"
(see
[`Recommender`](http://michael.hahsler.net/recommenderlab/reference/Recommender.md)).

## Usage

``` r
funkSVD(x, k = 10, gamma = 0.015, lambda = 0.001,
  min_improvement = 1e-06, min_epochs = 50, max_epochs = 200,
  verbose = FALSE)
```

## Arguments

- x:

  a matrix, potentially containing NAs.

- k:

  number of features (i.e, rank of the approximation).

- gamma:

  regularization term.

- lambda:

  learning rate.

- min_improvement:

  required minimum improvement per iteration.

- min_epochs:

  minimum number of iterations per feature.

- max_epochs:

  maximum number of iterations per feature.

- verbose:

  show progress.

## Details

Funk SVD decomposes a matrix (with missing values) into two components
\\U\\ and \\V\\. The singular values are folded into these matrices. The
approximation for the original matrix can be obtained by \\R = UV'\\.

This function `predict` in this implementation folds in new data rows by
estimating the \\u\\ vectors using gradient descend and then calculating
the reconstructed complete matrix r for these users via \\r = uV'\\.

## Value

An object of class `"funkSVD"` with components

- U:

  the \\U\\ matrix.

- V:

  the \\V\\ matrix.

- parameters:

  a list with parameter values.

## References

Y. Koren, R. Bell, and C. Volinsky. Matrix Factorization Techniques for
Recommender Systems, IEEE Computer, pp. 42-49, August 2009.

## Note

The code is based on the implmentation in package rrecsys by Ludovik
Coba and Markus Zanker.

## Examples

``` r
# this takes a while to run!
if (FALSE) { # \dontrun{
data("Jester5k")

# helper to calculate root mean squared error
rmse <- function(pred, truth) sqrt(sum((truth-pred)^2, na.rm = TRUE))

train <- as(Jester5k[1:100], "matrix")
fsvd <- funkSVD(train, verbose = TRUE)

# reconstruct the original rating matrix as R = UV'
r <- tcrossprod(fsvd$U, fsvd$V)
rmse(train, r)

# fold in new users for matrix completion
test <- as(Jester5k[101:105], "matrix")
p <- predict(fsvd, test, verbose = TRUE)
rmse(test, p)
} # }
```
