# Error Calculation

Calculate the mean absolute error (MAE), mean square error (MSE), root
mean square error (RMSE) and for matrices also the Frobenius norm
(identical to RMSE).

## Usage

``` r
MSE(true, predicted, na.rm = TRUE)
RMSE(true, predicted, na.rm = TRUE)
MAE(true, predicted, na.rm = TRUE)
frobenius(true, predicted, na.rm = TRUE)
```

## Arguments

- true:

  true values.

- predicted:

  predicted values

- na.rm:

  ignore missing values.

## Value

The error value.

## Details

Frobenius norm requires matrices.

## See also

Other evaluation:
[`calcPredictionAccuracy()`](http://michael.hahsler.net/recommenderlab/reference/calcPredictionAccuracy.md),
[`evaluate()`](http://michael.hahsler.net/recommenderlab/reference/evaluate.md),
[`evaluationResultList-class`](http://michael.hahsler.net/recommenderlab/reference/evaluationResultList-class.md),
[`evaluationResults-class`](http://michael.hahsler.net/recommenderlab/reference/evaluationResults-class.md),
[`evaluationScheme()`](http://michael.hahsler.net/recommenderlab/reference/evaluationScheme.md),
[`evaluationScheme-class`](http://michael.hahsler.net/recommenderlab/reference/evaluationScheme-class.md),
[`plot()`](http://michael.hahsler.net/recommenderlab/reference/plot.md)

## Examples

``` r
true <- rnorm(10)
predicted <- rnorm(10)

MAE(true, predicted)
#> [1] 1.284605
MSE(true, predicted)
#> [1] 3.156311
RMSE(true, predicted)
#> [1] 1.776601

true <- matrix(rnorm(9), nrow = 3)
predicted <- matrix(rnorm(9), nrow = 3)

frobenius(true, predicted)
#> [1] 1.124603
```
