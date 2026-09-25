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

## Details

Frobenius norm requires matrices.

## Value

The error value.

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
