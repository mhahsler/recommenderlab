

#' @title
#' Error Calculation
#'
#' @description Calculate the mean absolute error (MAE), mean square error (MSE),
#' root mean square error (RMSE) and for matrices also the Frobenius norm (identical to RMSE).
#' @aliases Error
#' @aliases RMSE
#' @aliases frobenius
#' @aliases MSE
#' @aliases MAE
#'
#' @usage MSE(true, predicted, na.rm = TRUE)
#' RMSE(true, predicted, na.rm = TRUE)
#' MAE(true, predicted, na.rm = TRUE)
#' frobenius(true, predicted, na.rm = TRUE)
#'
#' @param true  true values.
#'
#' @param predicted  predicted values
#'
#' @param na.rm  ignore missing values.
#'
#' @details Frobenius norm requires matrices.
#'
#' @return The error value.
#'
#' @examples true <- rnorm(10)
#' predicted <- rnorm(10)
#'
#' MAE(true, predicted)
#' MSE(true, predicted)
#' RMSE(true, predicted)
#'
#' true <- matrix(rnorm(9), nrow = 3)
#' predicted <- matrix(rnorm(9), nrow = 3)
#'
#' frobenius(true, predicted)
#' @family evaluation
#' @name error
MAE <- function(true, predicted, na.rm = TRUE) {
  if (length(true) != length(predicted))
    stop("length does not match!")
  mean(abs(true - predicted), na.rm = na.rm)
}

MSE <- function(true, predicted, na.rm = TRUE) {
  if (length(true) != length(predicted))
    stop("length does not match!")
  mean((true - predicted) ^ 2, na.rm = na.rm)
}

RMSE <- function(true, predicted, na.rm = TRUE) {
  if (length(true) != length(predicted))
    stop("length does not match!")
  mean((true - predicted) ^ 2, na.rm = na.rm) ^ .5
}

frobenius <- function(true, predicted, na.rm = TRUE) {
  if (is.null(dim(true)) ||
      is.null(dim(predicted)))
    stop("matrix needed!")
  if (any(dim(true) != dim(predicted)))
    stop("matrix dimensions do not match!")
  RMSE(true, predicted, na.rm = na.rm)
}
