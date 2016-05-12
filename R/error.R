
MAE <- function(true, predicted, na.rm = TRUE) {
  if(length(true) != length(predicted)) stop("length does not match!")
  mean(abs(true - predicted), na.rm = na.rm)
}

MSE <- function(true, predicted, na.rm = TRUE){
  if(length(true) != length(predicted)) stop("length does not match!")
  mean((true - predicted)^2, na.rm = na.rm)
}

RMSE <- function(true, predicted, na.rm = TRUE) {
  if(length(true) != length(predicted)) stop("length does not match!")
  mean((true - predicted)^2, na.rm = na.rm)^.5
}

frobenius <- function(true, predicted, na.rm = TRUE){
  if(is.null(dim(true)) || is.null(dim(predicted))) stop("matrix needed!")
  if(any(dim(true) != dim(predicted))) stop("matrix dimensions do not match!")
  RMSE(true, predicted, na.rm = na.rm)
}

