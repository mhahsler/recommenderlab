
### SVD with column mean imputation

.REAL_SVD_param <- list(
  k = 10,                     ## rank of approximation
  maxiter    = 100,           ## max. number of SVD iterations
  normalize  = "center"      ## rows
)

REAL_SVD <- function(data, parameter= NULL) {

  p <- getParameters(.REAL_SVD_param, parameter)

  ### row normalization?
  if(!is.null(p$normalize) && is(data, "realRatingMatrix"))
    data <- normalize(data, method=p$normalize)

  m <- as(data, "matrix")

  ### do column mean imputation
  means <- colMeans(m, na.rm = TRUE)
  ### remaining NAs have no ratings! We give it the smallest possible rating.
  means[is.na(means)] <- min(m, na.rm = TRUE)

  nas <- is.na(m)
  m[nas] <- rep(means, colSums(nas))

  ### get final truncated SVD decomposition
  svd <- irlba::irlba(m, nv = p$k, maxit = p$maxiter)

  model <- c(list(
    description = "Truncated SVD",
    svd = svd,
    columnMeans = means ### needed for imputation
  ), p)

  predict <- function(model, newdata, n = 10,
    data=NULL, type=c("topNList", "ratings", "ratingMatrix"), ...) {

    type <- match.arg(type)
    n <- as.integer(n)

    ### newdata are userid
    if(is.numeric(newdata)) {
      if(is.null(data) || !is(data, "ratingMatrix"))
        stop("If newdata is a user id then you need to specify data.")
      newdata <- data[newdata, , drop=FALSE]
    }

    if(!is.null(model$normalize) && is(newdata, "realRatingMatrix"))
      newdata <- normalize(newdata, method=model$normalize)

    if(ncol(newdata) != nrow(model$svd$v)) stop("number of items in newdata does not match model.")

    ### reconstruct full rating matrix R = U Sigma V^T
    #r <- svd$u %*% tcrossprod(diag(svd$d), svd$v)

    ### folding in new user u_a = Sigma^-1 V^T r_a
    r_a <- as(newdata@data, "matrix")

    ### impute missing ratings
    nas <- is.na(r_a)
    r_a[nas] <- rep(model$columnMeans, colSums(nas))

    u_a <- r_a %*% model$svd$v %*% diag(1/model$svd$d)

    ### prediction p_a,i = u_a Sigma V^T
    ratings <- as(u_a %*% tcrossprod(diag(model$svd$d), model$svd$v), "matrix")

    #rownames(ratings) <- rownames(newdata)
    colnames(ratings) <- names(model$columnMeans)

    ratings <- new("realRatingMatrix", data=dropNA(ratings),
      normalize = getNormalize(newdata))
    ratings <- denormalize(ratings)

    returnRatings(ratings, newdata, type, n)
  }

  ## construct recommender object
  new("Recommender", method = "SVD", dataType = class(data),
    ntrain = nrow(data), model = model, predict = predict)
}

recommenderRegistry$set_entry(
  method="SVD", dataType = "realRatingMatrix", fun=REAL_SVD,
  description="Recommender based on SVD approximation with column-mean imputation.",
  parameters = .REAL_SVD_param)


