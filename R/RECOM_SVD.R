
### uses impute.svd from bcv which implements an EM-based imputation
### uses irlba for fast truncated SVD

.REAL_SVD_param <- list(
  approxRank = 10,            ## rank of approximation
  maxiter    = 100,           ## max. number of SVD iterations
  normalize  = "center",
  minRating  = NA
)

REAL_SVD <- function(data, parameter= NULL) {

  p <- .get_parameters(.REAL_SVD_param, parameter)

  ### row normalization?
  if(!is.null(p$normalize))
    data <- normalize(data, method=p$normalize)

  ### use SVD imputation (this is slow)
  if(!is.na(p$approxRank)) {
    s <- bcv::impute.svd(data@data, k=p$approxRank, maxiter = p$maxiter)$x
  } else {
    s <- bcv::impute.svd(data@data, maxiter = p$maxiter)$x
  }

  ### get final truncated SVD decomposition
  s <- irlba::irlba(s, nv = p$approxRank, maxit = p$maxiter)

  model <- c(list(
    description = "Truncated SVD",
    svd = s
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

    if(!is.null(model$normalize))
      newdata <- normalize(newdata, method=model$normalize)

    ### reconstruct full rating matrix R = U Sigma V^T
    #r <- svd$u %*% tcrossprod(diag(svd$d), svd$v)

    ### folding in new user u_a = Sigma^-1 V^T r_a
    r_a <- newdata@data
    u_a <- r_a %*% model$svd$v %*% diag(1/model$svd$d)

    ### prediction p_a,i = u_a Sigma V^T
    ratings <- as(u_a %*% tcrossprod(diag(model$svd$d), model$svd$v), "matrix")

    rownames(ratings) <- rownames(newdata)
    colnames(ratings) <- colnames(newdata)
    ratings <- new("realRatingMatrix", data=dropNA(ratings))

    if(!is.null(model$normalize))
      ratings <- denormalize(ratings)

    if(type=="ratingMatrix") return(ratings)
    ratings <- removeKnownRatings(ratings, newdata)
    if(type=="ratings") return(ratings)
    getTopNLists(ratings, n=n, minRating=model$minRating)

  }

  ## construct recommender object
  new("Recommender", method = "SVD", dataType = class(data),
    ntrain = nrow(data), model = model, predict = predict)
}

recommenderRegistry$set_entry(
  method="SVD", dataType = "realRatingMatrix", fun=REAL_SVD,
  description="Recommender based on EM-based SVD approximation from package bcv (real data).",
  parameters = .REAL_SVD_param)

recommenderRegistry$set_entry(
  method="SVD", dataType = "binaryRatingMatrix", fun=REAL_SVD,
  description="Recommender based on EM-based SVD approximation from package bcv (real data).",
  parameters = .REAL_SVD_param)


