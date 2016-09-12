
### Funk SVD + folding in new users...

.REAL_SVDF_param <- list(
  k = 10,
  gamma = 0.015,
  lambda = 0.001,
  min_epochs = 50,
  max_epochs = 200,
  min_improvement = 1e-6,
  normalize  = "center",
  verbose = FALSE
)

REAL_SVDF <- function(data, parameter= NULL) {

  p <- getParameters(.REAL_SVDF_param, parameter)

  ### row normalization?
  if(!is.null(p$normalize) && is(data, "realRatingMatrix"))
    data <- normalize(data, method=p$normalize)

  svd <- funkSVD(as(data, "matrix"), k =p$k, gamma = p$gamma, lambda = p$lambda,
    min_epochs = p$min_epochs, max_epochs= p$max_epochs,
    min_improvement = p$min_improvement,
    verbose = p$verbose)

  model <- c(list(
    description = "Truncated Funk SVD",
    svd = svd
  ), p)

  predict <- function(model, newdata, n = 10,
    data=NULL, type=c("topNList", "ratings", "ratingMatrix"), ...) {

    type <- match.arg(type)
    n <- as.integer(n)

    ### newdata are userid
    if(is.vector(newdata)) {
      if(is.null(data) || !is(data, "ratingMatrix"))
        stop("If newdata is a user id then you need to specify data.")
      newdata <- data[newdata, , drop=FALSE]
    }

    if(ncol(newdata) != nrow(model$svd$V)) stop("number of items in newdata does not match model.")

    if(!is.null(model$normalize) && is(newdata, "realRatingMatrix"))
      newdata <- normalize(newdata, method=model$normalize)

    ratings <- predict.funkSVD(model$svd, as(newdata, "matrix"))

    ratings <- new("realRatingMatrix", data=dropNA(ratings),
      normalize = getNormalize(newdata))
    ratings <- denormalize(ratings)

    returnRatings(ratings, newdata, type, n)
  }

  ## construct recommender object
  new("Recommender", method = "SVDF", dataType = class(data),
    ntrain = nrow(data), model = model, predict = predict)
}

recommenderRegistry$set_entry(
  method="SVDF", dataType = "realRatingMatrix", fun=REAL_SVDF,
  description="Recommender based on Funk SVD with gradient descend.",
  parameters = .REAL_SVDF_param)

