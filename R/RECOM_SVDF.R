
### Funk SVD + folding in new users...

.REAL_SVDF_param <- list(
  k = 10,
  gamma = 0.015,
  lambda = 0.001,
  min_epochs = 50,
  max_epochs = 200,
  min_improvement = 1e-6,
  normalize  = "center",
  minRating  = NA,
  verbose = FALSE
)

REAL_SVDF <- function(data, parameter= NULL) {

  p <- .get_parameters(.REAL_SVDF_param, parameter)

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

    if(!is.null(model$normalize) && is(newdata, "realRatingMatrix"))
      newdata <- normalize(newdata, method=model$normalize)

    ratings <- predict.funkSVD(model$svd, as(newdata, "matrix"))
    ratings <- new("realRatingMatrix", data=dropNA(ratings))

    if(!is.null(model$normalize))
      ratings <- denormalize(ratings)

    if(type=="ratingMatrix") return(ratings)
    ratings <- removeKnownRatings(ratings, newdata)
    if(type=="ratings") return(ratings)

    getTopNLists(ratings, n=n, minRating=model$minRating)

  }

  ## construct recommender object
  new("Recommender", method = "SVDF", dataType = class(data),
    ntrain = nrow(data), model = model, predict = predict)
}

recommenderRegistry$set_entry(
  method="SVDF", dataType = "realRatingMatrix", fun=REAL_SVDF,
  description="Recommender based on Funk SVD with gradient descend (real data).",
  parameters = .REAL_SVDF_param)

#recommenderRegistry$set_entry(
#  method="SVD", dataType = "binaryRatingMatrix", fun=REAL_SVD,
#  description="Recommender based on EM-based SVD approximation from package bcv #(real data).",
#  parameters = .REAL_SVD_param)
