## create a hybrid recommender


HybridRecommender <- function(..., weights = NULL) {
  recommenders <- list(...)

  if(length(recommenders) < 1) stop("No base recommender specified!")

  if(is.null(weights)) weights <- rep(1, length(recommenders))
  else if(length(recommenders) != length(weights)) stop("Number of recommenders and length of weights do not agree!")
  weights <- weights/sum(weights)

  if(!all(sapply(recommenders, is, "Recommender"))) stop("Not all supplied models are of class 'Recommender'.")

  model <- list(recommenders = recommenders, weights = weights)

  predict <- function(model=NULL, newdata, n=10,
    data= NULL, type=c("topNList", "ratings", "ratingMatrix"), ...) {

    type <- match.arg(type)

    ## newdata are userid
    if(is.numeric(newdata)) {
      if(is.null(data) || !is(data, "ratingMatrix"))
        stop("If newdata is a user id then data needes to be the training dataset.")
      newdata <- data[newdata, , drop = FALSE]
    }

    #if(ncol(newdata) != length(model$labels)) stop("number of items in newdata does not match model.")

    pred <- lapply(model$recommenders, FUN = function(object)
      object@predict(object@model, newdata, data=data, type="ratings", ...))

    ratings <- matrix(NA, nrow=nrow(newdata), ncol = ncol(newdata))
    for(i in 1:nrow(pred[[1]])) {
      ### Ignore NAs!
      ratings[i,] <-
        colSums(t(sapply(pred, FUN = function(p)
          as(p[i,], "matrix"))) * model$weights, na.rm = TRUE) /
        colSums(t(sapply(pred, FUN = function(p)
          !is.na(as(p[i,], "matrix")))) * model$weights, na.rm = TRUE)

    }
    ratings[!is.finite(ratings)] <- NA

    dimnames(ratings) <- dimnames(newdata)

    ratings <- as(ratings, "realRatingMatrix")
    colnames(ratings) <- colnames(newdata)

    if(type == "ratingMatrix")
      stop("Hybrid cannot predict a complete ratingMatrix!")

    returnRatings(ratings, newdata, type, n)
  }

  ## this recommender has no model
  new("Recommender", method = "HYBRID",
    dataType = "ratingMatrix",
    ntrain = recommenders[[1]]@ntrain,  ### take training set size from firs recommender
    model = model,
    predict = predict)
}


## recommender interface
.HYBRID_params <- list(
  recommenders = NULL,
  weights = NULL
)

HYBRID <- function(data, parameter = NULL) {
  p <- getParameters(.HYBRID_params, parameter)

  # build the individual recommenders
  recommenders <- lapply(parameter$recommenders, FUN = function(p)
    Recommender(data = data, method = p$name, parameter = p$param))

  do.call(HybridRecommender, c(recommenders, weights = list(p$weights)))
}

## register recommender
recommenderRegistry$set_entry(
  method="HYBRID", dataType = "realRatingMatrix", fun=HYBRID,
  description="Hybrid recommender that aggegates several recommendation strategies using weighted averages.",
  parameters=.HYBRID_params)

recommenderRegistry$set_entry(
  method="HYBRID", dataType = "binaryRatingMatrix", fun=HYBRID,
  description="Hybrid recommender that aggegates several recommendation strategies using weighted averages.",
  parameters=.HYBRID_params)
