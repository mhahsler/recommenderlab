## create a hybrid recommender


#' @title
#' Create a Hybrid Recommender
#'
#' @description Creates and combines recommendations using several recommender algorithms.
#'
#' @usage HybridRecommender(..., weights = NULL, aggregation_type = "sum")
#'
#' @param \dots  objects of class 'Recommender'.
#'
#' @param weights  weights for the recommenders. The recommenders are equally
#' 	weighted by default.
#'
#' @param aggregation_type  How are the recommendations aggregated. Options are "sum", "min", and
#' 		"max".
#'
#' @details     The hybrid recommender is initialized with a set of pretrained Recommender objects.
#'     Typically, the algorithms are trained using the same training set. If different
#'     training sets are used, then, at least the training sets
#'     need to have the same items in the same order.
#'
#'     Alternatively, hybrid recommenders can be created using the regular \code{Recommender()}
#'     interface. Here \code{method} is set to \code{HYBRID} and \code{parameter} contains
#'     a list with recommenders and weights. The recommenders are a list of recommender algorithms,
#'     each represented by a list with the elements \code{name} (the recommender method)
#'     and \code{parameters} (the algorithm parameters). This interface can be used with \code{evaluate()}.
#'
#'
#'     For creating recommendations (\code{predict}), each recommender algorithm
#'     is used to create ratings. The individual ratings are combined using
#'     a weighted sum where missing ratings are ignored. Weights can be specified in \code{weights}.
#'
#' @return An object of class 'Recommender'.
#'
#' @seealso \code{\linkS4class{Recommender}}
#'
#' @examples data("MovieLense")
#' MovieLense100 <- MovieLense[rowCounts(MovieLense) >100,]
#' train <- MovieLense100[1:100]
#' test <- MovieLense100[101:103]
#'
#' ## mix popular movies with a random recommendations for diversity and
#' ## rerecommend some movies the user liked.
#' recom <- HybridRecommender(
#'   Recommender(train, method = "POPULAR"),
#'   Recommender(train, method = "RANDOM"),
#'   Recommender(train, method = "RERECOMMEND"),
#'   weights = c(.6, .1, .3)
#'   )
#'
#' recom
#'
#' getModel(recom)
#'
#' as(predict(recom, test), "list")
#'
#' ## create a hybrid recommender using the regular Recommender interface.
#' ## This is needed to use hybrid recommenders with evaluate().
#' recommenders <- list(
#'   RANDOM = list(name = "POPULAR", param = NULL),
#'   POPULAR = list(name = "RANDOM", param = NULL),
#'   RERECOMMEND = list(name = "RERECOMMEND", param = NULL)
#' )
#'
#' weights <- c(.6, .1, .3)
#'
#' recom <- Recommender(train, method = "HYBRID",
#'   parameter = list(recommenders = recommenders, weights = weights))
#' recom
#'
#' as(predict(recom, test), "list")
#' @family recommender models
#' @name HybridRecommender
HybridRecommender <- function(..., weights = NULL, aggregation_type = "sum") {
  recommenders <- list(...)

  if(length(recommenders) < 1) stop("No base recommender specified!")

  if(is.null(weights)) weights <- rep(1, length(recommenders))
  else if(length(recommenders) != length(weights)) stop("Number of recommenders and length of weights do not agree!")
  weights <- weights/sum(weights)
  
  aggregation_fun <- switch (aggregation_type, 
                             "sum" = colSums,
                             "max" = colMaxs,
                             "min" = colMins
  )

  if(!all(sapply(recommenders, is, "Recommender"))) stop("Not all supplied models are of class 'Recommender'.")

  model <- list(recommenders = recommenders, weights = weights)

  predict <- function(model=NULL, newdata, n=10,
    data= NULL, type=c("topNList", "ratings", "ratingMatrix"), ...) {

    type <- match.arg(type)

    ## newdata are userid
    if(is.numeric(newdata)) {
      if(is.null(data) || !is(data, "ratingMatrix"))
        stop("If newdata is a user ID, data must be the training data set.")
      newdata <- data[newdata, , drop = FALSE]
    }

    #if(ncol(newdata) != length(model$labels)) stop("number of items in newdata does not match model.")

    pred <- lapply(model$recommenders, FUN = function(object)
      object@predict(object@model, newdata, data=data, type="ratings", ...))

    ratings <- matrix(NA, nrow=nrow(newdata), ncol = ncol(newdata))
    for(i in 1:nrow(pred[[1]])) {
      ### Ignore NAs!
      ratings[i,] <-
        aggregation_fun(t(sapply(pred, FUN = function(p)
          as(p[i,], "matrix"))) * model$weights, na.rm = TRUE)
      
      normalizer <- colSums(t(sapply(pred, FUN = function(p)
        !is.na(as(p[i,], "matrix")))) * model$weights, na.rm = TRUE)
      if(aggregation_type == "max" || aggregation_type == "min"){
        normalizer <- (normalizer > 0) * 1
      }
      ratings[i,] <- ratings[i,] / normalizer
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
  weights = NULL,
  aggregation_type = "sum"
)

HYBRID <- function(data, parameter = NULL) {
  p <- getParameters(.HYBRID_params, parameter)

  # build the individual recommenders
  recommenders <- lapply(parameter$recommenders, FUN = function(p)
    Recommender(data = data, method = p$name, parameter = p$param))

  do.call(HybridRecommender, c(recommenders, weights = list(p$weights), aggregation_type = list(p$aggregation_type)))
}

## register recommender
recommenderRegistry$set_entry(
  method="HYBRID", dataType = "realRatingMatrix", fun=HYBRID,
  description="Hybrid recommender that aggregates several recommendation strategies using weighted averages.",
  parameters=.HYBRID_params)

recommenderRegistry$set_entry(
  method="HYBRID", dataType = "binaryRatingMatrix", fun=HYBRID,
  description="Hybrid recommender that aggregates several recommendation strategies using weighted averages.",
  parameters=.HYBRID_params)
