#' @title
#' Calculate Recommendations
#'
#' @description Creates recommendations using a recommender model and data about new users.
#' @aliases predict,Recommender-method
#'
#' @usage \S4method{predict}{Recommender}(object, newdata, n = 10, data=NULL,
#'     type="topNList", ...)
#'
#' @param object a recommender model (class \code{"Recommender"}).
#'
#' @param newdata data for active users (class \code{"ratingMatrix"})
#'    or the index of users in the training data to create recommendations for.
#'    If an index is used then some recommender algorithms need to be passed
#'    the training data as argument \code{data}. Some algorithms may only support
#'    user indices.
#'
#' @param n  number of recommendations in the top-N list.
#'
#' @param data  training data needed by some recommender algorithms if
#'     \code{newdata} is a user index and not user data.
#'
#' @param type  type of recommendation. The default type is
#'   \code{"topNList"} which creates
#'   a top-N recommendation list with recommendations.
#'   Some recommenders can also predict ratings with
#'   type \code{"ratings"} which returns only predicted ratings with
#'   known ratings represented by \code{NA},
#'   or type \code{"ratingMatrix"} which returns a completed rating matrix (Note that
#'   the predicted ratings may differ from the known ratings).
#'
#' @param \dots further arguments.
#'
#' @return Returns an object of class \code{"topNList"} or of other appropriate
#' classes.
#'
#' @seealso \code{\linkS4class{Recommender}},
#' \code{\linkS4class{ratingMatrix}}.
#'
#' @examples data("MovieLense")
#' MovieLense100 <- MovieLense[rowCounts(MovieLense) >100,]
#' train <- MovieLense100[1:50]
#'
#' rec <- Recommender(train, method = "POPULAR")
#' rec
#'
#' ## create top-N recommendations for new users
#' pre <- predict(rec, MovieLense100[101:102], n = 10)
#' pre
#' as(pre, "list")
#'
#' ## predict ratings for new users
#' pre <- predict(rec, MovieLense100[101:102], type="ratings")
#' pre
#' as(pre, "matrix")[,1:10]
#'
#'
#' ## create recommendations using user ids with ids 1..10 in the
#' ## training data
#' pre <- predict(rec, 1:10 , data = train, n = 10)
#' pre
#' as(pre, "list")
#' @family recommendations
#' @name predict
setMethod("predict", signature(object = "Recommender"),
  function(object,
    newdata,
    n = 10,
    data = NULL,
    type = "topNList",
    ...) {
    if (!is(newdata, "ratingMatrix") && !is(newdata, "numeric"))
      stop("newdata needs to be a subclass of class ratingMatrix or a numeric vector with user IDs!")

    object@predict(
      object@model,
      newdata,
      n = n,
      data = data,
      type = type,
      ...
    )
  })

### helper to return ratings
#' @title
#' Internal Utility Functions
#'
#' @description Utility functions used internally by recommender algorithms. See files starting
#' with \code{RECOM} in the package's \code{R} directory for examples of usage.
#' @aliases internalFunctions
#' @aliases returnRatings
#' @aliases getParameters
#'
#' @usage returnRatings(ratings, newdata,
#'   type = c("topNList", "ratings", "ratingMatrix"),
#'   n, randomize = NULL, minRating = NA)
#'
#' getParameters(defaults, parameter)
#'
#' @param ratings  a realRatingMatrix.
#'
#' @param newdata  a realRatingMatrix.
#'
#' @param type  type of recommendation to return.
#'
#' @param n  max. number of entries in the top-N list.
#'
#' @param randomize  randomization factor for producing the top-N list.
#'
#' @param minRating  do not include ratings less than this.
#'
#' @param defaults  list with parameters and default values.
#'
#' @param parameter  list with actual parameters.
#'
#' @details \code{returnRatings} is used in the predict function of recommender algorithms
#' to return different types of recommendations.
#'
#' \code{getParameters} is a helper function which checks parameters for
#' consistency and provides default values. Used in the Recommender constructor.
#' @name internal
#' @family internal
returnRatings <- function(ratings,
  newdata,
  type = c("topNList", "ratings", "ratingMatrix"),
  n,
  randomize = NULL,
  minRating = NA) {
  type <- match.arg(type)

  ratings <- as(ratings, "realRatingMatrix")
  ratings <- denormalize(ratings)

  if (type == "ratingMatrix") {
    ### replace with known ratings. Removed. We return the approximation by the algorithm instead.
    #nm <- as(denormalize(newdata), "matrix")
    #rm <- as(ratings, "matrix")
    #rm[!is.na(nm)] <- nm[!is.na(nm)]
    #return(as(rm, "realRatingMatrix"))
    return(ratings)
  }

  ratings <- removeKnownRatings(ratings, newdata)
  if (type == "ratings")
    return(ratings)

  getTopNLists(ratings, n, randomize = randomize, minRating = minRating)
}
