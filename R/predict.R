setMethod("predict", signature(object = "Recommender"),
  function(object, newdata, n = 10, data = NULL, type = "topNList", ...) {

    if(!is(newdata, "ratingMatrix") && !is(newdata, "numeric"))
      stop("newdata needs to be a subclass of class ratingMatrix or a numeric vector with user IDs!")

    object@predict(object@model, newdata, n = n, data = data, type= type, ...)
  }
)

### helper to return ratings
returnRatings <- function(ratings, newdata,
  type=c("topNList", "ratings", "ratingMatrix"), n,
  randomize = NULL, minRating = NA) {

  type <- match.arg(type)

  ratings <- as(ratings, "realRatingMatrix")
  ratings <- denormalize(ratings)

  if(type == "ratingMatrix") {
    ### replace with known ratings. Removed. We return the approximation by the algorithm instead.
    #nm <- as(denormalize(newdata), "matrix")
    #rm <- as(ratings, "matrix")
    #rm[!is.na(nm)] <- nm[!is.na(nm)]
    #return(as(rm, "realRatingMatrix"))
    return(ratings)
  }

  ratings <- removeKnownRatings(ratings, newdata)
  if(type == "ratings") return(ratings)

  getTopNLists(ratings, n, randomize = randomize, minRating = minRating)
}
