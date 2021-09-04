## recommend random unknown items

RANDOM <- function(data = NULL, parameter = NULL) {

  model <- list(
    range = NA,
    labels = colnames(data)
  )

  if(is(data, "binaryRatingMatrix")) model$range <- c(0, 1)
  else {
    if(!is.null(parameter$range))  model$range <- parameter$range
    else model$range <- range(getRatings(data), na.rm = TRUE)
  }

  predict <- function(model = NULL, newdata, n = 10,
    data = NULL, type = c("topNList", "ratings", "ratingMatrix"), ...) {

    type <- match.arg(type)

    ## newdata are userid
    if(is.numeric(newdata)) {
      if(is.null(data) || !is(data, "ratingMatrix"))
        stop("If newdata is a user id then data needes to be the training dataset.")
      newdata <- data[newdata, , drop = FALSE]
    }

    if(ncol(newdata) != length(model$labels)) stop("number of items in newdata does not match model.")

    ### FIXME: topN lists could be created directly more memory efficient.

    ratings <- new("realRatingMatrix",
      data = dropNA(matrix(runif(n = nrow(newdata)*ncol(newdata),
        min = model$range[1], max = model$range[2]),
        nrow = nrow(newdata),
        dimnames=list(rownames(newdata), model$labels))),
      normalize = NULL
    )

    returnRatings(ratings, newdata, type, n)
  }

  ## this recommender has no model
  new("Recommender", method = "RANDOM", dataType = class(data),
    ntrain = nrow(data),
    model = model, predict = predict)
}

## register recommender
recommenderRegistry$set_entry(method = "RANDOM", dataType = "realRatingMatrix",
  fun = RANDOM,
  description = "Produce random recommendations (real ratings).")

recommenderRegistry$set_entry(method = "RANDOM",
  dataType = "binaryRatingMatrix",
  fun = RANDOM,
  description = "Produce random recommendations (binary ratings).")

