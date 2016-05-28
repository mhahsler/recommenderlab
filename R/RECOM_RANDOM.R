## recommend random unknown items

RANDOM <- function(data=NULL, parameter=NULL) {

  model <- list(
    range = range(getRatings(data)),
    labels = colnames(data)
  )

  predict <- function(model=NULL, newdata, n=10,
    data= NULL, type=c("topNList", "ratings", "ratingMatrix"), ...) {

    type <- match.arg(type)

    ## newdata are userid
    if(is.numeric(newdata)) {
      if(is.null(data) || !is(data, "ratingMatrix"))
        stop("If newdata is a user id then data needes to be the training dataset.")
      newdata <- data[newdata, , drop = FALSE]
    }

    if(ncol(newdata) != length(model$labels)) stop("number of items in newdata does not match model.")

    newdata <- normalize(newdata, method = "Z-score")

    ## create random ratings (Z-scores)
    ratings <- matrix(rnorm(nrow(newdata)*ncol(newdata)),
      nrow=nrow(newdata), ncol=ncol(newdata),
      dimnames=list(NULL, model$labels))

    ratings <- new("realRatingMatrix", data=dropNA(ratings),
      normalize = getNormalize(newdata))
    ratings <- denormalize(ratings)

    ### check that min/max is not violated
    ratings@data@x[ratings@data@x<model$range[1]] <- model$range[1]
    ratings@data@x[ratings@data@x>model$range[2]] <- model$range[2]

    returnRatings(ratings, newdata, type, n)
  }

  ## this recommender has no model
  new("Recommender", method = "RANDOM", dataType = "realRatingMatrix",
    ntrain = nrow(data),
    model = model, predict = predict)
}

## register recommender
recommenderRegistry$set_entry(method="RANDOM", dataType="realRatingMatrix",
  fun=RANDOM,
  description="Produce random recommendations (real ratings).")

BIN_RANDOM <- function(data=NULL, parameter=NULL) {

  model <- list(
    labels = colnames(data)
  )

  predict <- function(model=NULL, newdata, n=10,
    data= NULL, type=c("topNList", "ratings", "ratingMatrix"), ...) {

    type <- match.arg(type)

    ## newdata are userid
    if(is.numeric(newdata)) {
      if(is.null(data) || !is(data, "ratingMatrix"))
        stop("If newdata is a user id then data needes to be the training dataset.")
      newdata <- data[newdata, , drop = FALSE]
    }

    if(ncol(newdata) != length(model$labels)) stop("number of items in newdata does not match model.")

    ratings <- new("realRatingMatrix",
      data = dropNA(matrix(runif(n = nrow(newdata)*ncol(newdata)),
        nrow = nrow(newdata))),
      normalize = NULL
    )
    rownames(ratings) <- rownames(newdata)
    colnames(ratings) <- model$labels

    returnRatings(ratings, newdata, type, n)
  }

  ## this recommender has no model
  new("Recommender", method = "RANDOM", dataType = "binaryRatingMatrix",
    ntrain = nrow(data),
    model = model, predict = predict)
}


recommenderRegistry$set_entry(method="RANDOM",
  dataType="binaryRatingMatrix", fun=BIN_RANDOM,
  description="Produce random recommendations (binary ratings).")

