## always recommends the top-N popular items (without known items)

BIN_POPULAR <- function(data, parameter = NULL) {

  topN <- new("topNList",
    items = list(order(colCounts(data), decreasing=TRUE)),
    itemLabels = colnames(data),
    n = ncol(data))

  model <- list( topN = topN )

  predict <- function(model, newdata, n=10,
    data=NULL,  type=c("topNList"),...) {

    type <- match.arg(type)

    if(is.numeric(newdata)) {
      if(is.null(data) || !is(data, "ratingMatrix"))
        stop("If newdata is a user id then data needes to be the training dataset.")
      newdata <- data[newdata,]
    }

    topN <- model$topN
    topN@items <- replicate(nrow(newdata), topN@items, simplify = TRUE)
    names(topN@items) <- rownames(newdata)
    topN <- removeKnownItems(topN, newdata)
    topN <- bestN(topN, n)
    return(topN)
  }

  ## construct recommender object
  new("Recommender", method = "POPULAR", dataType = class(data),
    ntrain = nrow(data), model = model, predict = predict)
}

## register recommender
recommenderRegistry$set_entry(
  method="POPULAR", dataType = "binaryRatingMatrix", fun=BIN_POPULAR,
  description="Recommender based on item popularity (binary data)."
)




## always recommends the top-N popular items (without known items)
REAL_POPULAR <- function(data, parameter = NULL) {

  p <- .get_parameters(list(
    normalize="center",
    aggregationRatings=colMeans,
    aggregationPopularity=colSums
  ), parameter)

  ## normalize data
  if(!is.null(p$normalize)) data <- normalize(data, method=p$normalize)

  topN <- new("topNList",
    items = list(order(p$aggregationPopularity(data), decreasing=TRUE)),
    itemLabels = colnames(data),
    n= ncol(data))

  ratings <- new("realRatingMatrix", data = dropNA(t(p$aggregationRatings(data))),
    normalize = data@normalize)

  model <- c(list(
    topN = topN,
    ratings = ratings
  ), p)

  predict <- function(model, newdata, n=10,
    data=NULL, type=c("topNList", "ratings", "ratingMatrix"), ...) {

    type <- match.arg(type)

    if(is.numeric(newdata)) {
      if(is.null(data) || !is(data, "ratingMatrix"))
        stop("If newdata is a user id then data needes to be the training dataset.")
      newdata <- data[newdata,]
    }

    if(type=="topNList") {
      topN <- model$topN
      topN@items <- replicate(nrow(newdata), topN@items, simplify = TRUE)
      names(topN@items) <- rownames(newdata)

      topN <- removeKnownItems(topN, newdata)
      topN <- bestN(topN, n)
      return(topN)
    }

    ratings <- denormalize(model$ratings)
    ratings <- ratings[rep(1L, nrow(newdata)),]

    returnRatings(ratings, newdata, type, n)
  }

  ## construct recommender object
  new("Recommender", method = "POPULAR", dataType = class(data),
    ntrain = nrow(data), model = model, predict = predict)
}

## register recommender
recommenderRegistry$set_entry(
  method="POPULAR", dataType = "realRatingMatrix", fun=REAL_POPULAR,
  description="Recommender based on item popularity (real data).")
