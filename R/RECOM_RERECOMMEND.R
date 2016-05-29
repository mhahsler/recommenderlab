## rerecommends highly rated items

.RERECOMMEND_params <- list(
  randomize = 1,
  minRating = NA
)

RERECOMMEND <- function(data=NULL, parameter=NULL) {

  p <- getParameters(.RERECOMMEND_params, parameter)

  model <- c(list(labels = colnames(data)), p)

  predict <- function(model=NULL, newdata, n=10,
    data= NULL, type=c("topNList", "ratings", "ratingMatrix"), ...) {

    type <- match.arg(type)

    if(type == "ratingMatrix")
      stop("Rerecommender cannot predict a complete ratingMatrix!")

    ## newdata are userid
    if(is.numeric(newdata)) {
      if(is.null(data) || !is(data, "ratingMatrix"))
        stop("If newdata is a user id then data needes to be the training dataset.")
      newdata <- data[newdata, , drop = FALSE]
    }

    if(ncol(newdata) != length(model$labels)) stop("number of items in newdata does not match model.")

    ## just reuse user ratings
    ratings <- newdata

    if(type == "ratings") return(ratings)

    getTopNLists(ratings, n,
      minRating = model$minRating,
      randomize = model$randomize)
  }

  ## this recommender has no model
  new("Recommender", method = "RERECOMMEND",
    dataType = "ratingMatrix",
    ntrain = nrow(data),
    model = model,
    predict = predict)
}

## register recommender
recommenderRegistry$set_entry(method="RERECOMMEND",
  dataType="realRatingMatrix",
  fun=RERECOMMEND,
  description="Re-recommends highly rated items (real ratings).",
  parameters = .RERECOMMEND_params)

### FIXME: need to implement

#recommenderRegistry$set_entry(method="RERECOMMEND", dataType="binaryRatingMatrix",
#  fun=RANDOM,
#  description="Re-recommends items (binary ratings).",
#  parameters = .RERECOMMEND_params)

