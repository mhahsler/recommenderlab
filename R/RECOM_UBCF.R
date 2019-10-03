# collaborative filtering

## simple k-nearest neighbor
.knn <- function(sim, k) apply(sim, MARGIN=1, FUN=function(x) head(
  order(x, decreasing=TRUE, na.last=TRUE), k))

.BIN_UBCF_param <- list(
  method = "jaccard",
  nn = 25,
  weighted = TRUE,
  sample = FALSE
)

BIN_UBCF <- function(data, parameter = NULL){

  p <- getParameters(.BIN_UBCF_param, parameter)

  if(p$sample) data <- sample(data, p$sample)

  model <- c(list(
    description = "UBCF-Binary Data: contains full or sample of data set",
    data = data
  ), p )

  predict <- function(model, newdata, n=10, data=NULL,
    type=c("topNList", "ratings", "ratingMatrix"), ...) {

    type <- match.arg(type)


    ## newdata are userid
    if(is.numeric(newdata)) {
      if(is.null(data) || !is(data, "ratingMatrix"))
        stop("If newdata is a user id then data needes to be the training dataset.")
      newdata <- data[newdata,]
    }

    if(ncol(newdata) != ncol(model$data)) stop("number of items in newdata does not match model.")

    ## prediction
    ## FIXME: add Weiss dissimilarity

    sim <- similarity(newdata, model$data,
      method = model$method)

    neighbors <- .knn(sim, model$nn)

    if(model$weighted) {
      ## similarity of the neighbors
      s_uk <- sapply(1:nrow(sim), FUN=function(x)
        sim[x, neighbors[,x]])

      ## calculate the weighted sum
      ratings <- t(sapply(1:nrow(newdata), FUN=function(i) {
        ## neighbors ratings of active user i
        r_neighbors <- as(model$data[neighbors[,i]], "dgCMatrix")
        ## normalize by the sum of weights only if a rating is available
      drop(as(crossprod(r_neighbors, s_uk[,i]), "matrix"))/drop(as(crossprod(!dropNAis.na(r_neighbors), s_uk[,i]), "matrix"))
      }))

    }else{
      ratings <- t(sapply(1:nrow(newdata), FUN=function(i) {
        colCounts(model$data[neighbors[,i]])
      }))
    }

    rownames(ratings) <- rownames(newdata)

    ratings <- new("realRatingMatrix", data=dropNA(ratings))
    ## prediction done

    returnRatings(ratings, newdata, type, n)
  }

  ## construct recommender object
  new("Recommender", method = "UBCF", dataType = class(data),
    ntrain = nrow(data), model = model, predict = predict)
}

.REAL_UBCF_param <- list(
  method = "cosine",
  nn = 25,
  sample = FALSE,
  weighted = TRUE,
  normalize="center"
)


REAL_UBCF <- function(data, parameter = NULL){

  p <- getParameters(.REAL_UBCF_param, parameter)

  if(p$sample) data <- sample(data, p$sample)

  ## normalize data
  if(!is.null(p$normalize)) data <- normalize(data, method=p$normalize)

  model <- c(list(
    description = "UBCF-Real data: contains full or sample of data set",
    data = data
  ), p)

  predict <- function(model, newdata, n=10,
    data=NULL, type=c("topNList", "ratings", "ratingMatrix"), ...) {

    type <- match.arg(type)

    ## newdata are userid
    if(is.numeric(newdata)) {
      if(is.null(data) || !is(data, "ratingMatrix"))
        stop("If newdata is a user id then data needes to be the training dataset.")
      newdata <- data[newdata,]
    }

    if(!is.null(model$normalize))
      newdata <- normalize(newdata, method=model$normalize)

    ## predict ratings
    sim <- similarity(newdata, model$data,
      method = model$method)

    neighbors <- .knn(sim, model$nn)

    ## r_ui = r_u_bar + [sum_k s_uk * r_ai - r_a_bar] / sum_k s_uk
    ## k is the neighborhood
    ## r_ai - r_a_bar_ is normalize(r_ai) = newdata

    if(model$weighted) { # average ratings weighted by similarity
      s_uk <- sapply(1:nrow(sim), FUN=function(x)
        sim[x, neighbors[,x]])

      ratings <- t(sapply(1:nrow(newdata), FUN=function(i) {
        ## neighbors ratings of active user i
        r_neighbors <- as(model$data[neighbors[,i]], "dgCMatrix")
        ## normalize by the sum of weights only if a rating is available
        drop(as(crossprod(r_neighbors, s_uk[,i]), "matrix"))/drop(as(crossprod(!dropNAis.na(r_neighbors), s_uk[,i]), "matrix"))
      }))
      ratings[!is.finite(ratings)] <- NA  ### make NaN into NA

    }else{ ### unweighted average
      ratings <- t(sapply(1:nrow(newdata), FUN=function(i) {
        ## neighbors ratings of active user i
        r_neighbors <- as(model$data[neighbors[,i]], "dgCMatrix")
        ## normalize by the sum of weights only if a rating is available
        colSums(r_neighbors)/colSums(!dropNAis.na(r_neighbors))
      }))
      ratings[!is.finite(ratings)] <- NA  ### make NaN into NA
    }


    ### Note: If no user in the neighborhood has a rating for the item then it is NA!


    rownames(ratings) <- rownames(newdata)
    ratings <- new("realRatingMatrix", data=dropNA(ratings),
      normalize = getNormalize(newdata))
    ratings <- denormalize(ratings)

    returnRatings(ratings, newdata, type, n)
  }

  ## construct recommender object
  new("Recommender", method = "UBCF", dataType = class(data),
    ntrain = nrow(data), model = model, predict = predict)
}


## register recommender
recommenderRegistry$set_entry(
  method="UBCF", dataType = "binaryRatingMatrix", fun=BIN_UBCF,
  description="Recommender based on user-based collaborative filtering.",
  parameters=.BIN_UBCF_param)


recommenderRegistry$set_entry(
  method="UBCF", dataType = "realRatingMatrix", fun=REAL_UBCF,
  description="Recommender based on user-based collaborative filtering.",
  parameters=.REAL_UBCF_param)


