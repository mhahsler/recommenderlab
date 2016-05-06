## always recommends the top-N popular items (without known items)
BIN_POPULAR <- function(data, parameter = NULL) {
  
  topN <- new("topNList", 
    items = list(order(colCounts(data), decreasing=TRUE)),
    itemLabels = colnames(data),
    n = ncol(data))
  
  model <- list( topN = topN )
  
  predict <- function(model, newdata, n=10, 
    data=NULL,  type=c("topNList", "ratings"),...) {
    
    type <- match.arg(type)
    if(type != "topNList") stop("POPULAR for binary data only supports topNList.")
    
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
    aggregation=colSums ## could also be colMeans
  ), parameter)
  
  ## normalize data
  if(!is.null(p$normalize)) data <- normalize(data, method=p$normalize)
  
  topN <- new("topNList", 
    items = list(order(p$aggregation(data), decreasing=TRUE)),
    itemLabels = colnames(data),
    n= ncol(data))
  
  ratings <- new("realRatingMatrix", data = dropNA(t(colMeans(data))), 
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
    
    ### FIXME: 
    ## ratings always use colMeans!
    #if(!is.null(model$normalize)) 
    #    newdata <- normalize(newdata, method=model$normalize)
    # ratings <- denormalize(ratings, factors=getNormalize(newdata))
    
    ratings <- denormalize(ratings)
    
    ## make one row for each new user
    triplets <- as(ratings@data, "dgTMatrix")
    triplets@i <- rep(0:(nrow(newdata)-1), each = length(triplets@i))
    triplets@j <- rep(triplets@j, times = nrow(newdata))
    triplets@x <- rep(triplets@x, times = nrow(newdata))
    triplets@Dim[1] <- nrow(newdata)
    triplets@Dimnames[[1]] <- rownames(newdata)
    ratings@data <- as(triplets, "dgCMatrix")
    
    if(type=="ratingMatrix") return(ratings)
    
    ratings <- removeKnownRatings(ratings, newdata)
    
    return(ratings)
  }
  
  ## construct recommender object
  new("Recommender", method = "POPULAR", dataType = class(data),
    ntrain = nrow(data), model = model, predict = predict)
}

## register recommender
recommenderRegistry$set_entry(
  method="POPULAR", dataType = "realRatingMatrix", fun=REAL_POPULAR, 
  description="Recommender based on item popularity (real data).")
