### Contributed by Saurabh Bathnagar (sbhatnagar@book.com)

.REAL_PCA_param <- list(
  categories = 20,
  method="Cosine",
  normalize = "center",
  normalize_sim_matrix = FALSE,
  alpha = 0.5,
  na_as_zero = FALSE,
  minRating = NA
)

REAL_PCA <- function(data, parameter= NULL) {
  
  p <- .get_parameters(.REAL_PCA_param, parameter)
  
  
  if(!is.null(p$normalize))
    data <- normalize(data, method=p$normalize)
  
  # Perform PCA
  data <- data@data
  pcv<-princomp(data, cor=TRUE)
  # Get the loadings
  lpcv<-loadings(pcv)
  
  # Total number of categories
  cats <- min(dim(lpcv)[2], p$categories)
  
  #   det(lpcv[,1:99] %*% t(lpcv[,1:99]))
  
  # Convert to right type
  itemcat <- new("realRatingMatrix", 
                 data = as(lpcv[,1:cats], "dgCMatrix"))
  
  model <- c(list(
    description = "PCA: Reduced item-category matrix",
    itemcat = itemcat
  ), p)
  
  predict <- function(model, newdata, n = 10,
                      data=NULL, type=c("topNList", "ratings", "ratingMatrix"), ...) {
    
    type <- match.arg(type)
    
    ## newdata are userid
    if(is.numeric(newdata)) {
      if(is.null(data) || !is(data, "ratingMatrix"))
        stop("If newdata is a user id then data needes to be the training dataset.")
      newdata <- data[newdata,]
    }
    
    n <- as.integer(n)
    
    if(!is.null(model$normalize))
      newdata <- normalize(newdata, method=model$normalize)
    
    ## predict all ratings
    u <- as(newdata, "dgCMatrix")
    itemcat <- as(model$itemcat, "dgCMatrix")
    ratings <- u %*% itemcat  %*% t(itemcat)
    
    ratings <- new("realRatingMatrix", data=dropNA(ratings),
                   normalize = getNormalize(newdata))
    ## prediction done
    
    
    if(!is.null(model$normalize))
      ratings <- denormalize(ratings)
    
    rownames(ratings) <- rownames(newdata)
    
    if(type=="ratingMatrix") return(ratings)
    
    ratings <- removeKnownRatings(ratings, newdata)
    
    if(type=="ratings") return(ratings)
    
    getTopNLists(ratings, n=n, minRating=model$minRating)
    
  }
  
  ## construct recommender object
  new("Recommender", method = "PCA", dataType = class(data),
      ntrain = nrow(data), model = model, predict = predict)
}

recommenderRegistry$set_entry(
  method="PCA", dataType = "realRatingMatrix", fun=REAL_PCA,
  description="Recommender based on PCA approximation (real data).",
  parameters = .REAL_PCA_param)

