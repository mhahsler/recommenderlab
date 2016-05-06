### FIXME 
###  * This stores the complete matrix and then does the calculaiton
###    in predict!
###  * Stochastic Gradient Decent would be nice!

### used impute.svd from bcv which implements an EM-based imputation

.REAL_SVD_param <- list(
  approxRank = NA,                  ## rank of approximation
  maxiter    = 100,           ## max. number of SVD iterations
  normalize  = "center",
  minRating  = NA
)


REAL_SVD <- function(data, parameter= NULL) {
  
  p <- .get_parameters(.REAL_SVD_param, parameter)
  
  if(!is.null(p$normalize))
    data <- normalize(data, method=p$normalize)
  
  model <- c(list(
    description = "full matrix",
    data = data
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
    
    # Perform SVD
    data <- model$data@data
    data <- rBind(newdata@data, data)
    
    data <- as(data, "matrix")
    
    if(!is.na(p$approxRank)) {
      s <- impute.svd(data, k=p$approxRank, maxiter = p$maxiter)$x
    } else { 
      s <- impute.svd(data, maxiter = p$maxiter)$x 
    }
    
    ratings <- s[1:nrow(newdata),]
    
    rownames(ratings) <- rownames(newdata)
    colnames(ratings) <- colnames(data)
    ratings <- new("realRatingMatrix", data=dropNA(ratings))
    
    if(!is.null(model$normalize))
      ratings <- denormalize(ratings)
    
    if(type=="ratingMatrix") return(ratings)
    
    ratings <- removeKnownRatings(ratings, newdata)
    
    if(type=="ratings") return(ratings)
    
    getTopNLists(ratings, n=n, minRating=model$minRating)
    
  }
  
  ## construct recommender object
  new("Recommender", method = "SVD", dataType = class(data),
    ntrain = nrow(data), model = model, predict = predict)
}

recommenderRegistry$set_entry(
  method="SVD", dataType = "realRatingMatrix", fun=REAL_SVD,
  description="Recommender based on EM-based SVD approximation from package bcv (real data).",
  parameters = .REAL_SVD_param)

recommenderRegistry$set_entry(
  method="SVD", dataType = "binaryRatingMatrix", fun=REAL_SVD,
  description="Recommender based on EM-based SVD approximation from package bcv (real data).",
  parameters = .REAL_SVD_param)


