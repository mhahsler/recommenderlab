setMethod("calcPredictionAccuracy", signature(x= "realRatingMatrix",
		data = "realRatingMatrix"),

	function(x, data, byUser=FALSE, ...) {

      if(byUser) fun <- rowMeans
	    else fun <- mean

	    ## we use matrix to make sure NAs are accounted for correctly
	    MAE <- fun(abs(as(x, "matrix") - as(data,"matrix")),
		    na.rm=TRUE)
	    MSE <- fun((as(x, "matrix") - as(data,"matrix"))^2,
		    na.rm=TRUE)
	    RMSE <- sqrt(MSE)

	    drop(cbind(RMSE, MSE, MAE))
	})

setMethod("calcPredictionAccuracy", signature(x= "topNList",
  data = "realRatingMatrix"),

  function(x, data, byUser=FALSE, given=NULL, goodRating=NA, ...) {
    if(is.na(goodRating)) stop("You need to specify goodRating!")

    data <- binarize(data, goodRating)
    calcPredictionAccuracy(x, data, byUser, given,...)
})

setMethod("calcPredictionAccuracy", signature(x= "topNList",
  data = "binaryRatingMatrix"),

  function(x, data, byUser=FALSE, given=NULL, ...) {
    if(is.null(given)) stop("You need to specify how many items were given for the prediction!")

    N <- rep(ncol(data) - given, nrow(data))
    TP <- rowSums(as(x, "ngCMatrix") * as(data, "ngCMatrix"))
    FN <- rowCounts(data) - TP          # note: TP+FN = rowCounts(data)
    FP <- rowCounts(x) - TP             # note: TP+FP = rowCounts(x)
    TN <-  N - TP - FP - FN

    # Sum over test users
    #if(!byUser) {
    #  TP <- sum(TP, na.rm=TRUE)
    #  FP <- sum(FP, na.rm=TRUE)
    #  TN <- sum(TN, na.rm=TRUE)
    #  FN <- sum(FN, na.rm=TRUE)
    #  N  <- sum(N,  na.rm=TRUE)
    #}

    ## calculate some important measures
    precision <- TP / (TP + FP)
    recall <- TP / (TP + FN)
    TPR <- recall
    FPR <- FP / (FP + TN)

    res <- cbind(TP, FP, FN, TN, N, precision, recall, TPR, FPR)

    #Average over test users
    if(!byUser) res <- colMeans(res, na.rm=TRUE)

    res
    })

