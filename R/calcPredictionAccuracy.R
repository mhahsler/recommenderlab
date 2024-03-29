setMethod("calcPredictionAccuracy", signature(x = "realRatingMatrix",
  data = "realRatingMatrix"),

  function(x, data, byUser = FALSE, ...) {
    if (byUser)
      fun <- rowMeans
    else
      fun <- mean

    ## we use matrix to make sure NAs are accounted for correctly
    MAE <- fun(abs(as(x, "matrix") - as(data, "matrix")),
      na.rm = TRUE)
    MSE <- fun((as(x, "matrix") - as(data, "matrix")) ^ 2,
      na.rm = TRUE)
    RMSE <- sqrt(MSE)

    drop(cbind(RMSE, MSE, MAE))
  })

setMethod("calcPredictionAccuracy", signature(x = "topNList",
  data = "realRatingMatrix"),

  function(x,
    data,
    byUser = FALSE,
    given = NULL,
    goodRating = NA,
    ...) {
    if (is.na(goodRating))
      stop("You need to specify goodRating!")

    data <- binarize(data, goodRating)
    calcPredictionAccuracy(x, data, byUser, given, ...)
  })

setMethod("calcPredictionAccuracy", signature(x = "topNList",
  data = "binaryRatingMatrix"),

  function(x,
    data,
    byUser = FALSE,
    given = NULL,
    ...) {
    if (is.null(given))
      stop("You need to specify how many items were given for the prediction!")

    if (any(given < 0))
      stop("For all-but-x schemes, you need to supply a vector with positive given values. This vector can be created from an evaluations scheme es by calling getData(es, 'given').")

    # given show up as a prediction of NA and a test data FALSE (TN)
    N <- ncol(data) - given

    TP <- rowSums(as(x, "ngCMatrix") * as(data, "ngCMatrix"))
    PredPositives <- rowSums(as(x, "ngCMatrix"))
    Positives <- rowSums(as(data, "ngCMatrix"))
    FP <- PredPositives - TP

    FN <- Positives - TP
    TN <- N - TP - FP - FN

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
    if (!byUser)
      res <- colMeans(res, na.rm = TRUE)

    res
  })
