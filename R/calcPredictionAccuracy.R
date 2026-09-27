#' @title
#'   Calculate the Prediction Error for a Recommendation
#'
#' @description Calculate prediction accuracy. For predicted ratings,
#'   MAE (mean absolute error), MSE (mean squared error), and RMSE (root mean
#'   squared error) are calculated. For top-N lists, binary classification
#'   metrics such as precision, recall, TPR, and FPR are returned.
#' @aliases calcPredictionAccuracy,realRatingMatrix,realRatingMatrix-method
#' @aliases calcPredictionAccuracy,topNList,binaryRatingMatrix-method
#' @aliases calcPredictionAccuracy,topNList,realRatingMatrix-method
#'
#' @usage calcPredictionAccuracy(x, data, ...)
#'
#' \S4method{calcPredictionAccuracy}{realRatingMatrix,realRatingMatrix}(x, data, byUser = FALSE, ...)
#'
#' \S4method{calcPredictionAccuracy}{topNList,realRatingMatrix}(x, data, byUser = FALSE,
#'   given = NULL, goodRating = NA, ...)
#'
#' \S4method{calcPredictionAccuracy}{topNList,binaryRatingMatrix}(x, data, byUser = FALSE,
#'   given = NULL, ...)
#'
#' @param x  Predicted items in a "topNList" or predicted ratings as a "realRatingMatrix"
#'
#' @param data  Observed true ratings for the users as a "RatingMatrix". The users have to be in the same order as in \code{x}.
#'
#' @param byUser  logical; Should the accuracy measures be reported for each user individually instead of being averaged over all users?
#'
#' @param given  How many items were given to create the predictions. If the data comes from an all-but-x evaluation scheme (i.e., a negative value for \code{given}), supply a vector containing the number of items given for each prediction.
#'   This can be obtained from the evaluation scheme \code{es} using \code{getData(es, "given")}.
#'
#' @param goodRating  If \code{x} is a "topNList" and \code{data} is a "realRatingMatrix" then \code{goodRating} is used as the threshold for determining what rating in \code{data} is considered a good rating.
#'
#' @param ...  further arguments.
#'
#' @details The function calculates the accuracy of predictions compared to the observed true ratings (\code{data}) averaged over the users. Use \code{byUser = TRUE} to get the results for each user.
#'
#' If the predictions are numeric ratings (i.e., a "realRatingMatrix"),
#' the error measures RMSE, MSE, and MAE are calculated.
#'
#' If the predictions are a "topNList", the confusion matrix entries (true positives TP, false positives FP, false negatives FN, and true negatives TN) and binary classification measures such as precision, recall, TPR, and FPR are calculated. If the data is a "realRatingMatrix", then
#' \code{goodRating} must be specified to identify items that should be recommended (i.e., those with ratings at or above this threshold).
#' Note that you need to specify the number of items given to the recommender to create predictions.
#' The number of predictions by user (N) is the total number of items in the data minus the number of given items. The number of TP is limited by the size of the top-N list. Also, since the counts for TP, FP, FN and TN are averaged over the users (unless \code{byUser = TRUE} is used),
#' they will not be whole numbers.
#'
#' If the predictions are a "topNList" and the observed data is a "realRatingMatrix", \code{goodRating} determines which ratings in \code{data} count as good for the binary classification measures. An item in the top-N list counts as a true positive if its observed rating is at least \code{goodRating}.
#'
#' @return Returns a vector with the appropriate measures averaged over all users.
#' For \code{byUser=TRUE}, a matrix with a row for each user is returned.
#'
#' @references Asela Gunawardana and Guy Shani (2009). A Survey of Accuracy Evaluation Metrics of
#' Recommendation Tasks, Journal of Machine Learning Research 10, 2935-2962.
#'
#' @seealso \code{\linkS4class{topNList}},
#' \code{\linkS4class{binaryRatingMatrix}},
#' \code{\linkS4class{realRatingMatrix}}.
#'
#' @examples ### recommender for real-valued ratings
#' data(Jester5k)
#'
#' ## create 90/10 split (known/unknown) for the first 500 users in Jester5k
#' e <- evaluationScheme(Jester5k[1:500, ], method = "split", train = 0.9,
#'     k = 1, given = 15)
#' e
#'
#' ## create a user-based CF recommender using training data
#' r <- Recommender(getData(e, "train"), "UBCF")
#'
#' ## create predictions for the test data using known ratings (see given above)
#' p <- predict(r, getData(e, "known"), type = "ratings")
#' p
#'
#' ## compute error metrics averaged per user and then averaged over all
#' ## recommendations
#' calcPredictionAccuracy(p, getData(e, "unknown"))
#' head(calcPredictionAccuracy(p, getData(e, "unknown"), byUser = TRUE))
#'
#' ## evaluate topNLists instead (you need to specify given and goodRating!)
#' p <- predict(r, getData(e, "known"), type = "topNList")
#' p
#' calcPredictionAccuracy(p, getData(e, "unknown"), given = 15, goodRating = 5)
#'
#' ## evaluate a binary recommender
#' data(MSWeb)
#' MSWeb10 <- sample(MSWeb[rowCounts(MSWeb) >10,], 50)
#'
#' e <- evaluationScheme(MSWeb10, method="split", train = 0.9,
#'     k = 1, given = 3)
#' e
#'
#' ## create a user-based CF recommender using training data
#' r <- Recommender(getData(e, "train"), "UBCF")
#'
#' ## create predictions for the test data using known ratings (see given above)
#' p <- predict(r, getData(e, "known"), type="topNList", n = 10)
#' p
#'
#' calcPredictionAccuracy(p, getData(e, "unknown"), given = 3)
#' calcPredictionAccuracy(p, getData(e, "unknown"), given = 3, byUser = TRUE)
#' @family evaluation
#' @name calcPredictionAccuracy
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
    stop("For all-but-x schemes, supply a vector of positive given values. This vector can be obtained from an evaluation scheme es by calling getData(es, 'given').")

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
