## negative given implement All-but-given

#' @title
#' Creator Function for evaluationScheme
#'
#' @description Creates an evaluationScheme object from a data set. The scheme can be a
#' simple split into training and test data, k-fold cross-evaluation or using k
#' independent bootstrap samples.
#' @aliases evaluationScheme,ratingMatrix-method
#'
#' @usage evaluationScheme(data, ...)
#'
#' \S4method{evaluationScheme}{ratingMatrix}(data, method="split",
#'     train=0.9, k=NULL, given, goodRating = NA)
#'
#' @param data data set as a ratingMatrix.
#'
#' @param method a character string defining the evaluation
#' 	method to use (see details).
#'
#' @param train fraction of the data set used for training.
#'
#' @param k number of folds/times to run the evaluation (defaults to 10
#' 	    for cross-validation and bootstrap and 1 for split).
#'
#' @param given single number of items given for evaluation or
#' 	  a vector of length of data giving the number of items given for each
#' 	    observation. Negative values implement all-but schemes. For example,
#' 	    \code{given = -1} means all-but-1 evaluation.
#'
#' @param goodRating numeric; threshold at which ratings are considered
#' 	good for evaluation. E.g., with \code{goodRating=3} all items
#' 	with actual user rating of greater or equal 3 are
#' 	considered positives in the evaluation process.
#' 	Note that this argument is only used when the rating matrix is a
#' 	subclass of realRatingMatrix.
#'
#' @param \dots further arguments.
#'
#' @details \code{evaluationScheme} creates an evaluation scheme (training and test data)
#' with \code{k} runs and one of the following methods:
#'
#' \code{"split"} randomly assigns
#' the proportion of objects specified by \code{train} to the training set and
#' the rest is used for the test set.
#'
#' \code{"cross-validation"} creates a k-fold cross-validation scheme. The data
#' is randomly split into k parts and in each run k-1 parts are used for
#' training and the remaining part is used for testing. After all k runs each
#' part was used as the test set exactly once.
#'
#' \code{"bootstrap"} creates the training set by taking a bootstrap sample
#' (sampling with replacement) of size \code{train} times number of users in
#' the data set.
#' All objects not in the training set are used for testing.
#'
#' For evaluation, Breese et al. (1998) introduced the
#' four experimental protocols called Given 2, Given 5, Given 10 and All-but-1.
#' During testing, the Given x protocol presents the algorithm with
#' only x randomly chosen items for the test user, and the algorithm
#' is evaluated by how well it is able to predict the withheld items.
#' For All-but-x,
#' the algorithm sees all but
#' x withheld ratings for the test user.
#' \code{given} controls x in the evaluations scheme.
#' Positive integers result in a Given x protocol, while negative values
#' produce a All-but-x protocol.
#'
#' If a user does not have enough ratings to satisfy \code{given}, then the user is dropped from the
#' evaluation with a warning.
#'
#' @return Returns an object of class \code{"evaluationScheme"}.
#'
#' @references Kohavi, Ron (1995). "A study of cross-validation and bootstrap for accuracy
#' estimation and model selection". Proceedings of  the Fourteenth International
#' Joint Conference on Artificial Intelligence, pp. 1137-1143.
#'
#' Breese JS, Heckerman D, Kadie C (1998). "Empirical Analysis of Predictive
#' Algorithms for Collaborative Filtering." In Uncertainty in Artificial
#' Intelligence. Proceedings of the Fourteenth Conference, pp. 43-52.
#'
#' @seealso \code{\link{getData}},
#' \code{\linkS4class{evaluationScheme}},
#' \code{\linkS4class{ratingMatrix}}.
#'
#' @examples data("MSWeb")
#'
#' MSWeb10 <- sample(MSWeb[rowCounts(MSWeb) >10,], 50)
#' MSWeb10
#'
#' ## simple split with 3 items given
#' esSplit <- evaluationScheme(MSWeb10, method="split",
#'         train = 0.9, k=1, given=3)
#' esSplit
#'
#' ## 4-fold cross-validation with all-but-1 items for learning.
#' esCross <- evaluationScheme(MSWeb10, method="cross-validation",
#'         k=4, given=-1)
#' esCross
#' @family evaluation
#' @name evaluationScheme
setMethod("evaluationScheme", signature(data = "ratingMatrix"),
  function(data,
    method = "split",
    train = 0.9,
    k = NULL,
    given,
    goodRating = NA) {
    goodRating <- as.numeric(goodRating)

    #if(given<1) stop("given needs to be >0!")

    #   if(is(data, "realRatingMatrix") && is.na(goodRating))
    #	stop("You need to set goodRating in the evaluationScheme for a realRatingMatrix!")

    n <- nrow(data)

    ## given can be one integer or a vector of length data
    given <- as.integer(given)
    if (length(given) != 1 && length(given) != n)
      stop("Length of given has to be one or length of data!")

    ## check size
    if (given > 0)
      not_enough_ratings <- which(rowCounts(data) < given)
    else ### all-but-x
      not_enough_ratings <- which(rowCounts(data) < (-given + 1))

    if (length(not_enough_ratings) > 1) {
      warning(
        "Dropping these users from the evaluation since they have fewer rating than specified in given!\n",
        "These users are ",
        paste(not_enough_ratings, collapse = ", ")
      )
      data <- data[-not_enough_ratings]
      n <- nrow(data)

      if (length(given) != 1)
        given <- given[-not_enough_ratings]
    }

    ## methods
    methods <- c("split", "cross-validation", "bootstrap")
    method_ind <- pmatch(method, methods)
    if (is.na(method_ind))
      stop("Unknown method!")
    method <- methods[method_ind]

    ## set default value for k
    if (is.null(k)) {
      if (method_ind == 1)
        k <- 1L
      else
        k <- 10L
    } else
      k <- as.integer(k)

    ## split
    if (method_ind == 1)
      runsTrain <- replicate(k,
        sample(1:n, n * train), simplify = FALSE)

    ## cross-validation
    else if (method_ind == 2) {
      train <- NA_real_

      fold_ids <- sample(rep(seq_len(k), length.out = n))
      runsTrain <- lapply(
        seq_len(k),
        FUN = function(i)
          which(fold_ids != i)
      )
    }

    ## bootstrap
    else if (method_ind == 3)
      runsTrain <- replicate(k, sample(1:n,
        n * train, replace = TRUE), simplify = FALSE)

    testData <- .splitKnownUnknown(data, given)

    new(
      "evaluationScheme",
      method	= method,
      given	= given,
      k		= k,
      train	= train,
      runsTrain	= runsTrain,
      data	= data,
      knownData	= testData$known,
      unknownData	= testData$unknown,
      goodRating = goodRating
    )
  })

## .splitKnownUnknown is implemented in realRatingMatrix and binaryRatingMatrix

setMethod("getData", signature(x = "evaluationScheme"),
  function(x,
    type = c("train", "known", "unknown", "given"),
    run = 1) {
    if (run > x@k)
      stop("Scheme does not contain that many runs!")

    type <- match.arg(type)
    switch(
      type,
      train = x@data[x@runsTrain[[run]]],
      known = x@knownData[-x@runsTrain[[run]]],
      unknown = x@unknownData[-x@runsTrain[[run]]],
      given =  rowCounts(x@knownData[-x@runsTrain[[run]]])
    )
  })


setMethod("show", signature(object = "evaluationScheme"),
  function(object) {
    if (length(object@given) == 1) {
      if (object@given >= 0)
        writeLines(sprintf("Evaluation scheme with %d items given",
          object@given))
      else
        writeLines(sprintf(
          "Evaluation scheme using all-but-%d items",
          abs(object@given)
        ))
    } else{
      writeLines(c("Evaluation scheme with multiple items given",
        "Summary:"))
      print(summary(object@given))
    }

    writeLines(sprintf("Method: %s with %d run(s).",
      sQuote(object@method), object@k))

    if (!is.na(object@train)) {
      writeLines(sprintf("Training set proportion: %1.3f",
        object@train))
    }

    if (!is.na(object@goodRating))
      writeLines(sprintf("Good ratings: >=%f", object@goodRating))
    else
      writeLines(sprintf("Good ratings: NA"))

    writeLines("Data set: ", sep = '')
    show(object@data)
    invisible(NULL)
  })
