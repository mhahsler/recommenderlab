

#' @title
#' Evaluate a Recommender Models
#'
#' @description Evaluates one or more recommender models using an evaluation scheme and returns evaluation metrics.
#' @aliases evaluate,evaluationScheme,character-method
#' @aliases evaluate,evaluationScheme,list-method
#'
#' @usage evaluate(x, method, ...)
#'
#' \S4method{evaluate}{evaluationScheme,character}(x, method, type="topNList",
#'   n=1:10, parameter=NULL, progress = TRUE, keepModel=FALSE)
#' \S4method{evaluate}{evaluationScheme,list}(x, method, type="topNList",
#'   n=1:10, parameter=NULL, progress = TRUE, keepModel=FALSE)
#'
#' @param x an evaluation scheme (class \code{"evaluationScheme"}).
#'
#' @param method a character string or a list. If
#'   a single character string is given it defines the recommender method
#'   used for evaluation. If several recommender methods need to be compared,
#'   \code{method} contains a nested list. Each element describes a recommender
#'   method and consists of a list with two elements: a character string
#'   named \code{"name"} containing the method and a list named
#'   \code{"parameters"} containing the parameters used for this recommender method.
#'   See \code{Recommender} for available methods.
#'
#' @param type evaluate "topNList" or "ratings"?
#'
#' @param n a vector of the different values for N used to generate top-N lists (only if type="topNList").
#'
#' @param parameter a list with parameters for the recommender algorithm (only
#'     used when \code{method} is a single method).
#'
#' @param progress logical; report progress?
#'
#' @param keepModel logical; store used recommender models?
#'
#' @param \dots further arguments.
#'
#' @details The evaluation uses the specification in the evaluation scheme to train a recommender models on training data and then evaluates the models on test data.
#' The result is a set of accuracy measures averaged over the test users.
#' See \code{\link{calcPredictionAccuracy}} for details on the accuracy measures and the averaging.
#' Note: Also the confusion matrix counts are averaged over users and therefore not whole numbers.
#'
#' See \code{vignette("recommenderlab")} for more details on the evaluation process and the metrics used.
#'
#' @return If a single recommender method is specified in  \code{method}, then an
#' object of class \code{"evaluationResults"} is returned.
#' If \code{method} is a list of recommendation models, then an object of class \code{"evaluationResultList"} is returned.
#'
#' @seealso \code{\link{calcPredictionAccuracy}},
#' \code{\linkS4class{evaluationScheme}},
#' \code{\linkS4class{evaluationResults}}.
#' \code{\linkS4class{evaluationResultList}}.
#'
#' @examples ### evaluate top-N list recommendations on a 0-1 data set
#' ## Note: we sample only 100 users to make the example run faster
#' data("MSWeb")
#' MSWeb10 <- sample(MSWeb[rowCounts(MSWeb) >10,], 100)
#'
#' ## create an evaluation scheme (10-fold cross validation, given-3 scheme)
#' es <- evaluationScheme(MSWeb10, method="cross-validation",
#'         k=10, given=3)
#'
#' ## run evaluation
#' ev <- evaluate(es, "POPULAR", n=c(1,3,5,10))
#' ev
#'
#' ## look at the results (the length of the topNList is shown as column n)
#' getResults(ev)
#'
#' ## get a confusion matrices averaged over the 10 folds
#' avg(ev)
#' plot(ev, annotate = TRUE)
#'
#' ## evaluate several algorithms (including a hybrid recommender) with a list
#' algorithms <- list(
#'   RANDOM = list(name = "RANDOM", param = NULL),
#'   POPULAR = list(name = "POPULAR", param = NULL),
#'   HYBRID = list(name = "HYBRID", param =
#'       list(recommenders = list(
#'           RANDOM = list(name = "RANDOM", param = NULL),
#'           POPULAR = list(name = "POPULAR", param = NULL)
#'         )
#'       )
#'   )
#' )
#'
#' evlist <- evaluate(es, algorithms, n=c(1,3,5,10))
#' evlist
#' names(evlist)
#'
#' ## select the first results by index
#' evlist[[1]]
#' avg(evlist[[1]])
#'
#' plot(evlist, legend="topright")
#'
#' ### Evaluate using a data set with real-valued ratings
#' ## Note: we sample only 100 users to make the example run faster
#' data("Jester5k")
#' es <- evaluationScheme(Jester5k[1:100], method="split",
#'   train=.9, given=10, goodRating=5)
#' ## Note: goodRating is used to determine positive ratings
#'
#' ## predict top-N recommendation lists
#' ## (results in TPR/FPR and precision/recall)
#' ev <- evaluate(es, "RANDOM", type="topNList", n=10)
#' getResults(ev)
#'
#' ## predict missing ratings
#' ## (results in RMSE, MSE and MAE)
#' ev <- evaluate(es, "RANDOM", type="ratings")
#' getResults(ev)
#' @family evaluation
#' @name evaluate
setMethod("evaluate", signature(x = "evaluationScheme", method = "character"),
  function(x,
    method,
    type = "topNList",
    n = 1:10,
    parameter = NULL,
    progress = TRUE,
    keepModel = FALSE) {
    scheme <- x
    runs <- 1:scheme@k

    if (progress)
      cat(method, "run fold/sample [model time/prediction time]")

    cm <- list()
    for (r in runs) {
      if (progress)
        cat("\n\t", r, " ")

      cm[[r]] <- .do_run_by_n(
        scheme,
        method,
        run = r,
        type = type,
        n = n,
        parameter = parameter,
        progress = progress,
        keepModel = keepModel
      )
    }

    if (progress)
      cat("\n")

    new(
      "evaluationResults",
      results = cm,
      method = recommenderRegistry$get_entry(method)$method
    )
  })

setMethod("evaluate", signature(x = "evaluationScheme", method = "list"),
  function(x,
    method,
    type = "topNList",
    n = 1:10,
    parameter = NULL,
    progress = TRUE,
    keepModel = FALSE) {
    ## method is a list of lists
    #list(RANDOM = list(name = "RANDOM", parameter = NULL),
    #	POPULAR = list(...

    results <- lapply(
      method,
      FUN = function(a)
        try(evaluate(
          x,
          a$name,
          n = n ,
          type = type,
          parameter = a$param,
          progress = progress,
          keepModel = keepModel
        ))
    )

    ## handle recommenders that have failed
    errs <- sapply(results, is, "try-error")
    if (any(errs))
    {
      warning(
        paste(
          "\n  Recommender '",
          names(results)[errs],
          "' has failed and has been removed from the results!",
          sep  =  ''
        )
      )
      results[errs] <- NULL
    }

    as(results, "evaluationResultList")
  })


## evaluation work horse
.do_run_by_n <-
  function(scheme,
    method,
    run,
    type,
    n,
    parameter = NULL,
    progress = FALSE,
    keepModel = TRUE) {
    ## prepare data
    train <- getData(scheme, type = "train", run = run)
    test_known <- getData(scheme, type = "known", run = run)
    test_unknown <- getData(scheme, type = "unknown", run = run)
    given <- getData(scheme, type = "given", run = run)

    ## train recommender
    time_model <- system.time(rec <-
        Recommender(train, method, parameter = parameter),
      gcFirst = FALSE)


    time_predict <- system.time(pre <-
        predict(rec, test_known, n = max(n), type = type),
      gcFirst = FALSE)

    if (is(pre, "topNList")) {
      res <- NULL
      for (i in 1:length(n)) {
        NN <- n[i]

        ## get best N
        topN <- bestN(pre, NN)

        r <-  calcPredictionAccuracy(
          topN,
          test_unknown,
          byUser = FALSE,
          given = given,
          goodRating = scheme@goodRating
        )
        res <- rbind(res, r)
      }
      res <- cbind(res, n)

    } else{
      res <- calcPredictionAccuracy(
        pre,
        test_unknown,
        byUser = FALSE,
        given = given,
        goodRating = scheme@goodRating
      )

      res <- rbind(res)
    }

    rownames(res) <- NULL

    time_usage <- function(x)
      x[1] + x[2]

    if (progress)
      cat("[",
        time_usage(time_model),
        "sec/",
        time_usage(time_predict),
        "sec] ",
        sep = "")

    new("confusionMatrix",
      cm = res,
      model =
        if (keepModel)
          rec
      else
        NULL)
  }
