## negative given implement All-but-given

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

      times <- as.integer(n / k)
      fold_ids <- sample(rep(1:k, times), times * k)
      runsTrain <- lapply(
        1:k,
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
