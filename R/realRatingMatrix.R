## realRatingMatrix stores a dgCMatrix where 0s are a very small number.

## coercions
setAs("matrix", "realRatingMatrix",
  function(from)
    new("realRatingMatrix",
      data = dropNA(from)))

setAs("realRatingMatrix", "matrix",
  function(from)
    dropNA2matrix(from@data))

setAs("realRatingMatrix", "dgCMatrix",
  function(from)
    from@data)

setAs("dgCMatrix", "realRatingMatrix",
  function(from)
    new("realRatingMatrix", data = from))

setAs("realRatingMatrix", "dgTMatrix",
  function(from)
    as(from@data, "TsparseMatrix"))

setAs("dgTMatrix", "realRatingMatrix",
  function(from)
    new("realRatingMatrix",
      data = as(from, "CsparseMatrix")))

setAs("realRatingMatrix", "ngCMatrix",
  function(from)
    as(from@data, "nsparseMatrix"))

## from a data.frame with columns user, item, rating
## this preserves 0s
setAs("data.frame", "realRatingMatrix", function(from) {
  user	<- from[, 1]
  item	<- from[, 2]
  if (ncol(from) >= 3)
    rating <- as.numeric(from[, 3])
  else
    rating <- rep(1, length(item))

  i <- factor(user)
  j <- factor(item)

  dgT <-
    new(
      "dgTMatrix",
      i = as.integer(i) - 1L,
      j = as.integer(j) - 1L,
      x = rating,
      Dim = c(length(levels(i)), length(levels(j))),
      Dimnames = list(levels(i), levels(j))
    )

  as(dgT, "realRatingMatrix")
})

setAs("realRatingMatrix", "data.frame", function(from) {
  trip <- as(from, "dgTMatrix")
  data.frame(user = rownames(from)[trip@i + 1L],
    item = colnames(from)[trip@j + 1L],
    rating = trip@x)[order(trip@i),]
})

setMethod("getList", signature(from = "realRatingMatrix"),
  function(from,
    decode = TRUE,
    ratings = TRUE,
    ...) {
    trip <- as(from, "dgTMatrix")

    lst <- split(trip@j + 1L, factor(trip@i,
      levels = 0:(nrow(trip) - 1L)), drop = FALSE)

    if (decode)
      lst <- lapply(lst, function(y)
        colnames(from)[y])
    else
      names(lst) <- NULL

    if (!ratings)
      return(lst)

    rts <- split(trip@x, factor(trip@i,
      levels = 0:(nrow(trip) - 1L)), drop = FALSE)
    for (i in 1:length(rts)) {
      names(rts[[i]]) <- lst[[i]]
    }

    rts
  })


## binarize
setMethod("binarize", signature(x = "realRatingMatrix"),
  function(x, minRating, ...) {
    x <- x@data
    x@x <- as.numeric(x@x >= minRating)
    x <- drop0(x)
    if (is.null(colnames(x)))
      colnames(x) <- 1:ncol(x)
    x <- as(t(x), "nsparseMatrix")

    new("binaryRatingMatrix", data = as(x, "itemMatrix"))
  })

setMethod("removeKnownRatings", signature(x = "realRatingMatrix"),
  function(x, known) {
    if (!is(known, "ratingMatrix"))
      stop("known needs to be a ratingMatrix!")

    if (nrow(x) != nrow(known))
      stop("removeKnownRatings: Number of rows in x and known do not match!")

    x@data[hasRating(known)] <- 0
    x
  })


## compute standard deviation
.dgC2list <- function(x, row = TRUE) {
  if (row)
    x <- t(x)
  lapply(
    2:length(x@p),
    FUN = function(i) {
      if (x@p[i - 1L] == x@p[i])
        numeric(0)
      else
        x@x[(x@p[i - 1L] + 1L):x@p[i]]
    }
  )
}

setMethod("rowSds", signature(x = "realRatingMatrix"),
  function(x, ...) {
    s <- sapply(.dgC2list(x@data, row = TRUE), sd)
    names(s) <- rownames(x)
    s
  })


setMethod("colSds", signature(x = "realRatingMatrix"),
  function(x, ...) {
    s <- sapply(.dgC2list(x@data, row = FALSE), sd)
    names(s) <- colnames(x)
    s
  })

## create test data
## Note: negative given means all-but-given
setMethod(".splitKnownUnknown", signature(data = "realRatingMatrix"),
  function(data, given) {
    ## given might of length one or length(data)
    if (length(given) == 1)
      given <- rep(given, nrow(data))
    nitems <- rowCounts(data)

    ## convert all-but-x to given
    allBut <- given < 0
    if (any(allBut))
      given[allBut] <- nitems[allBut] + given[allBut]

    ## check that we have enough ratings (this can happen for all-but-x)
    if (any(given < 1))
      warning(
        "The following users do not have enough ratings leaving no given items: ",
        paste(which(given < 1), collapse = ", ")
      )

    if (any(given > nitems))
      stop("Not enough ratings for user" ,
        paste(which(given > nitems), collapse = ", "))

    ## start the split
    trip <- t(as(data, "dgTMatrix"))
    data_list <- split(trip@i + 1L, trip@j)

    take <- unlist(lapply(seq_along(data_list), function(i) {
      tk <- rep(FALSE, times = length(data_list[[i]]))
      tk[sample(seq_along(tk), given[i])] <- TRUE
      tk
      }))

    tripKnown <- trip
    tripKnown@x <- tripKnown@x[take]
    tripKnown@i <- tripKnown@i[take]
    tripKnown@j <- tripKnown@j[take]

    ntake <- !take
    tripUnknown <- trip
    tripUnknown@x <- tripUnknown@x[ntake]
    tripUnknown@i <- tripUnknown@i[ntake]
    tripUnknown@j <- tripUnknown@j[ntake]

    known <- new("realRatingMatrix",
      data = as(t(tripKnown), "CsparseMatrix"))
    unknown <- new("realRatingMatrix",
      data = as(t(tripUnknown), "CsparseMatrix"))

    list(known = known,
      unknown = unknown)
  })

### subset is in ratingMatrix
setReplaceMethod("[", signature(x = "realRatingMatrix"),
  function(x, i, j, value) {
    ### protect zeros
    value[value == 0] <- .Machine$double.xmin

    if (missing(i))
      i <- 1:nrow(x)
    if (missing(j))
      j <- 1:ncol(x)

    x@data[i, j] <- value
    x
  })
