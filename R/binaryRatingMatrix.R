## binaryRatingMatrix based on itemMatrix (arules)

## Coercions
setAs("matrix", "binaryRatingMatrix",
  function(from)
    new("binaryRatingMatrix",
      data = as(from, "itemMatrix")))

setAs("binaryRatingMatrix", "matrix",
  function(from)
    as(from@data, "matrix"))

setAs("itemMatrix", "binaryRatingMatrix",
  function(from)
    new("binaryRatingMatrix",
      data = from))

setAs("binaryRatingMatrix", "itemMatrix",
  function(from)
    from@data)

## itemMatrix stores data transposed!
setAs("binaryRatingMatrix", "ngCMatrix",
  function(from)
    t(as(from@data, "ngCMatrix")))

setAs("binaryRatingMatrix", "dgCMatrix",
  function(from)
    as(as(from, "ngCMatrix"), "dsparseMatrix"))

setAs("binaryRatingMatrix", "dgTMatrix",
  function(from)
    as(as(from, "dgCMatrix"), "TsparseMatrix"))

## list
setMethod("getList", signature(from = "binaryRatingMatrix"),
  function(from, decode = TRUE, ...) {
    LIST(from@data, decode = decode)
  })

## FIXME: we could do this cheaper
setAs("data.frame", "binaryRatingMatrix",
  function(from) {
    rr <- as(from, "realRatingMatrix")
    binarize(rr, minRating = -Inf)
  })


## FIXME: removeKnownRatings should be implemented here!
#setMethod("removeKnownRatings", signature(x = "binaryRatingMatrix"),
#	function(x, known) {
#	    if(!is(known, "ratingMatrix")) stop("known needs to be a ratingMatrix!")
#
#	    stop("Currently not implemented!")
#	})

## split test data
setMethod(".splitKnownUnknown", signature(data = "binaryRatingMatrix"),
  function(data, given) {
    ## given might of length one or length(data)
    if (length(given) == 1)
      given <- rep(given, nrow(data))
    nitems <- rowCounts(data)

    ## calculate given for all-but-x case
    allBut <- given < 0
    if (any(allBut)) {
      given[allBut] <- nitems[allBut] + given[allBut]
    }

    ## check that we have enough ratings. This can happen for all-but-x
    if (any(given < 1))
      warning(
        "The following users do not have enough items leaving no given items: ",
        paste(which(given < 1), collapse = ", ")
      )

    if (any(given > nitems))
      stop("Not enough ratings for user" ,
        paste(which(given > nitems), collapse = ", "))

    ## split
    l <- getList(data, decode = FALSE)
    known_index <- lapply(
      seq_along(l),
      FUN = function(i)
        sample(1:length(l[[i]]), given[i])
    )

    known <- encode(lapply(
      seq_along(l),
      FUN = function(x)
        l[[x]][known_index[[x]]]
    ),
      itemLabels = itemLabels(data@data))
    rownames(known) <- rownames(data)

    unknown <- encode(lapply(
      seq_along(l),
      FUN = function(x)
        ### deal with integer(0) if there are not enough known items
        if (length(known_index[[x]]) == 0)
          l[[x]]
      else
        l[[x]][-known_index[[x]]]
    ),
      itemLabels = itemLabels(data@data))
    rownames(unknown) <- rownames(data)

    known <- new("binaryRatingMatrix", data = known)
    unknown <- new("binaryRatingMatrix", data = unknown)

    list(known = known,
      unknown = unknown)
  })
