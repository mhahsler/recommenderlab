## top-N lists

## coercion
setAs("topNList", "dgTMatrix",
  function(from) {
    i <- rep(1:length(from@items), lapply(from@items, length))
    j <- unlist(from@items)
    if(!is.null(from@ratings)) x <- unlist(from@ratings)
    else x <- rep(1, length(j))

    new("dgTMatrix", i = i-1L, j = j-1L,
      x = x,
      Dim = c(length(from@items), length(from@itemLabels)),
      Dimnames = list(names(from@items), from@itemLabels))
  })

setAs("topNList", "dgCMatrix",
  function(from) as(as(from, "dgTMatrix"), "dgCMatrix"))

setAs("topNList", "ngCMatrix",
  function(from) as(as(from, "dgCMatrix"), "ngCMatrix"))

setAs("topNList", "realRatingMatrix",
  function(from) {
    new("realRatingMatrix", data = as(from, "dgCMatrix"))
  }
)

setAs("topNList", "matrix",
  function(from) dropNA2matrix(as(from, "dgCMatrix")))

setMethod("getList", signature(from = "topNList"),
  function(from, decode = TRUE, ...)
    if(decode) lapply(from@items, function(y) from@itemLabels[y])
  else from@items)

setAs("topNList", "list", function(from) getList(from, decode = TRUE))

setMethod("c", signature(x = "topNList"),
  function(x, ..., recursive = FALSE){
    args <- list(...)
    if (recursive)
      args <- unlist(args)
    for (y in args) {
      if (!is(y, "topNList"))
        stop("can only combine objects of class topNList")
      if (!identical(x@itemLabels, y@itemLabels))
        stop("topNlists are not compatible (item labels do not match).")
      x@items <- c(x@items, y@items)
      x@ratings <- c(x@ratings, y@ratings)
    }
    x
  }
)


## creation from realRatingMatrix
setMethod("getTopNLists", signature(x = "realRatingMatrix"),
  function(x, n=10, randomize=NULL, minRating=NA){
    n <- as.integer(n)

    # just in case
    x <- denormalize(x)

    ### FIXME: this takes a lot of memory for a dense rating matrix
    reclist <- getList(x, decode = FALSE)

    if(is.null(randomize) || is.na(randomize)) {
      reclist <- lapply(reclist, FUN = function(l)
        head(sort(l, decreasing = TRUE), n=n))

      reclist <- new("topNList",
        items = lapply(reclist, FUN = function(l) as.integer(names(l))),
        ratings = lapply(reclist, as.vector),
        itemLabels = colnames(x), n = n)

      if(!is.null(minRating) && !is.na(minRating))
        reclist <- bestN(reclist, n = n, minRating = minRating)

    }else{
    ## randomize recommendations
      reclist <- lapply(reclist, FUN = function(l) {
        if(!is.null(minRating) && !is.na(minRating)) l <- l[l>=minRating]
        if(length(l)>0) sample(l, size = min(n, length(l)),
          prob = (l-min(l)+1)^randomize)
        else integer(0)
      })

      reclist <- new("topNList",
        items = lapply(reclist, FUN = function(l) as.integer(names(l))),
        ratings = lapply(reclist, as.vector),
        itemLabels = colnames(x), n = n)
    }
  reclist
  })

## only keep best n items.
setMethod("bestN", signature(x = "topNList"),
  function(x, n = 10, minRating = NA) {

    if(!is.null(minRating) && !is.na(minRating)) {
      if(is.null(x@ratings)) stop("topNList does not contain ratings, setting minRatings not possible!")

      take <- lapply(x@ratings, ">=", minRating)
      x@items <- lapply(1:length(take), FUN=function(l) x@items[[l]][take[[l]]])
      x@ratings <- lapply(1:length(take), FUN=function(l) x@ratings[[l]][take[[l]]])
    }

    new("topNList", items = lapply(x@items, head, n),
      ratings = if(!is.null(x@ratings)) lapply(x@ratings, head, n) else NULL,
      itemLabels = x@itemLabels, n = as.integer(n))
    })


setMethod("removeKnownItems", signature(x = "topNList"),
  function(x, known) {
    if(!is(known, "ratingMatrix"))
      stop("known needs to be a ratingMatrix!")

    if(length(x) != nrow(known))
      stop("length of x and number of rows in known do not match!")

    ns <- names(x@items)

    rem <- lapply(1:length(x), FUN=function(i)
      which(x@items[[i]] %in% unlist(getList(known[i,],
        decode=FALSE, ratings=FALSE))))

    #x@items <- lapply(1:length(x), FUN=function(i) {
    #  setdiff(x@items[[i]],
    #    getList(known[i],
    #      decode=FALSE, ratings=FALSE)[[1]])
    #})

    x@items <- lapply(1:length(x), FUN=function(i) x@items[[i]][-rem[[i]]])
    names(x@items) <- ns

    if(!is.null(x@ratings))
      x@ratings <- structure(lapply(1:length(x), FUN=function(i)
        x@ratings[[i]][-rem[[i]]]), names = ns)

    x
  })

setMethod("length", signature(x = "topNList"),
  function(x) length(x@items))

setMethod("show", signature(object = "topNList"),
  function(object) {
    cat("Recommendations as", sQuote(class(object)),
      "with n =", object@n, "for",
      length(object@items),"users.","\n")
    invisible(NULL)
  })


setMethod("colCounts", signature(x = "topNList"),
  function(x, ...) {
    s <- colSums(as(x, "ngCMatrix"))
    names(s) <- x@itemLabels
    s
  })

setMethod("rowCounts", signature(x = "topNList"),
  function(x, ...) sapply(x@items, length))


