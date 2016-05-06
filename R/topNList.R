## top-N lists

## coercion
setAs("topNList", "dgTMatrix",
  function(from) {
    i <- rep(1:length(from@items), lapply(from@items, length))
    j <- unlist(from@items)
    x <- rep(1, length(j))
    
    new("dgTMatrix", i = i-1L, j = as.integer(j)-1L,
      x = x,
      Dim = c(length(from@items), length(from@itemLabels)),
      Dimnames = list(names(from@items), from@itemLabels))
  })

setAs("topNList", "dgCMatrix",
  function(from) as(as(from, "dgTMatrix"),"dgCMatrix"))

setAs("topNList", "ngCMatrix",
  function(from) as(as(from, "dgCMatrix"), "ngCMatrix"))

setAs("topNList", "matrix",
  function(from) as(as(from, "dgTMatrix"),"matrix"))

setMethod("getList", signature(from = "topNList"),
  function(from, decode = TRUE, ...)
    if(decode) lapply(from@items, function(y) from@itemLabels[y])
  else from@items)

setAs("topNList", "list", function(from) getList(from, decode = TRUE))

## creation from realRatingMatrix
setMethod("getTopNLists", signature(x = "realRatingMatrix"),
  function(x, n= 10, minRating = NA){
    n <- as.integer(n)
    x.m <- as(x, "matrix")
    
    if(!is.na(minRating)) x.m[x.m<minRating] <- NA
    
    reclist <- lapply(1:nrow(x), FUN=function(i) {
      head(order(as(x.m[i,],"matrix"),
        decreasing=TRUE, na.last=NA), n)
    })
    
    names(reclist) <- rownames(x)
    
    new("topNList", items = reclist, itemLabels = colnames(x), n = n)
  })

## only keep best n items.
setMethod("bestN", signature(x = "topNList"),
  function(x, n = 10) new("topNList", items = lapply(x@items, head, n), 
    itemLabels = x@itemLabels, n = as.integer(n)))


setMethod("removeKnownItems", signature(x = "topNList"),
  function(x, known) {
    if(!is(known, "ratingMatrix")) 
      stop("known needs to be a ratingMatrix!")
    
    if(length(x) != nrow(known)) 
      stop("length of x and number of rows in known do not match!")
    
    ns <- names(x@items)
    
    x@items <- lapply(1:length(x), FUN=function(i) {
      setdiff(x@items[[i]], 
        getList(known[i], 
          decode=FALSE, ratings=FALSE)[[1]])
    })
    
    
    names(x@items) <- ns
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


