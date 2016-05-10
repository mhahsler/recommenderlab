

setAs("matrix", "sparseNAMatrix", function(from) dropNA(from))
setAs("sparseNAMatrix", "matrix", function(from) dropNA2matrix(from))

.sub <- function(x, i, j, ..., drop) {

  if(!missing(drop) && drop) warning("drop not available for sparseNAMatrix!")
  if(missing(i)) i <- 1:nrow(x)
  if(missing(j)) j <- 1:ncol(x)

  as(as(x, "dgCMatrix")[i,j, ..., drop=FALSE], "sparseNAMatrix")
}


setMethod("[", signature(x = "sparseNAMatrix", i = "index", j="index",
  drop="logical"), .sub)
setMethod("[", signature(x = "sparseNAMatrix", i = "missing", j="index",
  drop="logical"), .sub)
setMethod("[", signature(x = "sparseNAMatrix", i = "index", j="missing",
  drop="logical"), .sub)
setMethod("[", signature(x = "sparseNAMatrix", i = "index", j="index",
  drop="missing"), .sub)
setMethod("[", signature(x = "sparseNAMatrix", i = "missing", j="index",
  drop="missing"), .sub)
setMethod("[", signature(x = "sparseNAMatrix", i = "index", j="missing",
  drop="missing"), .sub)

## convert to and from dgCMatrix to preserve 0s and do not store NAs
dropNA2matrix <- function(x) {
  if(!is(x, "dsparseMatrix")) stop("x needs to be a subclass of dsparseMatrix!")

  #if(!is(x, "dgCMatrix"))
  x <- as(x, "dgCMatrix")

  x@x[x@x==0] <- NA
  zeros <- Matrix::which(is.na(x), arr.ind=TRUE)
  x <- as(x, "matrix")
  x[x==0] <- NA
  x[zeros] <- 0
  x
}

dropNA <- function(x) {
  if(!is(x, "dsparseMatrix")
    && !is(x, "matrix"))
    stop("x needs to be a subclass of dsparseMatrix or matrix!")

  if(is(x, "matrix")) zeros <- which(x==0, arr.ind=TRUE)
  else {
    if(!is(x, "dgCMatrix")) x <- as(x, "dgCMatrix")

    nas <- which(is.na(x@x))
    x@x[x@x==0] <- NA
    x@x[nas] <- 0
    zeros <- Matrix::which(is.na(x), arr.ind=TRUE)
  }
  x[is.na(x)] <- 0
  x[zeros] <- NA
  x <- drop0(x)
  x[zeros] <- 0

  new("sparseNAMatrix", x)
}

