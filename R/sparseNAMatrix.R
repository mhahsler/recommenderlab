### Sparse matrix that drops NAs

# coercion from and to dgCMatrix is implicit
# setAs("matrix", "sparseNAMatrix", function(from) dropNA(from))
# setAs("sparseNAMatrix", "matrix", function(from) dropNA2matrix(from))
#
# .sub <- function(x, i, j, ..., drop) {
#   if(!missing(drop) && drop) warning("drop not available for sparseNAMatrix!")
#   if(missing(i)) i <- 1:nrow(x)
#   if(missing(j)) j <- 1:ncol(x)
#   as(as(x, "dgCMatrix")[i,j, ..., drop=FALSE], "sparseNAMatrix")
# }
#
# setMethod("[", signature(x = "sparseNAMatrix", i = "index", j="index",
#   drop="logical"), .sub)
# setMethod("[", signature(x = "sparseNAMatrix", i = "missing", j="index",
#   drop="logical"), .sub)
# setMethod("[", signature(x = "sparseNAMatrix", i = "index", j="missing",
#   drop="logical"), .sub)
# setMethod("[", signature(x = "sparseNAMatrix", i = "index", j="index",
#   drop="missing"), .sub)
# setMethod("[", signature(x = "sparseNAMatrix", i = "missing", j="index",
#   drop="missing"), .sub)
# setMethod("[", signature(x = "sparseNAMatrix", i = "index", j="missing",
#   drop="missing"), .sub)
#
# .repl <- function(x, i, j, ..., value) {
#   if(missing(i)) i <- 1:nrow(x)
#   if(missing(j)) j <- 1:ncol(x)
#
#   ### preserve zeros using NAs!
#   zeros <- value == 0
#   value[zeros] <- NA
#
#   x <- as(x, "dgCMatrix")
#   x@x[x@x == 0] <- NA
#   x[i,j, ...] <- value
#
#   x@x[is.na(x@x)] <- 0
#
#   as(x, "sparseNAMatrix")
# }
#
# setReplaceMethod("[", signature(x = "sparseNAMatrix",
#   i = "missing", j = "missing", value = "numeric"),
#   function (x, i,j,..., value) .repl(x, i, j, ..., value=value))
#
# setReplaceMethod("[", signature(x = "sparseNAMatrix",
#   i = "index", j = "missing", value = "numeric"),
#   function (x, i,j,..., value) .repl(x, i, j, ..., value = value))
#
# setReplaceMethod("[", signature(x = "sparseNAMatrix",
#   i = "missing", j = "index", value = "numeric"),
#   function (x, i,j,..., value) .repl(x, i, j, ..., value = value))
#
# setReplaceMethod("[", signature(x = "sparseNAMatrix",
#   i = "index", j = "index", value = "numeric"),
#   function (x, i,j,..., value) .repl(x, i, j, ..., value = value))


## convert to and from dgCMatrix to preserve 0s and do not store NAs

## sparse -> matrix
dropNA2matrix <- function(x) {
  if(!is(x, "dsparseMatrix")) stop("x needs to be a subclass of dsparseMatrix!")

  x <- as(x, "dgCMatrix")

  ## remember true NAs
  nas <- Matrix::which(is.na(x), arr.ind=TRUE)

  x@x[x@x==0] <- NA
  zeros <- Matrix::which(is.na(x), arr.ind=TRUE)
  x <- as(x, "matrix")
  x[x==0] <- NA
  x[zeros] <- 0
  x[nas] <- NA
  x
}

## matrix -> sparse
dropNA <- function(x) {
    if(!is(x, "matrix")) stop("x needs to be a matrix!")

    zeros <- which(x==0, arr.ind=TRUE)
    ## keep zeros
    x[is.na(x)] <- 0
    x[zeros] <- NA
    x <- drop0(x)
    x[zeros] <- 0
    x
}

dropNAis.na <- function(x) {
  if(!is(x, "dsparseMatrix")) stop("x needs to be a subclass of dsparseMatrix!")
  x <- as(x, "dgCMatrix")

  ### not represented means NA and 0 means 0
  ### this coercion keeps 0
  !as(x, "ngCMatrix")
}
