### Sparse matrix that drops NAs

# **Important:** see ? dropNA for details about how sparse data is stored

## FIXME: we cannot do this because Matrix does not export xMatrix!
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
## we add .Machine$double.xmin to real zeros to keep them.

## replace small values with 0 again
zapzero <- function(x, digits = 100) {
  zapsmall(x, digits = digits)
}

## sparse -> matrix
dropNA2matrix <- function(x) {
  if(!is(x, "dgCMatrix")) stop("x needs to be a dgCMatrix!")

  x <- as(x, "matrix")
  x[x == 0] <- NA
  # remove the small values representing real 0s
  zapzero(x)
}

## matrix -> sparse
dropNA <- function(x) {
    if(!is(x, "matrix")) stop("x needs to be a matrix!")

    # we preserve real zeros using a very small number
    x[x == 0] <- .Machine$double.xmin
    x[is.na(x)] <- 0
    # drop0 sometimes results in a "dsCMatrix"
    as(drop0(x), "generalMatrix")
}

dropNAis.na <- function(x) {
  if(!is(x, "dgCMatrix")) stop("x needs to be a dgCMatrix!")
  x == 0
}
