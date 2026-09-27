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
#' @title
#' %  Class ``sparseNAMatrix'' --- Sparse Matrix Representation With NAs Not Explicitly Stored
#' Sparse Matrix Representation With NAs Not Explicitly Stored
#'
#' @description %Class to represent matrices with dropped NAs.
#' Coerce from and to a
#' sparse matrix representation where \code{NA}s are not explicitly stored.
#' @aliases sparseNAMatrix-class
#' @aliases dropNA
#' @aliases dropNA2matrix
#' @aliases dropNAis.na
#'
#' @usage dropNA(x)
#' dropNA2matrix(x)
#' dropNAis.na(x)
#'
#' @param x  a matrix for \code{dropNA()}, or a sparse matrix with dropped NA values
#'     for \code{dropNA2matrix()} or \code{dropNAis.na()}.
#'
#' @details The representation is based on
#' the sparse \code{dgCMatrix} in \pkg{Matrix} but instead of zeros, \code{NA}s are dropped.
#' This is achieved by the following:
#'
#' \itemize{
#' \item Zeros are represented with a very small value (\code{.Machine$double.xmin})
#' so they do not get dropped in the sparse representation.
#' \item NAs are converted to 0 before coercion to \code{dgCMatrix} so they are not explicitly stored.
#' }
#'
#' \bold{Caution:} Be careful when working with the sparse matrix and sparse matrix operations
#' (multiplication, addition, etc.) directly.
#' \itemize{
#' \item Sparse matrix operations will see 0 where NAs should be.
#' \item Actual zero ratings have a small, but non-zero value (\code{.Machine$double.xmin}).
#' \item Sparse matrix operations that can result in a true 0
#'    need to be followed by replacing the 0 with \code{.Machine$double.xmin} or other operations
#'    (like subsetting) may drop the 0.
#' }
#'
#' \code{dropNAis.na()} correctly finds NA values in a sparse matrix with dropped NA values, while
#' \code{is.na()} does not work.
#'
#' \code{dropNA2matrix()} converts the sparse representation into a dense matrix. NAs represented by
#' dropped values are converted to true NAs. Zeros are recovered by using \code{zapsmall()} which replaces
#' small values by 0.
#'
#' @return %Returns a sparseNAMatrix (subclass of dgCMatrix) or a matrix.
#' Returns a dgCMatrix or a matrix, respectively.
#'
#' @seealso     \code{\link[Matrix:dgCMatrix-class]{dgCMatrix}} in \pkg{Matrix}.
#'
#' @examples m <- matrix(sample(c(NA,0:5),50, replace=TRUE, prob=c(.5,rep(.5/6,6))),
#'     nrow=5, ncol=10, dimnames = list(users=paste('u', 1:5, sep=''),
#'     items=paste('i', 1:10, sep='')))
#' m
#'
#' ## drop all NAs in the representation. Zeros are represented by very small values.
#' sparse <- dropNA(m)
#' sparse
#'
#' ## convert back to matrix
#' dropNA2matrix(sparse)
#'
#' ## Note: be careful with the sparse representation!
#' ## Do not use is.na, but use
#' dropNAis.na(sparse)
#' @family data preparation
#' @name sparseNAMatrix
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
