#' @title
#' Normalize the ratings
#'
#' @description Provides the generic for normalize/denormalize and
#' a method to normalize/denormalize the ratings in a realRatingMatrix.
#' @aliases denormalize
#' @aliases denormalize,realRatingMatrix-method
#' @aliases normalize,realRatingMatrix-method
#'
#' @usage normalize(x, ...)
#' \S4method{normalize}{realRatingMatrix}(x, method="center", row=TRUE)
#'
#' denormalize(x, ...)
#' \S4method{denormalize}{realRatingMatrix}(x, method=NULL, row=NULL,
#'     factors=NULL)
#'
#' @param x a realRatingMatrix.
#'
#' @param method normalization method. Currently "center" or "Z-score".
#'
#' @param row logical; normalize rows (or the columns)?
#'
#' @param factors a list with the factors to be used for denormalizing
#'   (elements are "mean" and "sds"). Usually these are not specified and the
#'   values stored in \code{x} are used.
#'
#' @param ... further arguments (currently unused).
#'
#' @details Normalization tries to reduce the individual rating bias by
#' row centering the data, i.e., by
#' subtracting
#' from each available rating the mean of the ratings of that user (row).
#' Z-score in addition divides by the standard deviation of the row/column.
#' Normalization can also be done on columns.
#'
#' Denormalization reverses normalization. It uses the normalization
#' information stored in x unless the user specifies method, row and factors.
#'
#' @return A normalized realRatingMatrix.
#'
#' @examples ## create a matrix with ratings
#' m <- matrix(sample(c(NA,0:5),50, replace=TRUE, prob=c(.5,rep(.5/6,6))),
#' nrow=5, ncol=10, dimnames = list(users=paste('u', 1:5, sep=''),
#' items=paste('i', 1:10, sep='')))
#'
#' ## do normalization
#' r <- as(m, "realRatingMatrix")
#' r_n1 <- normalize(r)
#' r_n2 <- normalize(r, method="Z-score")
#'
#' r
#' r_n1
#' r_n2
#'
#' ## show normalized data
#' image(r, main="Raw Data")
#' image(r_n1, main="Centered")
#' image(r_n2, main="Z-Score Normalization")
#' @keywords manipulation
#' @family data preparation
#' @name normalize
setMethod("normalize", signature(x = "realRatingMatrix"),
  function(x, method="center", row=TRUE){

    if(is.null(method) || is.na(method)) return(x)

    rc <- if(row) "row" else "col"

    if(!is.null(x@normalize[[rc]])) {
      warning("x was already normalized by ", rc ,"!")
      return(x)
    }

    method <- tolower(method)
    methods <- c("center", "z-score")
    method_id <- pmatch(method, methods)
    if(length(method_id)!=1 || is.na(method_id)) stop("Unknown normalization method: ", method)

    means <- NULL
    sds <- NULL ## standard deviations for Z-score

    if(row) { ### row
      data <- t(x@data)
      means <- rowMeans(x)
      data@x <- data@x-rep(means, rowCounts(x))

      if(method_id==2) { ## Z-Score
        sds <- rowSds(x)
        sds[is.na(sds) | sds==0] <- 1
        data@x <- data@x/rep(sds, rowCounts(x))

      }
      data <- t(data)

    }else{ ### col
      data <- x@data
      means <- colMeans(x)
      data@x <- data@x-rep(means, colCounts(x))

      if(method_id==2) { ## Z-score
        sds <- colSds(x)
        sds[is.na(sds) | sds==0] <- 1
        data@x <- data@x/rep(sds, colCounts(x))
      }
    }

    # preserve zeros
    #x@data <- dropNA(data)
    data@x[data@x == 0] <- .Machine$double.xmin
    x@data <- data

    x@normalize[[rc]] <- list(method=methods[method_id],
      factors=list(means=means, sds=sds))
    x
  })

setMethod("denormalize", signature(x = "realRatingMatrix"),
  function(x, method=NULL, row=NULL, factors=NULL){

    ## check if x was normalized!
    if(is.null(method) && is.null(x@normalize)) return(x)

    ## row=NULL denormalizes all (row and col)
    if(is.null(row))
      return(denormalize(denormalize(x, row = FALSE), row = TRUE))

    ## start denormalization
    if(row) what <- "row" else what <- "col"
    if(is.null(x@normalize[[what]])) return(x)

    if(is.null(method)) method <- x@normalize[[what]]$method
    if(is.null(factors)) factors <- x@normalize[[what]]$factors

    method <- tolower(method)
    methods <- c("center", "z-score")
    method_id <- pmatch(method, methods)
    if(length(method_id)!=1 || is.na(method_id))
      stop("Unknown normalization method: ", method)

    means <- factors$means
    sds <- factors$sds

    if(row) { ### row
      data <- t(x@data)

      if(method_id==2) { ## Z-Score
        data@x <- data@x*rep(sds, rowCounts(x))
      }

      data@x <- data@x+rep(means, rowCounts(x))

      data <- t(data)

    }else{ ### col
      data <- x@data

      if(method_id==2) { ## Z-score
        data@x <- data@x*rep(sds, colCounts(x))
      }

      data@x <- as.numeric(data@x+rep(means, colCounts(x)))
    }

    # preserve zeros
    #x@data <- dropNA(data)
    data@x[data@x == 0] <- .Machine$double.xmin
    x@data <- data

    x@normalize[[what]] <- NULL
    if(length(x@normalize) == 0) x@normalize <- NULL

    x

  })
