setMethod("normalize", signature(x = "realRatingMatrix"),
  function(x, method="center", row=TRUE){

    if(is.null(method) || is.na(method)) return(x)

    rc <- if(row) "row" else "col"

    if(!is.null(x@normalize[[rc]])) {
      warning("x was already normalized by ", rc ,"!")
      return(x)
    }


    methods <- c("center", "Z-score")
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

    x@data <- dropNA(data)

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

    methods <- c("center", "Z-score")
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
        data@x <- data@x/rep(sds, colCounts(x))
      }

      data@x <- as.numeric(data@x+rep(means, colCounts(x)))
    }

    x@data <- dropNA(data)

    x@normalize[[what]] <- NULL
    if(length(x@normalize) == 0) x@normalize <- NULL

    x

  })
