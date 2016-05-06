setMethod("normalize", signature(x = "realRatingMatrix"),
  function(x, method="center", row=TRUE){
    
    if(!is.null(x@normalize)) {
      warning("x was already normalized!")
      return(x)
    }
    
    
    methods <- c("center", "Z-score")
    method_id <- pmatch(method, methods)
    if(is.na(method_id)) stop("Unknown normalization method!")
    
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
      x@data <- t(data)
      
    }else{ ### col
      data <- x@data
      means <- colMeans(x)
      data@x <- data@x-rep(means, colCounts(x))
      
      if(method_id==2) { ## Z-score
        sds <- colSds(x)
        sds[is.na(sds) | sds==0] <- 1
        data@x <- data@x/rep(sds, colCounts(x))
      }
      x@data <- data
    }
    
    x@normalize <- list(method=methods[method_id], row=row,
      factors=list(means=means, sds=sds))
    x
  })

setMethod("denormalize", signature(x = "realRatingMatrix"),
  function(x, method=NULL, row=NULL, factors=NULL){
    
    ## check if x was normalized!
    if(is.null(method) && is.null(x@normalize)) return(x)
    
    if(is.null(row)) row <- x@normalize$row
    if(is.null(method)) method <- x@normalize$method
    if(is.null(factors)) factors <- x@normalize$factors
    
    methods <- c("center", "Z-score")
    method_id <- pmatch(method, methods)
    if(is.na(method_id)) stop("Unknown normalization method!")
    
    means <- factors$means
    sds <- factors$sds
    
    if(row) { ### row
      data <- t(x@data)
      
      if(method_id==2) { ## Z-Score
        data@x <- data@x*rep(sds, rowCounts(x))
        
      }
      
      data@x <- data@x+rep(means, rowCounts(x))
      
      x@data <- t(data)
      
    }else{ ### col
      data <- x@data
      
      if(method_id==2) { ## Z-score
        data@x <- data@x/rep(sds, colCounts(x))
      }
      
      data@x <- as.numeric(data@x+rep(means, colCounts(x)))
      
      x@data <- data
    }
    
    x@normalize <- NULL
    x
    
  })
