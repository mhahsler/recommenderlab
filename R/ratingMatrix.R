## Each implementing class has to implement coercion to
## matrix, dgTMatrix, ngCMatrix, dgCMatrix

## object in data slot needs to have dim, dimnames and dimnames<- implemented

## dim
setMethod("dim", signature(x = "ratingMatrix"),
	function(x) dim(x@data))

## dimnames
setMethod("dimnames", signature(x = "ratingMatrix"),
	function(x) dimnames(x@data))

setReplaceMethod("dimnames", signature(x = "ratingMatrix",
		value = "list"), function(x, value) {
		dimnames(x@data) <- value
		x
	})

## coercion
setAs("ratingMatrix", "list", function(from) getList(from))

## this expects all ratingMatrices to be coercable to dgTMatrix
setMethod("getData.frame", signature(from = "ratingMatrix"),
	function(from, decode = TRUE, ratings = TRUE,...) {
	    dgT <- as(from, "dgTMatrix")

	    if(decode) {
		df <- data.frame(user=rownames(from)[dgT@i+1L],
			item=colnames(from)[dgT@j+1L],
			rating=dgT@x)
	    }else{
		df <- data.frame(user=dgT@i+1L,
			item=dgT@j+1L,
			rating=dgT@x)
	    }

	    if(!ratings) df <- df[,-3]

	    ## sort by users
	    df[order(df[,1]),]
	})

setAs("ratingMatrix", "data.frame", function(from) getData.frame(from))

## row/col counts, sums, etc.
## Matrix does not handle dimnames well
setMethod("colCounts", signature(x = "ratingMatrix"),
	function(x, ...) {
		s <- colSums(as(x, "ngCMatrix"))
		names(s) <- colnames(x)
		s
	})

setMethod("rowCounts", signature(x = "ratingMatrix"),
	function(x, ...) {
		s <- rowSums(as(x, "ngCMatrix"))
		names(s) <- rownames(x)
		s
	})

setMethod("colSums", signature(x = "ratingMatrix"),
	function(x, na.rm = FALSE, dims = 1, ...) {
		s <- colSums(as(x, "dgCMatrix"), na.rm, dims, ...)
		names(s) <- colnames(x)
		s
	})

setMethod("rowSums", signature(x = "ratingMatrix"),
	function(x, na.rm = FALSE, dims = 1, ...) {
		s <- rowSums(as(x, "dgCMatrix"), na.rm, dims, ...)
		names(s) <- rownames(x)
		s
	})

## we need to ignore 0s
setMethod("colMeans", signature(x = "ratingMatrix"),
	function(x, na.rm = FALSE, dims = 1, ...) {
		s <- colSums(x, dims, na.rm, ...) / colCounts(x, dims, na.rm, ...)
		names(s) <- colnames(x)
		s
	})

setMethod("rowMeans", signature(x = "ratingMatrix"),
	function(x, na.rm = FALSE, dims = 1, ...) {
		s <- rowSums(x, dims, na.rm, ...) / rowCounts(x, dims, na.rm, ...)
		names(s) <- rownames(x)
		s
	})

setMethod("nratings", signature(x = "ratingMatrix"),
	    function(x, ...) sum(rowCounts(x)))

setMethod("getNormalize", signature(x = "ratingMatrix"),
	    function(x, ...) x@normalize)

setMethod("getRatingMatrix", signature(x = "ratingMatrix"),
	    function(x, ...) x@data)

setMethod("getRatings", signature(x = "ratingMatrix"),
	    function(x, ...) as(x, "dgCMatrix")@x)

## subset
setMethod("[", signature(x = "ratingMatrix"),
	function(x, i, j, ..., drop) {
	    if(!missing(drop) && drop) warning("drop not implemented for ratingMatrix!")

	    if(missing(i)) i <- 1:nrow(x)
	    if(missing(j)) j <- 1:ncol(x)
	    if(is.null(i)) i <- integer(0)
	    if(is.null(j)) j <- integer(0)



	    x@data <- x@data[i,j, ..., drop=FALSE]
	    x
	})


## sample
setMethod("sample", signature(x = "ratingMatrix"),
	function(x, size, replace = FALSE, prob = NULL){
		index <- sample(c(1:nrow(x)), size = size,
			replace = replace, prob = prob)

		x[index,]
	})

## show
setMethod("show", signature(object = "ratingMatrix"),
  function(object) {
    cat(nrow(object), 'x', ncol(object), "rating matrix of class",
      sQuote(class(object)), "with",
      nratings(object), "ratings.\n")
    if(!is.null(object@normalize$row))
      cat("Normalized using",object@normalize$row$method,"on rows.\n")
    if(!is.null(object@normalize$col))
      cat("Normalized using",object@normalize$col$method,"on columns.\n")

    invisible(NULL)
  })

## image
setMethod("image", signature(x = "ratingMatrix"),
	function(x, xlab = "Items (Columns)", ylab = "Users (Rows)",
		colorkey=TRUE, ...) {

	## binaryRatingMatrix does not need a colorkey
	if(is(x, "binaryRatingMatrix")) colorkey <- FALSE

	Matrix::image(as(x, "dgTMatrix"), ylab = ylab, xlab = xlab,
		colorkey = colorkey, ...)
    })
