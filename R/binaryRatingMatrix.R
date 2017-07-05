## binaryRatingMatrix based on itemMatrix (arules)

## Coercions
setAs("matrix", "binaryRatingMatrix",
	function(from) new("binaryRatingMatrix",
		data = as(from, "itemMatrix")))

setAs("binaryRatingMatrix", "matrix",
	function(from) as(from@data, "matrix"))

setAs("itemMatrix", "binaryRatingMatrix",
	function(from) new("binaryRatingMatrix",
		data = from))

setAs("binaryRatingMatrix", "itemMatrix",
	function(from) from@data)

## itemMatrix stores data transposed!
setAs("binaryRatingMatrix", "ngCMatrix",
	function(from) t(as(from@data, "ngCMatrix")))

setAs("binaryRatingMatrix", "dgCMatrix",
	function(from) as(as(from, "ngCMatrix"), "dgCMatrix"))

setAs("binaryRatingMatrix", "dgTMatrix",
	function(from) as(as(from, "dgCMatrix"), "dgTMatrix"))

## list
setMethod("getList", signature(from = "binaryRatingMatrix"),
	function(from, decode = TRUE, ...) {
		LIST(from@data, decode = decode)
	}
)

## FIXME: we could do this cheaper
setAs("data.frame", "binaryRatingMatrix",
	function(from) {
	    rr <- as(from, "realRatingMatrix")
	    binarize(rr, minRating=-Inf)
	})


## FIXME: removeKnownRatings should be implemented here!
#setMethod("removeKnownRatings", signature(x = "binaryRatingMatrix"),
#	function(x, known) {
#	    if(!is(known, "ratingMatrix")) stop("known needs to be a ratingMatrix!")
#
#	    stop("Currently not implemented!")
#	})

## split test data
setMethod(".splitKnownUnknown", signature(data="binaryRatingMatrix"),
	function(data, given) {

		## given might of length one or length(data)
		if(length(given)==1) given <- rep(given, nrow(data))
		nitems <- rowCounts(data)

		allBut <- given < 0
		if(any(allBut)) {
		  given[allBut] <- nitems[allBut] + given[allBut]
		}

		if(any(given>nitems)) stop("Not enough ratings for user" ,
		  paste(which(given>nitems), collapse = ", "))

		l <- getList(data, decode=FALSE)
		known_index <- lapply(1:length(l),
			FUN = function(i) sample(1:length(l[[i]]), given[i]))

		known <- encode(
			lapply(1:length(l), FUN = function(x)
				l[[x]][known_index[[x]]]),
			itemLabels = itemLabels(data@data))
    rownames(known) <- rownames(data)

		unknown <- encode(
			lapply(1:length(l), FUN = function(x)
				l[[x]][-known_index[[x]]]),
			itemLabels = itemLabels(data@data))
    rownames(unknown) <- rownames(data)

		known <- new("binaryRatingMatrix", data = known)
		unknown <- new("binaryRatingMatrix", data = unknown)

		list(
			known = known,
			unknown = unknown
		)
	})


