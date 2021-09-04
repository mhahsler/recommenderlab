setClassUnion("listOrNull", c("list", "NULL"))

## FIXME: we cannot do this because Matrix does not export xMatrix!
## sparse matrix with NAs dropped
#setClass("sparseNAMatrix", contains = "dgCMatrix")

## Recommender
setClass("Recommender",
	representation(
		method	= "character",
		dataType= "character",
		ntrain	= "integer",
		model	= "list",
		predict = "function"
	)
)

setClassUnion("RecommenderOrNull", c("Recommender", "NULL"))

## Ratings
setClass("ratingMatrix",
	representation(
		normalize = "listOrNull"
	))

## uses itemMatrix from arules
setClass("binaryRatingMatrix",
	contains="ratingMatrix",
	representation(
		data = "itemMatrix"
	))

### Legacy data:
#setClassUnion("sparseNAMatrix_legacy", c("sparseNAMatrix", "dgCMatrix"))

setClass("realRatingMatrix",
  contains="ratingMatrix",
  representation(
    #data = "sparseNAMatrix"
    #data = "sparseNAMatrix_legacy"
    data = "dgCMatrix"
  ) #,
  #validity = function(object) {
  #  if(!is(object@data, "sparseNAMatrix")) warning("dgCMatrix in realRatingMatrix is deprecated (should be sparseNAMatrix). Use object@data <- as(object@data, \"sparseNAMatrix\") to fix this issue.")
  #  TRUE
  #}
)


## Top-N list
## items is a list of index vectors with the top N items.
setClass("topNList",
	representation(
		items   = "list",
	  ratings = "listOrNull",
		itemLabels= "character",
		n       = "integer"
	),
  validity = function(object) {
    if(!all(sapply(object@items, is.integer)))
      stop("items slot needs to contain a list of item ids (interger).")
    if(!is.null(object@ratings) &&
        any(sapply(object@items, length) != sapply(object@ratings, length)))
      stop("ratings and items do not aggree with each other")

    TRUE
  }
)


## Evaluation
setClass("evaluationScheme",
	representation(
		method	= "character",
		given	= "integer",
		k	= "integer",
		train	= "numeric",
		runsTrain= "list",
		data	= "ratingMatrix",
		knownData= "ratingMatrix",
		unknownData= "ratingMatrix",
		goodRating = "numeric"
	)
)

setClass("confusionMatrix",
	representation(
		cm	= "matrix",
		model	= "RecommenderOrNull"
	)
)

setClass("evaluationResults",
	representation(
		results	= "list",	## list of confusionMatrix
		method	= "character"
	)
)

setClass("evaluationResultList",
	contains="list"			## list of evaluationResults
)
