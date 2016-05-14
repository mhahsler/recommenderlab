## helper
setClassUnion("listOrNull", c("list", "NULL"))

## sparse matrix with NAs dropped
setClass("sparseNAMatrix", contains = "dgCMatrix")

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
setClassUnion("sparseNAMatrix_legacy", c("sparseNAMatrix", "dgCMatrix"))

setClass("realRatingMatrix",
	contains="ratingMatrix",
	representation(
		#data = "sparseNAMatrix"
		data = "sparseNAMatrix_legacy"
	))


## Top-N list
## items is a list of index vectors with the top N items.
setClass("topNList",
	representation(
		items   = "list",
	  ratings = "listOrNull",
		itemLabels= "character",
		n       = "integer"
	)
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
		model	= "listOrNull"
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
