setClassUnion("listOrNull", c("list", "NULL"))

## FIXME: we cannot do this because Matrix does not export xMatrix!
## sparse matrix with NAs dropped
#setClass("sparseNAMatrix", contains = "dgCMatrix")

## Recommender
#' @title
#' Class "Recommender": A Recommender Model
#'
#' @description Represents a recommender model learned for a given data set
#' (a rating matrix).
#' @aliases show,Recommender-method
#' @docType class
#'
#' @seealso 	See \code{\link{Recommender}} for the constructor function and
#' 	a description of available methods.
#'
#' @rawRd \section{Objects from the Class}{
#' Objects are created by
#' the creator function \code{Recommender(data, method, parameter = NULL)}
#' }
#'
#' @rawRd \section{Slots}{
#' 	 \describe{
#'     \item{\code{method}:}{Object of class \code{"character"};
#' 	used recommendation method.}
#'     \item{\code{dataType}:}{Object of class \code{"character"};
#' 	concrete class of the input data.}
#'     \item{\code{ntrain}:}{Object of class \code{"integer"};
#' 	size of training set. }
#'     \item{\code{model}:}{Object of class \code{"list"}; the model. }
#'     \item{\code{predict}:}{Object of class \code{"function"}; code to compute
#' 	a recommendation using the model. }
#'   }
#' }
#'
#' @rawRd \section{Methods}{
#'   \describe{
#'     \item{getModel}{\code{signature(x = "Recommender")}: retrieve the model. }
#'     \item{predict}{\code{signature(object = "Recommender")}: create
#' 	recommendations for new data (argument \code{newdata}). }
#'     \item{show}{\code{signature(object = "Recommender")} }
#' 	 }
#' }
#' @keywords classes
#' @family recommender models
#' @name Recommender-class
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
#' @title
#' Class "ratingMatrix": Virtual Class for Rating Data
#'
#' @description Defines a common class for rating data.
#' @aliases ratingMatrix
#' @aliases coerce,ratingMatrix,list-method
#' @aliases coerce,ratingMatrix,data.frame-method
#' @aliases [,ratingMatrix,ANY,ANY,ANY-method
#' @aliases sample,ratingMatrix-method
#' @aliases colCounts
#' @aliases colCounts,ratingMatrix-method
#' @aliases rowCounts
#' @aliases rowCounts,ratingMatrix-method
#' @aliases colMeans,ratingMatrix-method
#' @aliases rowMeans,ratingMatrix-method
#' @aliases dim,ratingMatrix-method
#' @aliases dimnames<-,ratingMatrix,list-method
#' @aliases dimnames,ratingMatrix-method
#' @aliases nratings
#' @aliases nratings,ratingMatrix-method
#' @aliases show,ratingMatrix-method
#' @aliases image,ratingMatrix-method
#' @aliases getNormalize
#' @aliases getNormalize,ratingMatrix-method
#' @aliases getRatings
#' @aliases getRatings,ratingMatrix-method
#' @aliases hasRating
#' @aliases hasRating,ratingMatrix-method
#' @aliases getRatingMatrix
#' @aliases getRatingMatrix,ratingMatrix-method
#' @docType class
#'
#' @seealso 	See implementing classes
#' 	\code{\linkS4class{realRatingMatrix}}
#' 	and
#' 	\code{\linkS4class{binaryRatingMatrix}}.
#' 	See \code{\link{getList}},
#' 	\code{\link{getData.frame}},
#' 	\code{\link{similarity}},
#' 	\code{\link{dissimilarity}} and
#' 	\code{\link{dissimilarity}}.
#'
#' @rawRd \section{Objects from the Class}{A virtual Class: No objects may be created from it.}
#'
#' @rawRd \section{Methods}{
#' 	\describe{
#' 		\item{[}{\code{signature(x = "ratingMatrix", i = "ANY", j = "ANY", drop = "ANY")}: subset the rating matrix (\code{drop} is ignored). }
#' 		\item{coerce}{\code{signature(from = "ratingMatrix", to = "list")}}
#' 		\item{coerce}{\code{signature(from = "ratingMatrix", to = "data.frame")}: a data.frame with three columns. Col 1 contains user ids, col 2 contains        item ids and col 3 contains ratings.}
#' 		\item{colCounts}{\code{signature(x = "ratingMatrix")}:  number of ratings per column.}
#' 		\item{rowCounts}{\code{signature(x = "ratingMatrix")}:  number of ratings per row.}
#' 		\item{colMeans}{\code{signature(x = "ratingMatrix")}: column-wise rating means. }
#' 		\item{rowMeans}{\code{signature(x = "ratingMatrix")}: row-wise rating means. }
#' 		\item{dim}{\code{signature(x = "ratingMatrix")}: dimensions of the rating matrix. }
#' 		\item{dimnames<-}{\code{signature(x = "ratingMatrix", value = "list")}: replace dimnames. }
#' 		\item{dimnames}{\code{signature(x = "ratingMatrix")}: retrieve dimnames. }
#' 		\item{getNormalize}{\code{signature(x = "ratingMatrix")}: returns a list with normalization information for the matrix (NULL if data is not normalized). }
#'     \item{getRatings}{\code{signature(x = "ratingMatrix")}: returns all
#' 	ratings in \code{x} as a numeric vector. }
#'     \item{getRatingMatrix}{\code{signature(x = "ratingMatrix")}: returns the ratings as a sparse matrix. The format differs between binary and real rating matrices.}
#'     \item{hasRating}{\code{signature(x = "ratingMatrix")}: returns a sparse logical matrix with TRUE for user-item combinations which have a rating. }
#' 		\item{image}{\code{signature(x = "ratingMatrix")}: plot the matrix. }
#' 		\item{nratings}{\code{signature(x = "ratingMatrix")}: number of ratings in the matrix. }
#' 		\item{sample}{\code{signature(x = "ratingMatrix")}: sample from users (rows). }
#' 		\item{show}{\code{signature(object = "ratingMatrix")} }
#'
#' }
#' }
#' @keywords classes
#' @family rating data
#' @name ratingMatrix-class
setClass("ratingMatrix",
	representation(
		normalize = "listOrNull"
	))

## uses itemMatrix from arules
#' @title
#' Class "binaryRatingMatrix": A Binary Rating Matrix
#'
#' @description A matrix for binary rating data. A value of 1 indicates a positive
#' rating; 0 indicates no rating or a negative rating. This coding is common for
#' market-basket data, where products are either bought or not.
#' @aliases binaryRatingMatrix
#' @aliases coerce,matrix,binaryRatingMatrix-method
#' @aliases coerce,itemMatrix,binaryRatingMatrix-method
#' @aliases coerce,data.frame,binaryRatingMatrix-method
#' @aliases coerce,binaryRatingMatrix,matrix-method
#' @aliases coerce,binaryRatingMatrix,dgTMatrix-method
#' @aliases coerce,binaryRatingMatrix,ngCMatrix-method
#' @aliases coerce,binaryRatingMatrix,dgCMatrix-method
#' @aliases coerce,binaryRatingMatrix,itemMatrix-method
#' @aliases coerce,binaryRatingMatrix,list-method
#' @docType class
#'
#' @seealso 	\code{\link[arules:itemMatrix-class]{itemMatrix}} in \pkg{arules},
#' 	\code{\link{getList}}.
#'
#' @rawRd \section{Objects from the Class}{
#' Objects can be created by calls of the form \code{new("binaryRatingMatrix", data = im)}, where \code{im} is an \code{itemMatrix} as defined in package
#' \pkg{arules}, by coercion from a matrix (all non-zero values will be a 1),
#' or by using \code{binarize} for
#' an object of class "realRatingMatrix".
#' }
#'
#' @rawRd \section{Slots}{
#' 	 \describe{
#'     \item{\code{data}:}{Object of class \code{"itemMatrix"} (see package \pkg{arules})}
#'   }
#' }
#'
#' @rawRd \section{Extends}{
#' Class \code{"\linkS4class{ratingMatrix}"}, directly.
#' }
#'
#' @rawRd \section{Methods}{
#'   \describe{
#'     \item{coerce}{\code{signature(from = "matrix", to = "binaryRatingMatrix")}:
#'     The matrix needs to be a logical matrix, or a 0-1 matrix (0 means FALSE and 1 means TRUE).
#'     NAs are interpreted as FALSE.
#'     }
#'     \item{coerce}{\code{signature(from = "itemMatrix", to = "binaryRatingMatrix")}}
#'     \item{coerce}{\code{signature(from = "data.frame", to = "binaryRatingMatrix")}}
#'     \item{coerce}{\code{signature(from = "binaryRatingMatrix", to = "matrix")}}
#'     \item{coerce}{\code{signature(from = "binaryRatingMatrix", to = "dgTMatrix")}}
#'     \item{coerce}{\code{signature(from = "binaryRatingMatrix", to = "ngCMatrix")}}
#'     \item{coerce}{\code{signature(from = "binaryRatingMatrix", to = "dgCMatrix")}}
#'     \item{coerce}{\code{signature(from = "binaryRatingMatrix", to = "itemMatrix")}}
#'     \item{coerce}{\code{signature(from = "binaryRatingMatrix", to = "list")}}
#' %    \item{dissimilarity}{\code{signature(x = "binaryRatingMatrix")}}
#' %    \item{LIST}{\code{signature(from = "binaryRatingMatrix")}: ... }
#' 	 }
#' }
#'
#' @examples ## create a 0-1 matrix
#' m <- matrix(sample(c(0,1), 50, replace=TRUE), nrow=5, ncol=10,
#'     dimnames=list(users=paste("u", 1:5, sep=''),
#'     items=paste("i", 1:10, sep='')))
#' m
#'
#' ## coerce it into a binaryRatingMatrix
#' b <- as(m, "binaryRatingMatrix")
#' b
#'
#' ## coerce it back to see if it worked
#' as(b, "matrix")
#'
#' ## use some methods defined in ratingMatrix
#' dim(b)
#' dimnames(b)
#'
#' ## counts
#' rowCounts(b) ## number of ratings per user
#' colCounts(b) ## number of ratings per item
#'
#' ## plot
#' image(b)
#'
#' ## sample and subset
#' sample(b,2)
#' b[1:2,1:5]
#'
#' ## coercion
#' as(b, "list")
#' head(as(b, "data.frame"))
#' head(getData.frame(b, ratings=FALSE))
#'
#' ## creation from user/item tuples
#' df <- data.frame(user=c(1,1,2,2,2,3), items=c(1,4,1,2,3,5))
#' df
#' b2 <- as(df, "binaryRatingMatrix")
#' b2
#' as(b2, "matrix")
#' @keywords classes
#' @family rating data
#' @name binaryRatingMatrix-class
setClass("binaryRatingMatrix",
	contains="ratingMatrix",
	representation(
		data = "itemMatrix"
	))

### Legacy data:
#setClassUnion("sparseNAMatrix_legacy", c("sparseNAMatrix", "dgCMatrix"))

#' @title
#' Class "realRatingMatrix": Real-valued Rating Matrix
#'
#' @description A matrix containing ratings (typically 1-5 stars, etc.).
#' @aliases realRatingMatrix
#' @aliases coerce,matrix,realRatingMatrix-method
#' @aliases coerce,realRatingMatrix,matrix-method
#' @aliases coerce,realRatingMatrix,dgTMatrix-method
#' @aliases coerce,dgTMatrix,realRatingMatrix-method
#' @aliases coerce,realRatingMatrix,ngCMatrix-method
#' @aliases coerce,realRatingMatrix,dgCMatrix-method
#' @aliases coerce,dgCMatrix,realRatingMatrix-method
#' @aliases coerce,data.frame,realRatingMatrix-method
#' @aliases coerce,realRatingMatrix,data.frame-method
#' @aliases rowSds
#' @aliases rowSds,realRatingMatrix-method
#' @aliases colSds
#' @aliases colSds,realRatingMatrix-method
#' @aliases binarize
#' @aliases binarize,realRatingMatrix-method
#' @aliases removeKnownRatings
#' @aliases removeKnownRatings,realRatingMatrix-method
#' @aliases [<-,realRatingMatrix,ANY,ANY,ANY-method
#' @aliases getTopNLists
#' @aliases getTopNLists,realRatingMatrix-method
#' @docType class
#'
#' @seealso 	See \code{\linkS4class{ratingMatrix}} inherited methods,
#' %	\code{\linkS4class{sparseNAMatrix}},
#' 	\code{\linkS4class{binaryRatingMatrix}},
#' 	\code{\linkS4class{topNList}},
#' 	\code{\link{getList}} and \code{\link{getData.frame}}.
#' 	Also see \code{\link[Matrix]{dgCMatrix-class}},
#' 	\code{\link[Matrix]{dgTMatrix-class}} and
#' 	\code{\link[Matrix]{ngCMatrix-class}}
#' 	in \pkg{Matrix}.
#'
#' @rawRd \section{Objects from the Class}{
#' Objects can be created by calls of the form \code{new("realRatingMatrix", data = m)}, where \code{m} is sparse matrix of class
#'   %\code{sparseNAMatrix} (subclass of
#'     \code{dgCMatrix} in package \pkg{Matrix} %)
#'   or by coercion from a regular matrix, a data.frame containing user/item/rating triplets as rows, or
#'   a sparse matrix in triplet form (\code{dgTMatrix} in package \pkg{Matrix}).
#' }
#'
#' @rawRd \section{Slots}{
#'     \describe{
#' 	\item{\code{data}:}{Object of class
#' 	  %\code{sparseNAMatrix} which is a subclass of
#' 	  \code{"dgCMatrix"}, a sparse matrix
#' 	    defined in package \pkg{Matrix}. Note that this matrix drops NAs instead
#' 	    of zeroes. Operations on \code{"dgCMatrix"} potentially will delete
#' 	    zeroes.}
#' 	\item{\code{normalize}:}{\code{NULL} or a list with normalization factors. }
#'     }
#' }
#'
#' @rawRd \section{Extends}{
#' Class \code{"\linkS4class{ratingMatrix}"}, directly.
#' }
#'
#' @rawRd \section{Methods}{
#'   \describe{
#'     \item{coerce}{\code{signature(from = "matrix", to = "realRatingMatrix")}: Note that
#'     unknown ratings have to be encoded in the matrix as NA and not as 0 (which would mean an actual rating of 0).}
#'     \item{coerce}{\code{signature(from = "realRatingMatrix", to = "matrix")}}
#'     \item{coerce}{\code{signature(from = "data.frame", to = "realRatingMatrix")}:
#' 	coercion from a data.frame with three columns.
#' 	Col 1 contains user ids, col 2 contains	item ids and
#' 	col 3 contains ratings.}
#'     \item{coerce}{\code{signature(from = "realRatingMatrix", to = "data.frame")}: produces user/item/rating triplets.}
#'     \item{coerce}{\code{signature(from = "realRatingMatrix", to = "dgTMatrix")}}
#'     \item{coerce}{\code{signature(from = "dgTMatrix", to = "realRatingMatrix")}}
#'     \item{coerce}{\code{signature(from = "realRatingMatrix", to = "dgCMatrix")}}
#'     \item{coerce}{\code{signature(from = "dgCMatrix", to = "realRatingMatrix")}}
#'     \item{coerce}{\code{signature(from = "realRatingMatrix", to = "ngCMatrix")}}
#'
#'     \item{binarize}{\code{signature(x = "realRatingMatrix")}: create a
#'         \code{"binaryRatingMatrix"} by setting all ratings larger or equal to
#'         the argument \code{minRating} as 1 and all others to 0.}
#'     	\item{getTopNLists}{\code{signature(x = "realRatingMatrix")}: create
#' 	     top-N lists from the ratings in x. Arguments are
#' 	     \code{n} (defaults to 10),
#' 	     \code{randomize} (default is \code{NULL}) and
#' 		   \code{minRating} (default is \code{NA}).
#' 		   Items with a rating below \code{minRating} will not be part of the
#' 		   top-N list. \code{randomize} can be used to get diversity in the
#' 		   predictions by randomly selecting items with a bias to higher rated
#' 		   items. The bias is introduced by choosing the items with a probability
#' 		   proportional to the rating \eqn{(r-min(r)+1)^{randomize}}.
#' 		   The larger the value
#' 		   the more likely it is to get very highly rated items and a negative
#' 		   value for \code{randomize} will select low-rated items. }
#'     \item{removeKnownRatings}{\code{signature(x = "realRatingMatrix")}: removes
#' 	all ratings in \code{x} for which ratings are available in
#' 	the realRatingMatrix (of same dimensions as \code{x})
#' 	passed as the argument \code{known}. }
#'     \item{rowSds}{\code{signature(x = "realRatingMatrix")}: calculate
#' 	the standard deviation of ratings for rows (users).}
#'     \item{colSds}{\code{signature(x = "realRatingMatrix")}: calculate
#' 	the standard deviation of ratings for columns (items).}
#' 	 }
#' }
#'
#' @examples ## create a matrix with ratings
#' m <- matrix(sample(c(NA,0:5),100, replace=TRUE, prob=c(.7,rep(.3/6,6))),
#' 	nrow=10, ncol=10, dimnames = list(
#' 	    user=paste('u', 1:10, sep=''),
#' 	    item=paste('i', 1:10, sep='')
#'     ))
#' m
#'
#' ## Coerce into a realRatingMatrix
#' r <- as(m, "realRatingMatrix")
#' r
#'
#' ## get some information
#' dimnames(r)
#' rowCounts(r) ## number of ratings per user
#' colCounts(r) ## number of ratings per item
#' colMeans(r) ## average item rating
#' nratings(r) ## total number of ratings
#' hasRating(r) ## user-item combinations with ratings
#'
#' ## histogram of ratings
#' hist(getRatings(r), breaks="FD")
#'
#' ## inspect a subset
#' image(r[1:5,1:5])
#'
#' ## coerce it back to see if it worked
#' as(r, "matrix")
#'
#' ## coerce to data.frame (user/item/rating triplets)
#' as(r, "data.frame")
#'
#' ## binarize into a binaryRatingMatrix with all 4+ rating a 1
#' b <- binarize(r, minRating=4)
#' b
#' as(b, "matrix")
#' @keywords classes
#' @family rating data
#' @name realRatingMatrix-class
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
#' @title
#' Class "topNList": Top-N List
#'
#' @description Represents recommendations as a Top-N list.
#' @aliases topNList
#' @aliases bestN
#' @aliases bestN,topNList-method
#' @aliases coerce,topNList,dgTMatrix-method
#' @aliases coerce,topNList,dgCMatrix-method
#' @aliases coerce,topNList,ngCMatrix-method
#' @aliases coerce,topNList,matrix-method
#' @aliases coerce,topNList,list-method
#' @aliases coerce,topNList,realRatingMatrix-method
#' @aliases colCounts,topNList-method
#' @aliases rowCounts,topNList-method
#' @aliases show,topNList-method
#' @aliases length,topNList-method
#' @aliases removeKnownItems
#' @aliases removeKnownItems,topNList-method
#' @aliases c,topNList-method
#' @docType class
#'
#' @seealso \code{\link{evaluate}},
#' \code{\link{getList}},
#' \code{\linkS4class{realRatingMatrix}}
#'
#' @rawRd \section{Objects from the Class}{
#' Objects can be created by
#' \code{predict} with a recommender model and new data. Alternatively,
#' objects can be created from a realRatingMatrix using
#' \code{\link{getTopNLists}}.
#' }
#'
#' @rawRd \section{Slots}{
#'     \describe{
#' 	\item{\code{ratings}:}{Object of class \code{"list"}.
#' 		Each element in the list represents a top-N recommendation
#' 		(an integer vector) with item IDs (column numbers in the rating
#' 		matrix). The items are ordered in each vector.}
#' 	\item{\code{items}:}{Object of class \code{"list"} or \code{NULL}.
#' 	  If available, a list of the same structure as \code{items} with the
#' 	  ratings. }
#' 	\item{\code{itemLabels}:}{Object of class \code{"character"}}
#' 	\item{\code{n}:}{Object of class \code{"integer"} specifying the
#' 		number of items in each recommendation.
#' 		Note that the actual number
#' 		on recommended items can be less depending on the data and the
#' 		used algorithm.}
#' 	}
#' }
#'
#' @rawRd \section{Methods}{
#'     \describe{
#' 	\item{coerce}{\code{signature(from = "topNList", to = "list")}: returns a
#' 	  list with the items (labels) in the topNList. }
#' 	\item{coerce}{\code{signature(from = "topNList", to = "realRatingMatrix")}: creates a rating Matrix with entries for the items in the topN list.}
#' 	\item{coerce}{\code{signature(from = "topNList", to = "dgTMatrix")}}
#' 	\item{coerce}{\code{signature(from = "topNList", to = "dgCMatrix")}}
#' 	\item{coerce}{\code{signature(from = "topNList", to = "ngCMatrix")}}
#' 	\item{coerce}{\code{signature(from = "topNList", to = "matrix")}: returns
#' 	a dense matrix with the ratings for the top-N items. All other items have a rating of NA.}
#' 	\item{c}{\code{signature(x = "topNList")}: combine several topN lists into a single list. The lists need to be for the same data (i.e., items). }
#' 	\item{bestN}{\code{signature(x = "topNList")}: returns only the best
#' 		n recommendations (second argument is \code{n} which defaults to 10).
#' 		The additional argument \code{minRating} can be used to remove all
#' 		entries with a rating below this value. }
#' 	\item{length}{\code{signature(x = "topNList")}: for how many users
#' 	    does this object contain a top-N list? }
#' 	\item{removeKnownItems}{\code{signature(x = "topNList")}:
#' 	    remove items from the top-N list which are known (have a rating)
#' 	    for the user given as a ratingMatrix passed on as argument
#' 		\code{known}. }
#' 	\item{colCounts}{\code{signature(x = "topNList")}: in how many top-N
#' 		does each item occur? }
#' 	\item{rowCounts}{\code{signature(x = "topNList")}: number of recommendations per user. }
#' 	\item{show}{\code{signature(object = "topNList")}}
#' 	}
#' }
#' @keywords classes
#' @family recommendations
#' @name topNList-class
setClass("topNList",
	representation(
		items   = "list",
	  ratings = "listOrNull",
		itemLabels= "character",
		n       = "integer"
	),
  validity = function(object) {
    if(!all(sapply(object@items, is.integer)))
      stop("The items slot must contain a list of integer item IDs.")
    if(!is.null(object@ratings) &&
        any(sapply(object@items, length) != sapply(object@ratings, length)))
      stop("The ratings and items lists must have matching lengths.")

    TRUE
  }
)


## Evaluation
#' @title
#' Class "evaluationScheme": Evaluation Scheme
#'
#' @description 	An evaluation scheme created from a data set. The scheme can be a simple split into training and test data, k-fold cross-evaluation or using k
#' 	bootstrap samples.
#' @aliases getData
#' @aliases getData,evaluationScheme-method
#' @aliases show,evaluationScheme-method
#' @docType class
#'
#' @seealso 	\code{\linkS4class{ratingMatrix}} and
#' 	the creator function \code{\link{evaluationScheme}}.
#'
#' @rawRd \section{Objects from the Class}{
#' Objects can be created by
#' \code{evaluationScheme(data, method="split", train=0.9, k=NULL, given=3).}
#' }
#'
#' @rawRd \section{Slots}{
#' 	 \describe{
#'     \item{\code{data}:}{Object of class \code{"ratingMatrix"}; the data set. }
#'     \item{\code{given}:}{Object of class \code{"integer"}; given ratings are
#'     randomly selected for each evaluation user and
#'     presented to the recommender
#'     algorithm to calculate recommend items/ratings.
#'     The recommended items are compared
#'     to the remaining items for the evaluation user.}
#'     \item{\code{goodRating}:}{Object of class \code{"numeric"}; Rating at which an item is considered a positive for evaluation. }
#'     \item{\code{k}:}{Object of class \code{"integer"}; number of runs for evaluation. Default is 1 for method "split" and 10 for "cross-validation" and "bootstrap".}
#'     \item{\code{knownData}:}{Object of class \code{"ratingMatrix"}; data set with only known (given) items. }
#'     \item{\code{method}:}{Object of class \code{"character"}; evaluation method. Available methods are: "split", "cross-validation" and "bootstrap".}
#'     \item{\code{runsTrain}:}{Object of class \code{"list"}; internal representation of the training and test data splits for the evaluation runs.}
#'     \item{\code{train}:}{Object of class \code{"numeric"}; portion of data used for training for "split" and "bootstrap".}
#'     \item{\code{unknownData}:}{Object of class \code{"ratingMatrix"}; data set with only unknown items. }
#'   }
#' }
#'
#' @rawRd \section{Methods}{
#'   \describe{
#' %    \item{evaluate}{\code{signature(x = "evaluationScheme")}: ... }
#'     \item{getData}{\code{signature(x = "evaluationScheme")}: access data.
#' 	Parameters are \code{type} ("train", "known" or "unknown", "given") and
#' 	\code{run} (1...k).
#' 	\code{"train"} returns the training data for the run,
#' 	\code{"known"} returns the known ratings used for prediction
#' 	for the test data,
#' 	\code{"unknown"} returns the ratings used for evaluation
#' 	for the test data, and
#' 	\code{"given"} returns the number of items that were given in "known." If the \code{given} items
#' 	was a positive number, then this will be a vector with this number, but if \code{given} was negative (all-but-x),
#' 	then the number of given items for each test user will be different.
#' 	}
#'     \item{show}{\code{signature(object = "evaluationScheme")} }
#' 	 }
#' }
#' @keywords classes
#' @family evaluation
#' @name evaluationScheme-class
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

#' @title
#' Class "evaluationResults": Results of the Evaluation of a Single Recommender Method
#'
#' @description Contains evaluation results for several runs of the same recommender method, represented as confusion matrices. The model used for each run may also be available.
#' @aliases confusionMatrix-class
#' @aliases avg
#' @aliases avg,evaluationResults-method
#' @aliases getConfusionMatrix
#' @aliases getConfusionMatrix,evaluationResults-method
#' @aliases getResults
#' @aliases getResults,evaluationResults-method
#' @aliases getModel
#' @aliases getModel,evaluationResults-method
#' @aliases getRuns
#' @aliases getRuns,evaluationResults-method
#' @aliases show,evaluationResults-method
#' @docType class
#'
#' @seealso 	\code{\link{evaluate}}
#'
#' @rawRd \section{Objects from the Class}{
#' Objects are created by \code{evaluate}.
#' }
#'
#' @rawRd \section{Slots}{
#' 	 \describe{
#'     \item{\code{results}:}{Object of class \code{"list"}: contains
#' 	objects of class \code{"ConfusionMatrix"}, one for each run specified
#' 	in the used evaluation scheme.}
#'   }
#' }
#'
#' @rawRd \section{Methods}{
#'   \describe{
#'     \item{avg}{\code{signature(x = "evaluationResults")}: returns evaluation metrics averaged across cross-validation folds. }
#'     \item{getConfusionMatrix}{\code{signature(x = "evaluationResults")}:
#' 	Deprecated. Use \code{getResults()}.}
#'     \item{getResults}{\code{signature(x = "evaluationResults")}:
#' 	returns a list of evaluation metrics with one element for each cross-validation fold.}
#'     \item{getModel}{\code{signature(x = "evaluationResults")}: returns a
#' 	list of recommender models used, if available. }
#'     \item{getRuns}{\code{signature(x = "evaluationResults")}: returns
#' 	the number of runs/number of confusion matrices.}
#' %    \item{plot}{\code{signature(x = "evaluationResults")}: plot }
#'     \item{show}{\code{signature(object = "evaluationResults")} }
#' 	 }
#' }
#' @keywords classes
#' @family evaluation
#' @name evaluationResults-class
setClass("evaluationResults",
	representation(
		results	= "list",	## list of confusionMatrix
		method	= "character"
	)
)

#' @title
#' Class "evaluationResultList": Results from Evaluating Multiple Recommender Methods
#'
#' @description Contains evaluation results for several runs of multiple recommender methods, represented as confusion matrices. The models used for each run may also be available.
#' @aliases coerce,list,evaluationResultList-method
#' @aliases avg,evaluationResultList-method
#' @aliases [,evaluationResultList,ANY,missing,missing-method
#' @aliases show,evaluationResultList-method
#' @docType class
#'
#' @seealso 	\code{\link{evaluate}},
#' 	\code{\linkS4class{evaluationResults}}.
#'
#' @rawRd \section{Objects from the Class}{
#' Objects are created by \code{evaluate}.
#' }
#'
#' @rawRd \section{Slots}{
#' 	 \describe{
#'     \item{\code{.Data}:}{Object of class \code{"list"}: a list of
#' 	\code{"evaluationResults"}.}
#'   }
#' }
#'
#' @rawRd \section{Extends}{
#' Class \code{"\linkS4class{list}"}, from data part.
#' %Class \code{"\linkS4class{vector}"}, by class "list", distance 2.
#' %Class \code{"\linkS4class{listOrNull}"}, by class "list", distance 2.
#' }
#'
#' @rawRd \section{Methods}{
#'   \describe{
#'     \item{avg}{\code{signature(x = "evaluationResultList")}: returns a
#' 	list of average confusion matrices.}
#'     \item{[}{\code{signature(x = "evaluationResultList", i = "ANY", j = "missing", drop = "missing")}}
#' %    \item{plot}{\code{signature(x = "evaluationResultList")}: ... }
#'     \item{coerce}{\code{signature(from = "list", to = "evaluationResultList")}}
#' 	\item{show}{\code{signature(object = "evaluationResultList")}}
#' 	 }
#' }
#' @keywords classes
#' @family evaluation
#' @name evaluationResultList-class
setClass("evaluationResultList",
	contains="list"			## list of evaluationResults
)
