\name{realRatingMatrix}
\Rdversion{1.1}
\docType{class}
\alias{realRatingMatrix-class}
\alias{realRatingMatrix}
\alias{coerce,matrix,realRatingMatrix-method}
\alias{coerce,realRatingMatrix,matrix-method}
\alias{coerce,realRatingMatrix,dgTMatrix-method}
\alias{coerce,dgTMatrix,realRatingMatrix-method}
\alias{coerce,realRatingMatrix,ngCMatrix-method}
\alias{coerce,realRatingMatrix,dgCMatrix-method}
\alias{coerce,dgCMatrix,realRatingMatrix-method}
\alias{coerce,data.frame,realRatingMatrix-method}
\alias{coerce,realRatingMatrix,data.frame-method}
\alias{rowSds}
\alias{rowSds,realRatingMatrix-method}
\alias{colSds}
\alias{colSds,realRatingMatrix-method}
%\alias{LIST,realRatingMatrix-method}
\alias{binarize}
\alias{binarize,realRatingMatrix-method}
\alias{removeKnownRatings}
\alias{removeKnownRatings,realRatingMatrix-method}
\alias{[<-,realRatingMatrix,ANY,ANY,ANY-method}
\alias{getTopNLists}
\alias{getTopNLists,realRatingMatrix-method}

\title{Class "realRatingMatrix": Real-valued Rating Matrix}
\description{A matrix containing ratings (typically 1-5 stars, etc.).}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("realRatingMatrix", data = m)}, where \code{m} is sparse matrix of class
  %\code{sparseNAMatrix} (subclass of
    \code{dgCMatrix} in package \pkg{Matrix} %)
  or by coercion from a regular matrix, a data.frame containing user/item/rating triplets as rows, or
  a sparse matrix in triplet form (\code{dgTMatrix} in package \pkg{Matrix}).
}
\section{Slots}{
    \describe{
	\item{\code{data}:}{Object of class
	  %\code{sparseNAMatrix} which is a subclass of
	  \code{"dgCMatrix"}, a sparse matrix
	    defined in package \pkg{Matrix}. Note that this matrix drops NAs instead
	    of zeroes. Operations on \code{"dgCMatrix"} potentially will delete
	    zeroes.}
	\item{\code{normalize}:}{\code{NULL} or a list with normalizaton factors. }
    }
}
\section{Extends}{
Class \code{"\linkS4class{ratingMatrix}"}, directly.
}
\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "matrix", to = "realRatingMatrix")}: Note that
    unknown ratings have to be encoded in the matrix as NA and not as 0 (which would mean an actual rating of 0).}
    \item{coerce}{\code{signature(from = "realRatingMatrix", to = "matrix")}}
    \item{coerce}{\code{signature(from = "data.frame", to = "realRatingMatrix")}:
	coercion from a data.frame with three columns.
	Col 1 contains user ids, col 2 contains	item ids and
	col 3 contains ratings.}
    \item{coerce}{\code{signature(from = "realRatingMatrix", to = "data.frame")}: produces user/item/rating triplets.}
    \item{coerce}{\code{signature(from = "realRatingMatrix", to = "dgTMatrix")}}
    \item{coerce}{\code{signature(from = "dgTMatrix", to = "realRatingMatrix")}}
    \item{coerce}{\code{signature(from = "realRatingMatrix", to = "dgCMatrix")}}
    \item{coerce}{\code{signature(from = "dgCMatrix", to = "realRatingMatrix")}}
    \item{coerce}{\code{signature(from = "realRatingMatrix", to = "ngCMatrix")}}

    \item{binarize}{\code{signature(x = "realRatingMatrix")}: create a
        \code{"binaryRatingMatrix"} by setting all ratings larger or equal to
        the argument \code{minRating} as 1 and all others to 0.}
    	\item{getTopNLists}{\code{signature(x = "realRatingMatrix")}: create
	     top-N lists from the ratings in x. Arguments are
	     \code{n} (defaults to 10),
	     \code{randomize} (default is \code{NULL}) and
		   \code{minRating} (default is \code{NA}).
		   Items with a rating below \code{minRating} will not be part of the
		   top-N list. \code{randomize} can be used to get diversity in the
		   predictions by randomly selecting items with a bias to higher rated
		   items. The bias is introduced by choosing the items with a probability
		   proportional to the rating \eqn{(r-min(r)+1)^{randomize}}.
		   The larger the value
		   the more likely it is to get very highly rated items and a negative
		   value for \code{randomize} will select low-rated items. }
    \item{removeKnownRatings}{\code{signature(x = "realRatingMatrix")}: removes
	all ratings in \code{x} for which ratings are available in
	the realRatingMatrix (of same dimensions as \code{x})
	passed as the argument \code{known}. }
    \item{rowSds}{\code{signature(x = "realRatingMatrix")}: calculate
	the standard deviation of ratings for rows (users).}
    \item{colSds}{\code{signature(x = "realRatingMatrix")}: calculate
	the standard deviation of ratings for columns (items).}
	 }
}
\seealso{
	See \code{\linkS4class{ratingMatrix}} inherited methods,
%	\code{\linkS4class{sparseNAMatrix}},
	\code{\linkS4class{binaryRatingMatrix}},
	\code{\linkS4class{topNList}},
	\code{\link{getList}} and \code{\link{getData.frame}}.
	Also see \code{\link[Matrix]{dgCMatrix-class}},
	\code{\link[Matrix]{dgTMatrix-class}} and
	\code{\link[Matrix]{ngCMatrix-class}}
	in \pkg{Matrix}.
}
\examples{
## create a matrix with ratings
m <- matrix(sample(c(NA,0:5),100, replace=TRUE, prob=c(.7,rep(.3/6,6))),
	nrow=10, ncol=10, dimnames = list(
	    user=paste('u', 1:10, sep=''),
	    item=paste('i', 1:10, sep='')
    ))
m

## coerce into a realRatingMAtrix
r <- as(m, "realRatingMatrix")
r

## get some information
dimnames(r)
rowCounts(r) ## number of ratings per user
colCounts(r) ## number of ratings per item
colMeans(r) ## average item rating
nratings(r) ## total number of ratings
hasRating(r) ## user-item combinations with ratings

## histogram of ratings
hist(getRatings(r), breaks="FD")

## inspect a subset
image(r[1:5,1:5])

## coerce it back to see if it worked
as(r, "matrix")

## coerce to data.frame (user/item/rating triplets)
as(r, "data.frame")

## binarize into a binaryRatingMatrix with all 4+ rating a 1
b <- binarize(r, minRating=4)
b
as(b, "matrix")
}
\keyword{classes}
