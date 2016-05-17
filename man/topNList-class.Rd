\name{topNList}
\Rdversion{1.1}
\docType{class}
\alias{topNList-class}
\alias{topNList}
\alias{bestN}
\alias{bestN,topNList-method}
\alias{coerce,topNList,dgTMatrix-method}
\alias{coerce,topNList,dgCMatrix-method}
\alias{coerce,topNList,ngCMatrix-method}
\alias{coerce,topNList,matrix-method}
\alias{coerce,topNList,list-method}
\alias{colCounts,topNList-method}
\alias{rowCounts,topNList-method}
\alias{show,topNList-method}
\alias{length,topNList-method}
\alias{removeKnownItems}
\alias{removeKnownItems,topNList-method}

\alias{getTopNLists}
\alias{getTopNLists,realRatingMatrix-method}


\title{Class "topNList": Top-N List}
\description{Recommendations a Top-N list. }
\section{Objects from the Class}{
Objects can be created by
\code{predict} with a recommender model and new data.
}
\section{Slots}{
    \describe{
	\item{\code{ratings}:}{Object of class \code{"list"}.
		Each element in the list represents a top-N recommendation
		(an integer vector) with item IDs (column numbers in the rating
		matrix). The items are ordered in each vector.}
	\item{\code{items}:}{Object of class \code{"list"} or \code{NULL}.
	  If available, a list of the same structure as \code{items} with the
	  ratings. }
	\item{\code{itemLabels}:}{Object of class \code{"character"}}
	\item{\code{n}:}{Object of class \code{"integer"} specifying the
		number of items in each recommendation.
		Note that the actual number
		on recommended items can be less depending on the data and the
		used algorithm.}
	}
}
\section{Methods}{
    \describe{
	\item{coerce}{\code{signature(from = "topNList", to = "dgTMatrix")}}
	\item{coerce}{\code{signature(from = "topNList", to = "dgCMatrix")}}
	\item{coerce}{\code{signature(from = "topNList", to = "ngCMatrix")}}
	\item{coerce}{\code{signature(from = "topNList", to = "matrix")}}
	\item{coerce}{\code{signature(from = "topNList", to = "list")}}

	\item{bestN}{\code{signature(x = "topNList")}: returns only the best
		n recommendations (second argument is n which defaults to 10). }
	\item{getTopNLists}{\code{signature(x = "realRatingMatrix")}: create
	        top-N lists from the ratings in x. Arguments are \code{n}
		and \code{minRating}. Items with a rating below
		\code{minRating} will not be part of the top-N list.}
	\item{length}{\code{signature(x = "topNList")}: for how many users
	    does this object contain a top-N list? }
	\item{removeKnownItems}{\code{signature(x = "topNList")}:
	    remove items from the top-N list which are known (have a rating)
	    for the user given as a ratingMatrix passed on as argument
		\code{known}. }
	\item{colCounts}{\code{signature(x = "topNList")}: in how many top-N
		does each item occur? }
	\item{rowCounts}{\code{signature(x = "topNList")}: number of recommendations per user. }
	\item{show}{\code{signature(object = "topNList")}}
	}
}
\seealso{
\code{\link{evaluate}}, \code{\linkS4class{realRatingMatrix}}
}
%\examples{
%}
\keyword{classes}
