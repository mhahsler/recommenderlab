\name{ratingMatrix}
\Rdversion{1.1}
\docType{class}
\alias{ratingMatrix-class}
\alias{ratingMatrix}
\alias{coerce,ratingMatrix,list-method}
\alias{coerce,ratingMatrix,data.frame-method}
\alias{[,ratingMatrix,ANY,ANY,ANY-method}
\alias{sample,ratingMatrix-method}
\alias{colCounts}
\alias{colCounts,ratingMatrix-method}
\alias{rowCounts}
\alias{rowCounts,ratingMatrix-method}
\alias{colMeans,ratingMatrix-method}
\alias{rowMeans,ratingMatrix-method}
\alias{dim,ratingMatrix-method}
\alias{dimnames<-,ratingMatrix,list-method}
\alias{dimnames,ratingMatrix-method}
\alias{nratings}
\alias{nratings,ratingMatrix-method}
\alias{show,ratingMatrix-method}
\alias{image,ratingMatrix-method}
\alias{getNormalize}
\alias{getNormalize,ratingMatrix-method}
\alias{getRatings}
\alias{getRatings,ratingMatrix-method}
\alias{hasRating}
\alias{hasRating,ratingMatrix-method}
\alias{getRatingMatrix}
\alias{getRatingMatrix,ratingMatrix-method}

\title{Class "ratingMatrix": Virtual Class for Rating Data}
\description{Defines a common class for rating data.}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Methods}{
	\describe{
		\item{[}{\code{signature(x = "ratingMatrix", i = "ANY", j = "ANY", drop = "ANY")}: subset the rating matrix (\code{drop} is ignorred). }
		\item{coerce}{\code{signature(from = "ratingMatrix", to = "list")}}
		\item{coerce}{\code{signature(from = "ratingMatrix", to = "data.frame")}: a data.frame with three columns. Col 1 contains user ids, col 2 contains        item ids and col 3 contains ratings.}
		\item{colCounts}{\code{signature(x = "ratingMatrix")}:  number of ratings per column.}
		\item{rowCounts}{\code{signature(x = "ratingMatrix")}:  number of ratings per row.}
		\item{colMeans}{\code{signature(x = "ratingMatrix")}: column-wise rating means. }
		\item{rowMeans}{\code{signature(x = "ratingMatrix")}: row-wise rating means. }
		\item{dim}{\code{signature(x = "ratingMatrix")}: dimensions of the rating matrix. }
		\item{dimnames<-}{\code{signature(x = "ratingMatrix", value = "list")}: replace dimnames. }
		\item{dimnames}{\code{signature(x = "ratingMatrix")}: retrieve dimnames. }
		\item{getNormalize}{\code{signature(x = "ratingMatrix")}: returns a list with normalization information for the matrix (NULL if data is not normalized). }
    \item{getRatings}{\code{signature(x = "ratingMatrix")}: returns all
	ratings in \code{x} as a numeric vector. }
    \item{getRatingMatrix}{\code{signature(x = "ratingMatrix")}: returns the ratings as a sparse matrix. The format is different for binary and real rating matices.}
    \item{hasRating}{\code{signature(x = "ratingMatrix")}: returns a sparse logical matrix with TRUE for user-item combinations which have a rating. }
		\item{image}{\code{signature(x = "ratingMatrix")}: plot the matrix. }
		\item{nratings}{\code{signature(x = "ratingMatrix")}: number of ratings in the matrix. }
		\item{sample}{\code{signature(x = "ratingMatrix")}: sample from users (rows). }
		\item{show}{\code{signature(object = "ratingMatrix")} }

}
}
\seealso{
	See implementing classes
	\code{\linkS4class{realRatingMatrix}}
	and
	\code{\linkS4class{binaryRatingMatrix}}.
	See \code{\link{getList}},
	\code{\link{getData.frame}},
	\code{\link{similarity}},
	\code{\link{dissimilarity}} and
	\code{\link{dissimilarity}}.
}
\keyword{classes}
