\name{binaryRatingMatrix}
\Rdversion{1.1}
\docType{class}
\alias{binaryRatingMatrix-class}
\alias{binaryRatingMatrix}
\alias{coerce,matrix,binaryRatingMatrix-method}
\alias{coerce,itemMatrix,binaryRatingMatrix-method}
\alias{coerce,data.frame,binaryRatingMatrix-method}
\alias{coerce,binaryRatingMatrix,matrix-method}
\alias{coerce,binaryRatingMatrix,dgTMatrix-method}
\alias{coerce,binaryRatingMatrix,ngCMatrix-method}
\alias{coerce,binaryRatingMatrix,dgCMatrix-method}
\alias{coerce,binaryRatingMatrix,itemMatrix-method}
\alias{coerce,binaryRatingMatrix,list-method}
%\alias{dissimilarity,binaryRatingMatrix-method}
%\alias{LIST,binaryRatingMatrix-method}

\title{Class "binaryRatingMatrix": A Binary Rating Matrix}
\description{A matrix to represent binary rating data. 1 codes for a positive
rating and 0 codes for either no or a negative rating. This
coding is common for market basked data where products are either bought or
not.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("binaryRatingMatrix", data = im)}, where \code{im} is an \code{itemMatrix} as defined in package
\pkg{arules}, by coercion from a matrix (all non-zero values will be a 1),
or by using \code{binarize} for
an object of class "realRatingMatrix".
}
\section{Slots}{
	 \describe{
    \item{\code{data}:}{Object of class \code{"itemMatrix"} (see package \pkg{arules})}
  }
}
\section{Extends}{
Class \code{"\linkS4class{ratingMatrix}"}, directly.
}
\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "matrix", to = "binaryRatingMatrix")}:
    The matrix needs to be a logical matrix, or a 0-1 matrix (0 means FALSE and 1 means TRUE).
    NAs are interpreted as FALSE.
    }
    \item{coerce}{\code{signature(from = "itemMatrix", to = "binaryRatingMatrix")}}
    \item{coerce}{\code{signature(from = "data.frame", to = "binaryRatingMatrix")}}
    \item{coerce}{\code{signature(from = "binaryRatingMatrix", to = "matrix")}}
    \item{coerce}{\code{signature(from = "binaryRatingMatrix", to = "dgTMatrix")}}
    \item{coerce}{\code{signature(from = "binaryRatingMatrix", to = "ngCMatrix")}}
    \item{coerce}{\code{signature(from = "binaryRatingMatrix", to = "dgCMatrix")}}
    \item{coerce}{\code{signature(from = "binaryRatingMatrix", to = "itemMatrix")}}
    \item{coerce}{\code{signature(from = "binaryRatingMatrix", to = "list")}}
%    \item{dissimilarity}{\code{signature(x = "binaryRatingMatrix")}}
%    \item{LIST}{\code{signature(from = "binaryRatingMatrix")}: ... }
	 }
}
\seealso{
	\code{\link[arules:itemMatrix-class]{itemMatrix}} in \pkg{arules},
	\code{\link{getList}}.
}
\examples{
## create a 0-1 matrix
m <- matrix(sample(c(0,1), 50, replace=TRUE), nrow=5, ncol=10,
    dimnames=list(users=paste("u", 1:5, sep=''),
    items=paste("i", 1:10, sep='')))
m

## coerce it into a binaryRatingMatrix
b <- as(m, "binaryRatingMatrix")
b

## coerce it back to see if it worked
as(b, "matrix")

## use some methods defined in ratingMatrix
dim(b)
dimnames(b)

## counts
rowCounts(b) ## number of ratings per user
colCounts(b) ## number of ratings per item

## plot
image(b)

## sample and subset
sample(b,2)
b[1:2,1:5]

## coercion
as(b, "list")
head(as(b, "data.frame"))
head(getData.frame(b, ratings=FALSE))

## creation from user/item tuples
df <- data.frame(user=c(1,1,2,2,2,3), items=c(1,4,1,2,3,5))
df
b2 <- as(df, "binaryRatingMatrix")
b2
as(b2, "matrix")
}
\keyword{classes}
