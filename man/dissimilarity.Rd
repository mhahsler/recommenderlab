\name{dissimilarity}
\Rdversion{1.1}
\alias{dissimilarity}
\alias{dissimilarity,binaryRatingMatrix-method}
\alias{dissimilarity,realRatingMatrix-method}
\alias{similarity}
\alias{similarity,ratingMatrix-method}
\title{
Dissimilarity and Similarity Calculation Between Rating Data
}
\description{
Calculate dissimilarities/similarities between ratings by users and for items.
}
\usage{
\S4method{dissimilarity}{binaryRatingMatrix}(x, y = NULL, method = NULL, args = NULL, which="users")
\S4method{dissimilarity}{realRatingMatrix}(x, y = NULL, method = NULL, args = NULL, which="users")

similarity(x, y = NULL, method = NULL, args = NULL, ...)
\S4method{similarity}{ratingMatrix}(x, y = NULL, method = NULL, args = NULL, which="users")
}
\arguments{
  \item{x}{a ratingMatrix.}
  \item{y}{\code{NULL} or a second ratingMatrix to calculate 
	cross-(dis)similarities.}
  \item{method}{(dis)similarity measure to use. Available measures
  are typically \code{"cosine"}, \code{"pearson"}, \code{"jaccard"}, etc.
  See \code{dissimilarity} for
  class \code{itemMatrix} in \pkg{arules} for details about measures
  for \code{binaryRatingMatrix} and \code{dist} in \pkg{proxy} for 
      \code{realRatingMatrix}. }
  \item{args}{a list of additional arguments for the methods.}
  \item{which}{a character string indicating if the (dis)similarity should be 
            calculated between \code{"users"} (rows) or \code{"items"} 
	    (columns).}
  \item{...}{further arguments.} 
}
\details{
Similarities are computed from dissimilarities using \eqn{s=1/(1+d)}
or \eqn{s=1-d} depending on the measure. For Pearson we use 1 - positive 
correlation.
}
\value{
  returns an object of class \code{dist}, \code{simil} 
  or an appropriate object (e.g., 
  a matrix) to represent a cross-(dis)similarity.
}
\seealso{
\code{\linkS4class{ratingMatrix}} and
\code{\link[arules]{dissimilarity}} in \pkg{arules}.
}
\examples{
data(MSWeb)

## between 5 users
dissimilarity(MSWeb[1:5,], method = "jaccard")
similarity(MSWeb[1:5,], method = "jaccard")

## between first 3 items
dissimilarity(MSWeb[,1:3], method = "jaccard", which = "items")
similarity(MSWeb[,1:3], method = "jaccard", which = "items")

## cross-similarity between first 2 users and users 10-20
similarity(MSWeb[1:2,], MSWeb[10:20,], method="jaccard")
}
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
