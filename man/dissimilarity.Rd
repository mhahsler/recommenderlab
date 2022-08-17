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
\S4method{dissimilarity}{binaryRatingMatrix}(x, y = NULL, method = NULL, args = NULL, which = "users")
\S4method{dissimilarity}{realRatingMatrix}(x, y = NULL, method = NULL, args = NULL, which = "users")

similarity(x, y = NULL, method = NULL, args = NULL, ...)
\S4method{similarity}{ratingMatrix}(x, y = NULL, method = NULL, args = NULL, which = "users",
  min_matching = 0, min_predictive = 0)
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
  \code{realRatingMatrix}.
  Default for \code{realRatingMatrix} is cosine and for \code{binaryRatingMatrix} is jaccard.}
  \item{args}{a list of additional arguments for the methods.}
  \item{which}{a character string indicating if the (dis)similarity should be
  calculated between \code{"users"} (rows) or \code{"items"} (columns).}
  \item{min_matching, min_predictive}{ Thresholds on the minimum number of ratings used to
  calculate the similarity and the minimum number of ratings that can be used for prediction.}
  \item{...}{further arguments.}
}
\details{
Most dissimlarites and similarities are calculated using the \pkg{proxy} package.
Similarities are typically converted into dissimilarities using \eqn{s = 1 / (1 + d)}
or \eqn{s = 1 - d} (used for Jaccard, Cosine and Pearson correlation) depending on the measure.

Similarities are usually defined in the range of \eqn{[0, 1]}, however,
Cosine similarity and Pearson correlation are defined in the interval \eqn{[-1, 1]}. We rescale these
measures with \eqn{s' = 1 / 2 (s + 1)} to the interval \eqn{[0, 1]}.

Similarities are calculated using only the ratings that are available for both
users/items. This can lead to calculating the measure using only a very small number (maybe only one)
of ratings. \code{min_matching} is the required number of shared ratings to calculate similarities.
To predict ratings, there need to be additional ratings in argument \code{y}.
\code{min_predictive}  is the  required number of additional ratings to calculate similarities. If
\code{min_matching} or \code{min_predictive} fails, then \code{NA} is reported instead of the calculated similarity.
}
\value{
  returns an object of class \code{"dist"}, \code{"simil"}
  or an appropriate object (e.g.,
  a matrix with class \code{"crossdist"} o \code{"crosssimil"}) to represent
  a cross-(dis)similarity.
}
\seealso{
\code{\linkS4class{ratingMatrix}},
\code{\link[arules]{dissimilarity}} in \pkg{arules}, and
\code{\link[proxy]{dist}} in \pkg{proxy}.
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
