\name{MovieLense}
\Rdversion{1.1}
\alias{MovieLense}
\docType{data}
\title{
MovieLense Dataset (100k)
}
\description{
The 100k MovieLense
ratings data set. The data was collected through the MovieLens web site
(movielens.umn.edu) during the seven-month period from September 19th,
1997 through April 22nd, 1998.
The data set contains about 100,000 ratings (1-5)
from 943 users on 1664 movies.
}
\usage{data(MovieLense)}
\format{
  The format is an object of class \code{"realRatingMatrix"}
}
%\details{
%}
\source{
GroupLens Research, \url{http://www.grouplens.org/node/73}
}
\references{
Herlocker, J., Konstan, J., Borchers, A., Riedl, J.. An Algorithmic
Framework for Performing Collaborative Filtering. Proceedings of the
1999 Conference on Research and Development in Information
Retrieval. Aug. 1999.
}
\examples{
data(MovieLense)
MovieLense

## look at the first few ratings of the first user
as(MovieLense[1,], "list")[[1]][1:10]


## visualize part of the matrix
image(MovieLense[1:100,1:100])

## number of ratings per user
hist(rowCounts(MovieLense))

## number of ratings per movie
hist(colCounts(MovieLense))

## mean rating (averaged over users)
mean(rowMeans(MovieLense))
}
\keyword{datasets}
