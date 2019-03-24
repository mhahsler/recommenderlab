\name{MovieLense}
\Rdversion{1.1}
\alias{MovieLense}
\alias{MovieLenseMeta}
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
from 943 users on 1664 movies. Movie metadata is also provided in \code{MovieLenseMeta}.
}
\usage{data(MovieLense)}
\format{
  The format of \code{MovieLense} is an object of class \code{"realRatingMatrix"}

  The format of \code{MovieLenseMeta} is a data.frame with movie title, year, IMDb URL and indicator variables for 19 genres.
}
%\details{
%}
\source{
GroupLens Research, \url{https://grouplens.org/datasets/movielens/}
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
head(as(MovieLense[1,], "list")[[1]])


## visualize part of the matrix
image(MovieLense[1:100,1:100])

## number of ratings per user
hist(rowCounts(MovieLense))

## number of ratings per movie
hist(colCounts(MovieLense))

## mean rating (averaged over users)
mean(rowMeans(MovieLense))

## available movie meta information
head(MovieLenseMeta)
}
\keyword{datasets}
