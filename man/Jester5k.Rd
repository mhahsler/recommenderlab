\name{Jester5k}
\alias{Jester5k}
\alias{JesterJokes}
\docType{data}
\title{Jester dataset (5k sample)}
\description{
The data set contains a sample of 5000 users from the anonymous
ratings data from
the Jester Online Joke Recommender System collected between
April 1999 and May 2003.
}
\usage{data(Jester5k)}
\format{
  The format of \code{Jester5k} is: Formal class 'realRatingMatrix' [package "recommenderlab"]

  The format of \code{JesterJokes} is: vector of character strings.
}
\details{
\code{Jester5k} contains a 5000 x 100 rating matrix (5000 users and 100 jokes)
with ratings between -10.00 and +10.00. All selected users have
rated 36 or more jokes.

The data also contains the actual jokes in \code{JesterJokes}.
}
%\source{
%}
\references{
Ken Goldberg, Theresa Roeder, Dhruv Gupta, and  Chris Perkins.
"Eigentaste: A Constant Time Collaborative Filtering Algorithm."
Information Retrieval, 4(2), 133-151. July 2001.
}
\examples{
data(Jester5k)
Jester5k

## number of ratings
nratings(Jester5k)

## number of ratings per user
summary(rowCounts(Jester5k))

## rating distribution
hist(getRatings(Jester5k), main="Distribution of ratings")

## 'best' joke with highest average rating
best <- which.max(colMeans(Jester5k))
cat(JesterJokes[best])
}
\keyword{datasets}
