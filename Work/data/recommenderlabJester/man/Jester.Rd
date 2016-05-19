\name{Jester}
\alias{Jester}
\alias{Jester_text}
\docType{data}
\title{Jester Dataset}
\description{
The data set contains the anonymous 
ratings data from
the Jester Online Joke Recommender System collected between 
April 1999 and May 2003. 
}
\usage{data(Jester)}
\format{
  The format is: Formal class 'realRatingMatrix' [package "recommenderlab"] 
}
\details{
24983 x 100 rating matrix (24983 users and 100 jokes)
with 1,810,455 ratings between -10.00 and +10.00. All selected users have
rated 15 or more jokes.

The text for the jokes are also available as a character vector of length 100
in \code{JesterJokes}.
}
%\source{
%}
\references{
Ken Goldberg, Theresa Roeder, Dhruv Gupta, and  Chris Perkins.
"Eigentaste: A Constant Time Collaborative Filtering Algorithm."
Information Retrieval, 4(2), 133-151. July 2001.
}
\examples{
data(Jester)
Jester

hist(getRatings(Jester), main="Distribution of ratings")

# what is the best joke? (highest average rating)
best <- which.max(colMeans(Jester))
cat(JesterJokes[best])
}
\keyword{datasets}
