\name{MSWeb}
\Rdversion{1.1}
\alias{MSWeb}
\docType{data}
\title{
Anonymous web data from www.microsoft.com
}
\description{
Vroots visited by users in a one week timeframe.
}
\usage{data(MSWeb)}
\format{
  The format is: Formal class \code{"binaryRatingMatrix"}.
}
\details{
The data was created by sampling and processing the www.microsoft.com logs.
The data records the use of www.microsoft.com by 38000 anonymous,
randomly-selected users. For each user, the data lists all the areas of the web
site (Vroots) that user visited in a one week timeframe in February 1998.

This dataset contains 32710 valid users and 285 Vroots.


}
\source{
Asuncion, A., Newman, D.J. (2007). UCI Machine Learning Repository, Irvine, CA:
University of California, School of Information and Computer Science.
\url{http://www.ics.uci.edu/~mlearn/MLRepository.html}
}
\references{
J. Breese, D. Heckerman., C. Kadie (1998). Empirical Analysis of Predictive
Algorithms for Collaborative Filtering, Proceedings of the Fourteenth
Conference on Uncertainty in Artificial Intelligence, Madison, WI.
}
\examples{
data(MSWeb)
MSWeb

nratings(MSWeb)

## look at first two users
as(MSWeb[1:2,], "list")

## items per user
hist(rowCounts(MSWeb), main="Distribution of Vroots visited per user")
}
\keyword{datasets}
