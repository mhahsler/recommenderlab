\name{internalFunctions}
\alias{returnRatings}
\alias{getParameters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Internal Utility Functions}
\description{
Utility functions used internally by recommender algorithms. See files starting
with \code{RECOM} in the package's \code{R} directory for examples of usage.
}
\usage{
returnRatings(ratings, newdata,
  type = c("topNList", "ratings", "ratingMatrix"),
  n, randomize = NULL, minRating = NA)

getParameters(defaults, parameter)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ratings}{}
  \item{newdata}{}
  \item{type}{}
  \item{n}{}
  \item{randomize}{}
  \item{minRating}{}

  \item{defaults}{}
  \item{parameter}{}
}
%\details{}
%\value{}
%\examples{}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
