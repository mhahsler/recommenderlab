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
  \item{ratings}{ a realRatingMatrix. }
  \item{newdata}{ a realRatingMatrix. }
  \item{type}{ type of recommendation to return. }
  \item{n}{ max. number of entries in the top-N list. }
  \item{randomize}{ randomization factor for producing the top-N list. }
  \item{minRating}{ do not include ratings less than this. }

  \item{defaults}{ list with parameters and default values. }
  \item{parameter}{ list with actual parameters. }
}
\details{
\code{returnRatings} is used in the predict function of recommender algorithms
to return different types of recommendations.

\code{getParameters} is a helper function which checks parameters for
consistency and provides default values. Used in the Recommender constructor.
}
%\value{}
%\examples{}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
