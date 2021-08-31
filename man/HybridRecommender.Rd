\name{HybridRecommender}
\Rdversion{1.1}
\alias{HybridRecommender}
\title{
Create a Hybrid Recommender
}
\description{
Creates and combines recommendations using several recommender algorithms.
}
\usage{
HybridRecommender(..., weights = NULL, aggregation_type = "sum")
}
\arguments{
	\item{\dots}{ objects of class 'Recommender'. }
	\item{weights}{ weights for the recommenders. The recommenders are equally
	weighted by default. }
		\item{aggregation_type}{ How are the recommendations aggregated. Options are "sum", "min", and
		"max". }
}
\details{
    The hybrid recommender is initialized with a set of pretrained Recommender objects.
    Typically, the algorithms are trained using the same training set. If different
    training sets are used, then, at least the training sets
    need to have the same items in the same order.

    Alternatively, hybrid recommenders can be created using the regular \code{Recommender()}
    interface. Here \code{method} is set to \code{HYBRID} and \code{parameter} contains
    a list with recommenders and weights. recommenders are a list of recommender alorithms,
    where each algorithms is represented as a list with elements name (method of the recommender)
    and parameters (the algorithms parameters). This method can be used in \code{evaluate()}


    For creating recommendations (\code{predict}), each recommender algorithm
    is used to create ratings. The individual ratings are combined using
    a weighted sum where missing ratings are ignored. Weights can be specified in \code{weights}.
}
\value{
An object of class 'Recommender'.
}
\seealso{
\code{\linkS4class{Recommender}}
}
\examples{
data("MovieLense")
MovieLense100 <- MovieLense[rowCounts(MovieLense) >100,]
train <- MovieLense100[1:100]
test <- MovieLense100[101:103]

## mix popular movies with a random recommendations for diversity and
## rerecommend some movies the user liked.
recom <- HybridRecommender(
  Recommender(train, method = "POPULAR"),
  Recommender(train, method = "RANDOM"),
  Recommender(train, method = "RERECOMMEND"),
  weights = c(.6, .1, .3)
  )

recom

getModel(recom)

as(predict(recom, test), "list")

## create a hybrid recommender using the regular Recommender interface.
## This is needed to use hybrid recommenders with evaluate().
recommenders <- list(
  RANDOM = list(name = "POPULAR", param = NULL),
  POPULAR = list(name = "RANDOM", param = NULL),
  RERECOMMEND = list(name = "RERECOMMEND", param = NULL)
)

weights <- c(.6, .1, .3)

recom <- Recommender(train, method = "HYBRID",
  parameter = list(recommenders = recommenders, weights = weights))
recom

as(predict(recom, test), "list")
}
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
