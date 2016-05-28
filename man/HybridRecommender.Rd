\name{HybridRecommender}
\Rdversion{1.1}
\alias{HybridRecommender}
\title{
Create a Hybrid Recommender
}
\description{
Combines several trained recommender algorithms into a hybrid recommender.
}
\usage{
HybridRecommender(..., weights = NULL)
}
\arguments{
	\item{\dots}{ objects of class 'Recommender'. }
	\item{weights}{ weights for the recommenders. The recommenders are equally
	weighted by default. }
}
%\details{
%}
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
}
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
