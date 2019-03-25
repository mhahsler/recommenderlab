\name{predict}
\Rdversion{1.1}
\alias{predict}
\alias{predict,Recommender-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predict Recommendations
}
\description{
Creates recommendations using a recommender model and data about new users.
}
\usage{
\S4method{predict}{Recommender}(object, newdata, n = 10, data=NULL,
    type="topNList", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{a recommender model (class \code{"Recommender"}).}
  \item{newdata}{data for active users (class \code{"ratingMatrix"})
   or the index of users in the training data to create recommendations for.
   If an index is used then some recommender algorithms need to be passed
   the training data as argument \code{data}. Some algorithms may only support
   user indices. }
  \item{n}{ number of recommendations in the top-N list.}
  \item{data}{ training data needed by some recommender algorithms if
    \code{newdata} is a user index and not user data. }
  \item{type}{ type of recommendation. The default type is
  \code{"topNList"} which creates
  a top-N recommendation list with recommendations.
  Some recommenders can also predict ratings with
  type \code{"ratings"} which returns only predicted ratings with
  known ratings represented by \code{NA},
  or type \code{"ratingMatrix"} which returns a completed rating matrix (Note that
  the predicted ratings may differ from the known ratings).}
  \item{\dots}{further arguments.}
}
%\details{
%}
\value{
Returns an object of class \code{"topNList"} or of other appropriate
classes.
}
\seealso{
\code{\linkS4class{Recommender}},
\code{\linkS4class{ratingMatrix}}.
}
\examples{
data("MovieLense")
MovieLense100 <- MovieLense[rowCounts(MovieLense) >100,]
train <- MovieLense100[1:50]

rec <- Recommender(train, method = "POPULAR")
rec

## create top-N recommendations for new users
pre <- predict(rec, MovieLense100[101:102], n = 10)
pre
as(pre, "list")

## predict ratings for new users
pre <- predict(rec, MovieLense100[101:102], type="ratings")
pre
as(pre, "matrix")[,1:10]


## create recommendations using user ids with ids 1..10 in the
## training data
pre <- predict(rec, 1:10 , data = train, n = 10)
pre
as(pre, "list")
}
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
