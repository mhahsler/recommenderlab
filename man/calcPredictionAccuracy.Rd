\name{calcPredictionAccuracy}
\Rdversion{1.1}
\alias{calcPredictionAccuracy}
\alias{calcPredictionAccuracy,realRatingMatrix,realRatingMatrix-method}
\alias{calcPredictionAccuracy,topNList,binaryRatingMatrix-method}
\alias{calcPredictionAccuracy,topNList,realRatingMatrix-method}
\title{
  Calculate the Prediction Error for a Recommendation
}
\description{
  Calculate prediction accuracy. For predicted ratings
  MAE (mean average error), MSE (means squared error) 
  and RMSE (root means squared error) are calculated. For topNLists
  various binary classification metrics are returned.
}
\usage{
calcPredictionAccuracy(x, data, ...)
\S4method{calcPredictionAccuracy}{realRatingMatrix,realRatingMatrix}(x, data, byUser=FALSE,...)
\S4method{calcPredictionAccuracy}{topNList,realRatingMatrix}(x, data, byUser=FALSE, given=NULL, goodRating=NA,...)
\S4method{calcPredictionAccuracy}{topNList,binaryRatingMatrix}(x, data, byUser=FALSE, given=NULL,...)
}
\arguments{
  \item{x}{ Predicted items in a "topNList" or predicted ratings as a "realRatingMatrix"}
  \item{data}{ Actual ratings by the user as a "RatingMatrix" }
  \item{byUser}{ logical; Should the errors be averaged by user or over all
  recommendations?}
  \item{given}{ how many items were given to create the predictions. }
  \item{goodRating}{ threshold for determining what rating is a good rating. Used only if
  x is a topNList and data is a "realRatingMatrix".}
  \item{...}{ further arguments.}
}
%\details{
%}
\value{
Returns a vector with the average measures for \code{byUser=TRUE}. 
Otherwise, a matrix with a row for each user is returned. 
}
\seealso{
\code{\linkS4class{topNList}},
\code{\linkS4class{binaryRatingMatrix}},
\code{\linkS4class{realRatingMatrix}}.
}
\references{
Asela Gunawardana and Guy Shani (2009). A Survey of Accuracy Evaluation Metrics of 
Recommendation Tasks, Journal of Machine Learning Research 10, 2935-2962.
}
\examples{
### real valued recommender  
data(Jester5k)

## create 90/10 split (known/unknown) for the first 500 users in Jester5k
e <- evaluationScheme(Jester5k[1:500,], method="split", train=0.9, 
    k=1, given=15)
e

## create a user-based CF recommender using training data
r <- Recommender(getData(e, "train"), "UBCF")

## create predictions for the test data using known ratings (see given above)
p <- predict(r, getData(e, "known"), type="ratings")
p

## compute error metrics averaged per user and then averaged over all 
## recommendations
calcPredictionAccuracy(p, getData(e, "unknown"))
head(calcPredictionAccuracy(p, getData(e, "unknown"), byUser=TRUE))

## evaluate topNLists instead (you need to specify given and goodRating!)
p <- predict(r, getData(e, "known"), type="topNList")
p
calcPredictionAccuracy(p, getData(e, "unknown"), given=15, goodRating=5) 
  
## evaluate a binary recommender
data(MSWeb)
MSWeb10 <- sample(MSWeb[rowCounts(MSWeb) >10,], 50)

e <- evaluationScheme(MSWeb10, method="split", train=0.9, 
    k=1, given=3)
e
  
## create a user-based CF recommender using training data
r <- Recommender(getData(e, "train"), "UBCF")
  
## create predictions for the test data using known ratings (see given above)
p <- predict(r, getData(e, "known"), type="topNList", n=10)
p

calcPredictionAccuracy(p, getData(e, "unknown"), given=3)
}
%\keyword{ ~kwd1 }
