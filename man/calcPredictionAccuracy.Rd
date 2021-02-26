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
  various binary classification metrics are returned (e.g., precision, recall, TPR, FPR).
}
\usage{
calcPredictionAccuracy(x, data, ...)
\S4method{calcPredictionAccuracy}{realRatingMatrix,realRatingMatrix}(x, data, byUser=FALSE,...)
\S4method{calcPredictionAccuracy}{topNList,realRatingMatrix}(x, data, byUser=FALSE, given=NULL, goodRating=NA,...)
\S4method{calcPredictionAccuracy}{topNList,binaryRatingMatrix}(x, data, byUser=FALSE, given=NULL,...)
}
\arguments{
  \item{x}{ Predicted items in a "topNList" or predicted ratings as a "realRatingMatrix"}
  \item{data}{ Observed true ratings for the users as a "RatingMatrix". The users have to be in the same order as in \code{x}. }
  \item{byUser}{ logical; Should the accuracy measures be reported for each user individually instead of being averaged over all users?}
  \item{given}{ how many items were given to create the predictions. }
  \item{goodRating}{ If \code{x} is a "topNList" and \code{data} is a "realRatingMatrix" then \code{goodRating} is used as the threshold for determining what rating in \code{data} is considered a good rating.}
  \item{...}{ further arguments.}
}
\details{
The function calculates the accuracy of predictions compared to the observed true ratings (\code{data}) averaged over the users. Use \code{byUser = TRUE} to get the results for each user.

If both, the predictions are numeric ratings (i.e. a "realRatingMatrix"),
then the error measures RMSE, MSE and MAE are calculated.

If the predictions are a "topNList", then the entries of the confusion matrix (true positives TP, false positives FP, false negatives FN and true negatives TN) and binary classification measures like precision, recall, TPR and FPR are calculated. If data is a "realRatingMatrix", then
\code{goodRating} has to be specified to identify items that should be recommended (i.e., have a rating of goodRating or more).
Note that you need to specify the number of items given to the recommender to create predictions.
The number of predictions by user (N) is the total number of items in the data minus the number of given items. The number of TP is limited by the size of the top-N list. Also, since the counts for TP, FP, FN and TN are averaged over the users (unless \code{byUser = TRUE} is used),
they will not be whole numbers.

If the ratings are a "topNList" and the observed data is a "realRatingMatrix" then \code{goodRating} is used
to determine what rating in \code{data} is considered a good rating for calculating binary classification measures. This means that an item in the topNList is considered a true positive if it has a rating of \code{goodRating} or better in the observed data.
}
\value{
Returns a vector with the appropriate measures averaged over all users.
For \code{byUser=TRUE}, a matrix with a row for each user is returned.
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
### recommender for real-valued ratings
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
calcPredictionAccuracy(p, getData(e, "unknown"), given=3, byUser = TRUE)
}
%\keyword{ ~kwd1 }
