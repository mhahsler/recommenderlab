\name{evaluate}
\Rdversion{1.1}
\alias{evaluate}
\alias{evaluate,evaluationScheme,character-method}
\alias{evaluate,evaluationScheme,list-method}
\title{
Evaluate a Recommender Models
}
\description{
Evaluates a single or a list of recommender model given an evaluation scheme and return evaluation metrics.
}
\usage{
evaluate(x, method, ...)

\S4method{evaluate}{evaluationScheme,character}(x, method, type="topNList",
  n=1:10, parameter=NULL, progress = TRUE, keepModel=FALSE)
\S4method{evaluate}{evaluationScheme,list}(x, method, type="topNList",
  n=1:10, parameter=NULL, progress = TRUE, keepModel=FALSE)
}
\arguments{
  \item{x}{an evaluation scheme (class \code{"evaluationScheme"}).}
  \item{method}{a character string or a list. If
  a single character string is given it defines the recommender method
  used for evaluation. If several recommender methods need to be compared,
  \code{method} contains a nested list. Each element describes a recommender
  method and consists of a list with two elements: a character string
  named \code{"name"} containing the method and a list named
  \code{"parameters"} containing the parameters used for this recommender method.
  See \code{Recommender} for available methods.}
  \item{type}{evaluate "topNList" or "ratings"?}
  \item{n}{a vector of the different values for N used to generate top-N lists (only if type="topNList").}
  \item{parameter}{a list with parameters for the recommender algorithm (only
    used when \code{method} is a single method).}
  \item{progress}{logical; report progress?}
  \item{keepModel}{logical; store used recommender models?}
  \item{\dots}{further arguments. }
}
\details{
The evaluation uses the specification in the evaluation scheme to train a recommender models on training data and then evaluates the models on test data.
The result is a set of accuracy measures averaged over the test users.
See \code{\link{calcPredictionAccuracy}} for details on the accuracy measures and the averaging.
Note: Also the confusion matrix counts are averaged over users and therefore not whole numbers.

See \code{vignette("recommenderlab")} for more details on the evaluaiton process and the used metrics.
}
\value{
If a single recommender method is specified in  \code{method}, then an
object of class \code{"evaluationResults"} is returned.
If \code{method} is a list of recommendation models, then an object of class \code{"evaluationResultList"} is returned.
}
\seealso{
\code{\link{calcPredictionAccuracy}},
\code{\linkS4class{evaluationScheme}},
\code{\linkS4class{evaluationResults}}.
\code{\linkS4class{evaluationResultList}}.
}
\examples{
### evaluate top-N list recommendations on a 0-1 data set
## Note: we sample only 100 users to make the example run faster
data("MSWeb")
MSWeb10 <- sample(MSWeb[rowCounts(MSWeb) >10,], 100)

## create an evaluation scheme (10-fold cross validation, given-3 scheme)
es <- evaluationScheme(MSWeb10, method="cross-validation",
        k=10, given=3)

## run evaluation
ev <- evaluate(es, "POPULAR", n=c(1,3,5,10))
ev

## look at the results (the length of the topNList is shown as column n)
getResults(ev)

## get a confusion matrices averaged over the 10 folds
avg(ev)
plot(ev, annotate = TRUE)

## evaluate several algorithms (including a hybrid recommender) with a list
algorithms <- list(
  RANDOM = list(name = "RANDOM", param = NULL),
  POPULAR = list(name = "POPULAR", param = NULL),
  HYBRID = list(name = "HYBRID", param =
      list(recommenders = list(
          RANDOM = list(name = "RANDOM", param = NULL),
          POPULAR = list(name = "POPULAR", param = NULL)
        )
      )
  )
)

evlist <- evaluate(es, algorithms, n=c(1,3,5,10))
evlist
names(evlist)

## select the first results by index
evlist[[1]]
avg(evlist[[1]])

plot(evlist, legend="topright")

### Evaluate using a data set with real-valued ratings
## Note: we sample only 100 users to make the example run faster
data("Jester5k")
es <- evaluationScheme(Jester5k[1:100], method="split",
  train=.9, given=10, goodRating=5)
## Note: goodRating is used to determine positive ratings

## predict top-N recommendation lists
## (results in TPR/FPR and precision/recall)
ev <- evaluate(es, "RANDOM", type="topNList", n=10)
getResults(ev)

## predict missing ratings
## (results in RMSE, MSE and MAE)
ev <- evaluate(es, "RANDOM", type="ratings")
getResults(ev)
}
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
