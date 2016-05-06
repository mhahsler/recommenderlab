\name{evaluationScheme-class}
\Rdversion{1.1}
\docType{class}
\alias{evaluationScheme-class}
%\alias{evaluate,evaluationScheme-method}
\alias{getData}
\alias{getData,evaluationScheme-method}
\alias{show,evaluationScheme-method}

\title{Class "evaluationScheme": Evaluation Scheme}
\description{
	An evaluation scheme created from a data set. The scheme can be a simple split into training and test data, k-fold cross-evaluation or using k
	bootstrap samples.}
\section{Objects from the Class}{
Objects can be created by 
\code{evaluationScheme(data, method="split", train=0.9, k=NULL, given=3).}
}
\section{Slots}{
	 \describe{
    \item{\code{data}:}{Object of class \code{"ratingMatrix"}; the data set. }
    \item{\code{given}:}{Object of class \code{"integer"}; given ratings are 
    randomly selected for each evaluation user and 
    presented to the recommender 
    algorithm to calculate recommend items/ratings. 
    The recommended items are compared 
    to the remaining items for the evaluation user.}
    \item{\code{goodRating}:}{Object of class \code{"numeric"}; Rating at which an item is considered a positive for evaluation. }
    \item{\code{k}:}{Object of class \code{"integer"}; number of runs for evaluation. Default is 1 for method "split" and 10 for "cross-validation" and "bootstrap".}
    \item{\code{knownData}:}{Object of class \code{"ratingMatrix"}; data set with only known (given) items. }
    \item{\code{method}:}{Object of class \code{"character"}; evaluation method. Available methods are: "split", "cross-validation" and "bootstrap".}
    \item{\code{runsTrain}:}{Object of class \code{"list"}; internal repesentation for the split in training and test data for the evaluation runs.}
    \item{\code{train}:}{Object of class \code{"numeric"}; portion of data used for training for "split" and "bootstrap".}
    \item{\code{unknownData}:}{Object of class \code{"ratingMatrix"}; data set with only unknown items. }
  }
}
\section{Methods}{
  \describe{
%    \item{evaluate}{\code{signature(x = "evaluationScheme")}: ... }
    \item{getData}{\code{signature(x = "evaluationScheme")}: access data.
	Parameters are \code{type} ("train", "known" or "unknown") and
	\code{run} (1...k). \code{"train"} returns the training data for 
	the run,
	\code{"known"} returns the known ratings used for prediction 
	for the test data, 
	and \code{"unknown"} returns the ratings used for evaluation 
	for the test data.}
    \item{show}{\code{signature(object = "evaluationScheme")} }
	 }
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
%\note{ ~~further notes~~ }
\seealso{
	\code{\linkS4class{ratingMatrix}} and
	the creator function \code{\link{evaluationScheme}}.
}
%\examples{
%}
\keyword{classes}
