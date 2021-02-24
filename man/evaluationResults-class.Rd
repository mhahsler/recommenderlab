\name{evaluationResults-class}
\Rdversion{1.1}
\docType{class}
\alias{evaluationResults-class}
\alias{confusionMatrix-class}
\alias{avg}
\alias{avg,evaluationResults-method}
\alias{getConfusionMatrix}
\alias{getConfusionMatrix,evaluationResults-method}
\alias{getResults}
\alias{getResults,evaluationResults-method}
\alias{getModel}
\alias{getModel,evaluationResults-method}
\alias{getRuns}
\alias{getRuns,evaluationResults-method}
%\alias{plot,evaluationResults-method}
\alias{show,evaluationResults-method}

\title{Class "evaluationResults": Results of the Evaluation of a Single Recommender Method}
\description{Contains the evaluation results for several runs using the same recommender method in form of confusion matrices. For each run the used model might be avialable.}
\section{Objects from the Class}{
Objects are created by \code{evaluate}.
}
\section{Slots}{
	 \describe{
    \item{\code{results}:}{Object of class \code{"list"}: contains
	objects of class \code{"ConfusionMatrix"}, one for each run specified
	in the used evaluation scheme.}
  }
}
\section{Methods}{
  \describe{
    \item{avg}{\code{signature(x = "evaluationResults")}: returns the evaluation metrics averaged of cross-validation folds. }
    \item{getConfusionMatrix}{\code{signature(x = "evaluationResults")}: 
	Deprecated. Use \code{getResults()}.}
    \item{getResults}{\code{signature(x = "evaluationResults")}: 
	returns a list of evaluation metrics with one element for each cross-valudation fold.}
    \item{getModel}{\code{signature(x = "evaluationResults")}: returns a
	list of used recommender models (if avilable). }
    \item{getRuns}{\code{signature(x = "evaluationResults")}: returns
	the number of runs/number of confusion matrices.}
%    \item{plot}{\code{signature(x = "evaluationResults")}: plot }
    \item{show}{\code{signature(object = "evaluationResults")} }
	 }
}
\seealso{
	\code{\link{evaluate}}
}
%\examples{
%}
\keyword{classes}
