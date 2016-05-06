\name{evaluationResultList-class}
\Rdversion{1.1}
\docType{class}
\alias{evaluationResultList-class}
\alias{coerce,list,evaluationResultList-method}
\alias{avg,evaluationResultList-method}
\alias{[,evaluationResultList,ANY,missing,missing-method}
%\alias{plot,evaluationResultList-method}
\alias{show,evaluationResultList-method}

\title{Class "evaluationResultList": Results of the Evaluation of a Multiple Recommender Methods }
\description{Contains the evaluation results for several runs using multiple recommender methods in form of confusion matrices. For each run the used models might be avialable.}
\section{Objects from the Class}{
Objects are created by \code{evaluate}.
}
\section{Slots}{
	 \describe{
    \item{\code{.Data}:}{Object of class \code{"list"}: a list of
	\code{"evaluationResults"}.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{list}"}, from data part.
%Class \code{"\linkS4class{vector}"}, by class "list", distance 2.
%Class \code{"\linkS4class{listOrNull}"}, by class "list", distance 2.
}
\section{Methods}{
  \describe{
    \item{avg}{\code{signature(x = "evaluationResultList")}: returns a
	list of average confusion matrices.}
    \item{[}{\code{signature(x = "evaluationResultList", i = "ANY", j = "missing", drop = "missing")}}
%    \item{plot}{\code{signature(x = "evaluationResultList")}: ... }
    \item{coerce}{\code{signature(from = "list", to = "evaluationResultList")}}
	\item{show}{\code{signature(object = "evaluationResultList")}}
	 }
}
\seealso{
	\code{\link{evaluate}},
	\code{\linkS4class{evaluationResults}}.
}
%\examples{
%}
\keyword{classes}
