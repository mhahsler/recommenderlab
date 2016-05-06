\name{Recommender-class}
\Rdversion{1.1}
\docType{class}
\alias{Recommender-class}
\alias{getModel,Recommender-method}
%\alias{predict,Recommender-method}
\alias{show,Recommender-method}

\title{Class "Recommender": A Recommender Model}
\description{Represents a recommender model learned for a given data set 
(a rating matrix).}
\section{Objects from the Class}{
Objects are created by
the creator function \code{Recommender(data, method, parameter = NULL)} 
}
\section{Slots}{
	 \describe{
    \item{\code{method}:}{Object of class \code{"character"}; 
	used recommendation method.}
    \item{\code{dataType}:}{Object of class \code{"character"}; 
	concrete class of the input data.}
    \item{\code{ntrain}:}{Object of class \code{"integer"}; 
	size of training set. }
    \item{\code{model}:}{Object of class \code{"list"}; the model. }
    \item{\code{predict}:}{Object of class \code{"function"}; code to compute
	a recommendation using the model. }
  }
}
\section{Methods}{
  \describe{
    \item{getModel}{\code{signature(x = "Recommender")}: retrieve the model. }
    \item{predict}{\code{signature(object = "Recommender")}: create
	recommendations for new data (argument \code{newdata}). }
    \item{show}{\code{signature(object = "Recommender")} }
	 }
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
%\note{ ~~further notes~~ }
\seealso{
	See \code{\link{Recommender}} for the constructor function and 
	a description of availble methods.
}
%\examples{
%}
\keyword{classes}
