\name{sparseNAMatrix-class}
\Rdversion{1.1}
\alias{dropNA}
\alias{dropNA2matrix}
\alias{sparseNAMatrix-class}
\alias{sparseNAMatrix}
\title{
  Class ``sparseNAMatrix'' --- Sparse Matrix Representation With NAs Not Explicitly Stored
}
\description{
Class to represent matrices with dropped NAs. Coerce from and to a
sparse matrix representation where \code{NA}s are not explicitly stored.
}
\section{Objects from the Class}{
  Objects are created by coercion from a matrix by
  calls of the form \code{new("sparseNAMatrix", ...)} or using \code{dropNA()} (see Usage section).
 }
\section{Slots}{ none }
\usage{
dropNA(x)
dropNA2matrix(x)
}
\arguments{
  \item{x}{ a matrix (for \code{dropNA()}) or a dgCMatrix
    (for \code{dropNA2matrix()})}
}
\section{Extends}{
  Class \code{\linkS4class{dgCMatrix}}, directly.
}
\details{
The representation is based on
dgCMatrix in \pkg{Matrix} but instead of zeros, \code{NA}s are dropped.
Be careful when working with the dgCMatrix directly since all
dropped values are NA and not 0!
}
\value{
Returns a sparseNAMatrix (subclass of dgCMatrix) or a matrix.
}
\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "matrix", to = "sparseNAMatrix")}}
    \item{coerce}{\code{signature(from = "sparseNAMatrix", to = "matrix")}}
}}

\seealso{
    \code{\link[Matrix:dgCMatrix-class]{dgCMatrix}} in \pkg{Matrix}.
}
\examples{
m <- matrix(sample(c(NA,0:5),50, replace=TRUE, prob=c(.5,rep(.5/6,6))),
    nrow=5, ncol=10, dimnames = list(users=paste('u', 1:5, sep=''),
    items=paste('i', 1:10, sep='')))
m

## drop all NAs in the representation
sparse <- as(m, "sparseNAMatrix") ## or dropNA(m)
sparse

## convert back to matrix
as(sparse, "matrix") ## or dropNA2matrix(sparse)

## Using regular coercion to dgCMatrix. Note that regular coercion
## to dgCMatrix drops 0s and not NAs!
as(m, "dgCMatrix")
}
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
