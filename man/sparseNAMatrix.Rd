\name{sparseNAMatrix-class}
\Rdversion{1.1}
\alias{dropNA}
\alias{dropNA2matrix}
%\alias{sparseNAMatrix-class}
%\alias{sparseNAMatrix}
%\alias{[,sparseNAMatrix,index,index,missing-method}
%\alias{[,sparseNAMatrix,index,missing,missing-method}
%\alias{[,sparseNAMatrix,missing,index,missing-method}
%\alias{[,sparseNAMatrix,index,index,logical-method}
%\alias{[,sparseNAMatrix,index,missing,logical-method}
%\alias{[,sparseNAMatrix,missing,index,logical-method}
%\alias{[,sparseNAMatrix,missing,index,logical-method}
%\alias{[<-,sparseNAMatrix,index,index,numeric-method}
%\alias{[<-,sparseNAMatrix,index,missing,numeric-method}
%\alias{[<-,sparseNAMatrix,missing,index,numeric-method}
%\alias{[<-,sparseNAMatrix,missing,missing,numeric-method}
%\alias{coerce,matrix,sparseNAMatrix-method}
%\alias{coerce,sparseNAMatrix,matrix-method}
% Note: coercions from and to dgCMatrix is implicit
\title{
%  Class ``sparseNAMatrix'' --- Sparse Matrix Representation With NAs Not Explicitly Stored
Sparse Matrix Representation With NAs Not Explicitly Stored
}
\description{
%Class to represent matrices with dropped NAs.
Coerce from and to a
sparse matrix representation where \code{NA}s are not explicitly stored.
}
%\section{Objects from the Class}{
%   Objects are created by coercion from a matrix by
%   calls of the form \code{new("sparseNAMatrix", ...)} or using \code{dropNA()} (see Usage section).
%  }
% \section{Slots}{ none }
\usage{
dropNA(x)
dropNA2matrix(x)
}
\arguments{
  \item{x}{ a matrix (for \code{dropNA()}). A sparse matrix
    (for \code{dropNA2matrix()})}
}
%\section{Extends}{
%  Class \code{\linkS4class{dgCMatrix}}, directly.
%}
\details{
The representation is based on
the sparse dgCMatrix in \pkg{Matrix} but instead of zeros, \code{NA}s are dropped.

\bold{Note:} Be careful when working with matrix operations
(multiplication, addition, etc.) since these will use the dgCMatrix superclass
which assumes that all dropped values are zero and not NA!
This means that the operations might remove zeros or add to NAs incorrectly.
%The result will always be a dgCMatrix and not a sparseNAMatrix.
}
\value{
%Returns a sparseNAMatrix (subclass of dgCMatrix) or a matrix.
Returns a dgCMatrix or a matrix, respectively.
}
%\section{Methods}{
%  \describe{
%    \item{coerce}{\code{signature(from = "matrix", to = "sparseNAMatrix")}}
%    \item{coerce}{\code{signature(from = "sparseNAMatrix", to = "matrix")}}
%    \item{coerce}{\code{signature(from = "dgCMatrix", to = "sparseNAMatrix")}}
%    \item{coerce}{\code{signature(from = "sparseNAMatrix", to = "dgCMatrix")}}
%    \item{[}{\code{signature(x = "sparseNAMatrix")}}
%}}

\seealso{
    \code{\link[Matrix:dgCMatrix-class]{dgCMatrix}} in \pkg{Matrix}.
}
\examples{
m <- matrix(sample(c(NA,0:5),50, replace=TRUE, prob=c(.5,rep(.5/6,6))),
    nrow=5, ncol=10, dimnames = list(users=paste('u', 1:5, sep=''),
    items=paste('i', 1:10, sep='')))
m

## drop all NAs in the representation
sparse <- dropNA(m)
sparse

## convert back to matrix
dropNA2matrix(sparse)
}
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
