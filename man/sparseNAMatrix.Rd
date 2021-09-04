\name{sparseNAMatrix-class}
\Rdversion{1.1}
\alias{dropNA}
\alias{dropNA2matrix}
\alias{dropNAis.na}
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
dropNAis.na(x)
}
\arguments{
  \item{x}{ a matrix for \code{dropNA()}, or a sparse matrix with dropped NA values
    for \code{dropNA2matrix()} or \code{dropNAis.na()}.}
}
%\section{Extends}{
%  Class \code{\linkS4class{dgCMatrix}}, directly.
%}
\details{
The representation is based on
the sparse dgCMatrix in \pkg{Matrix} but instead of zeros, \code{NA}s are dropped.
This is achieved by the following:

\itemize{
\item Zeros are represented with a very small value (\code{.Machine$double.xmin})
so they do not get dropped in the sparse representation.
\item NAs are converted to 0 before using \code{drop0()} to make them not explicitly stored.
}

\bold{Caution:} Be careful when working with the sparse matrix and sparse matrix operations
(multiplication, addition, etc.) directly.
\itemize{
\item Sparse matrix operations will see 0 where NAs should be.
\item Zeros are have a small, but non-zero value (\code{.Machine$double.xmin}).
\item Sparse matrix operations that can result in a true 0
   need to be followed by replacing the 0 with \code{.Machine$double.xmin} or other operations
   (like subsetting) will drop the 0.
}

\code{dropNAis.na()} correctly finds NA values in a sparse matrix with dropped NA values, while \code{is.na()} does not work.

\code{dropNA2matrix()} converts the sparse representation into a dense matrix. NAs represented by
dropped values are converted to true NAs. Zeros are recovered by using \code{zapsmall} which replaces
small values by 0.
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

## drop all NAs in the representation. Zeros are represented by very small values.
sparse <- dropNA(m)
sparse

## convert back to matrix
dropNA2matrix(sparse)

## Note: be careful with the sparse representation!
## Do not use is.na, but use
dropNAis.na(sparse)
}
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
