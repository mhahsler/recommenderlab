\name{dropNA}
\Rdversion{1.1}
\alias{dropNA}
\alias{dropNA2matrix}
\title{
  Sparse Matrix Representation With NAs Not Explicitly Stored 
}
\description{
Coerce from and to a sparse matrix representation where
\code{NA}s are not explicitly stored. 
}
\usage{
dropNA(x)
dropNA2matrix(x)
}
\arguments{
  \item{x}{ a matrix (for \code{dropNA()}) or a dgCMatrix 
    (for \code{dropNA2matrix()})}
}
\details{
The representation is based on
dgCMatrix in \pkg{Matrix} but instead of zeros, \code{NA}s are dropped.
Be careful when working with the dgCMatrix directly since all
dropped values are NA and not 0! 
}
\value{
Returns a dgCMatrix or a matrix.
}
\seealso{
    \code{\link[Matrix:dgCMatrix-class]{dgCMatrix}} in \pkg{Matrix}.
}
\examples{
m <- matrix(sample(c(NA,0:5),50, replace=TRUE, prob=c(.5,rep(.5/6,6))),
    nrow=5, ncol=10, dimnames = list(users=paste('u', 1:5, sep=''),
    items=paste('i', 1:10, sep='')))

## drop all NAs in the representation
sparse <- dropNA(m)
sparse

## convert back to matrix
dropNA2matrix(sparse)

## Using regular coercion to dgCMatrix. Note that regular coercion 
## to dgCMatrix drops 0s and not NAs!
as(m, "dgCMatrix")
}
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
