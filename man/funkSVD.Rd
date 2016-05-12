\name{funkSVD}
\alias{funkSVD}
\alias{predict.funkSVD}
\title{Funk SVD for Matrices with Missing Data}
\description{
Implements matrix decomposition by the stochastic gradient descent optimization popularized by Simon Funk to minimize the error on the known values.
}
\usage{
funkSVD(x, k = 10, gamma = 0.015, lambda = 0.001,
  min_improvement = 1e-06, min_epochs = 50, max_epochs = 200,
  verbose = FALSE)
}
\arguments{
  \item{x}{ a matrix, potentially containing NAs.}
  \item{k}{ number of features (i.e, rank of the approximation). }
  \item{gamma}{ regularization term. }
  \item{lambda}{ learning rate. }
  \item{min_improvement}{ required minimum improvement per iteration. }
  \item{min_epochs}{ minimum number of iterations per feature. }
  \item{max_epochs}{ maximum number of iterations per feature. }
  \item{verbose}{ show progress. }
}
\details{
Funk SVD decomposes a matrix (with missing values)
into two components \eqn{U} and \eqn{V}.
The singular values are folded into these matrices.
The approximation
for the original matrix can be obtained by \eqn{R = UV'}.

This function \code{predict} in this implementation folds in new data rows
by estimating the \eqn{u} vectors using gradient descend and then calculating
the reconstructed complete matrix r for these users via \eqn{r = uV'}.
}
\value{
An object of class \code{"funkSVD"} with components
\item{U}{ the \eqn{U} matrix. }
\item{V}{ the \eqn{V} matrix. }
\item{parameters}{ a list with parameter values. }
}
\references{
Y. Koren, R. Bell, and C. Volinsky. Matrix Factorization Techniques for Recommender Systems, IEEE Computer, pp. 42-49, August 2009.
}
%\author{
%}
\note{
The code is based on the implmentation in package \pkg{rrecsys} by
Ludovik Coba and Markus Zanker.
}
\examples{
### this takes a while to run
\dontrun{
data("Jester5k")

train <- as(Jester5k[1:100], "matrix")
fsvd <- funkSVD(train, verbose = TRUE)

### reconstruct the rating matrix as R = UV'
### and calculate the root mean square error on the known ratings
r <- tcrossprod(fsvd$U, fsvd$V)
rmse(train, r)

### fold in new users for matrix completion
test <- as(Jester5k[101:105], "matrix")
p <- predict(fsvd, test, verbose = TRUE)
rmse(test, p)
}
}
\keyword{model}
