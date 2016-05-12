\name{Error}
\alias{RMSE}
\alias{frobenius}
\alias{MSE}
\alias{MAE}
\title{Error Calculation}
\description{
Calculate the mean absolute error (MAE), mean square error (MSE),
root mean square error (RMSE) and for matrices also the Frobenius norm (identical to RMSE).
}
\usage{
MSE(true, predicted, na.rm = TRUE)
RMSE(true, predicted, na.rm = TRUE)
MAE(true, predicted, na.rm = TRUE)
frobenius(true, predicted, na.rm = TRUE)
}
\arguments{
  \item{true}{ true values. }
  \item{predicted}{ predicted values }
  \item{na.rm}{ ignore missing values. }
}
\details{
Frobenius norm requires matrices.
}
\value{
The error value.
}
\examples{
true <- rnorm(10)
predicted <- rnorm(10)

MAE(true, predicted)
MSE(true, predicted)
RMSE(true, predicted)

true <- matrix(rnorm(9), nrow = 3)
predicted <- matrix(rnorm(9), nrow = 3)

frobenius(true, predicted)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
