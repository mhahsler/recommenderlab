\name{Recommender}
\Rdversion{1.1}
\alias{Recommender}
\alias{recommenderRegistry}
\alias{Recommender,ratingMatrix-method}
\alias{getModel,Recommender-method}
\title{
Create a Recommender Model
}
\description{
Learns a recommender model from given data.
}
\usage{
Recommender(data, ...)
\S4method{Recommender}{ratingMatrix}(data, method, parameter=NULL)

}
\arguments{
	\item{data}{training data.}
	\item{method}{a character string defining the recommender method to use
	(see details).}
	\item{parameter}{parameters for the recommender algorithm.}
	\item{\dots}{further arguments. }
}
\details{
Recommender uses the registry mechanism from package \pkg{registry}
to manage methods. This let's the user easily specify and add new methods.
The registry is called \code{recommenderRegistry}. See examples section.
}
\value{
An object of class 'Recommender'.
}
\seealso{
\code{\linkS4class{Recommender}},
\code{\linkS4class{ratingMatrix}},
\code{\link{predict}}.
}
\examples{
data("MSWeb")
MSWeb10 <- sample(MSWeb[rowCounts(MSWeb) >10,], 100)

rec <- Recommender(MSWeb10, method = "POPULAR")
rec

getModel(rec)

## save and read a recommender model
saveRDS(rec, file = "rec.rds")
rec2 <- readRDS("rec.rds")
rec2
unlink("rec.rds")

## look at registry and a few methods
recommenderRegistry$get_entry_names()

recommenderRegistry$get_entry("POPULAR", dataType = "binaryRatingMatrix")

recommenderRegistry$get_entry("SVD", dataType = "realRatingMatrix")
}
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
