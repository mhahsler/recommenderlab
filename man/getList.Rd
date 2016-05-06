\name{getList}
\Rdversion{1.1}
\alias{getList}
\alias{getList,binaryRatingMatrix-method}
\alias{getList,realRatingMatrix-method}
\alias{getList,topNList-method}
\alias{getData.frame}
\alias{getData.frame,ratingMatrix-method}
\title{
  List and Data.frame Representation for Recommender Matrix Objects
}
\description{
Create a list or data.frame representation for various objects 
used in \pkg{recommenderlab}. These functions are used in addition to
available coercion to allow for parameters like \code{decode}.
}
\usage{
getList(from, ...)
\S4method{getList}{realRatingMatrix}(from, decode = TRUE, ratings = TRUE, ...)
\S4method{getList}{binaryRatingMatrix}(from, decode = TRUE, ...)
\S4method{getList}{topNList}(from, decode = TRUE, ...)

getData.frame(from, ...)
\S4method{getData.frame}{ratingMatrix}(from, decode = TRUE, ratings = TRUE, ...)
}
\arguments{
  \item{from}{ object to be represented as a list.}
  \item{decode}{ use item names or item IDs (column numbers) for items? }
  \item{ratings}{ include ratings in the list or data.frame?}
  \item{...}{ further arguments (currently unused).}
}
\details{
Lists have one vector with items (and ratings) per user. The 
data.frame has one row per rating with the user in the first column,
the item as the second and the rating as the third.
}
\value{
Returns a list or a data.frame.
}
\seealso{
\code{\linkS4class{binaryRatingMatrix}},
\code{\linkS4class{realRatingMatrix}},
\code{\linkS4class{topNList}}.
}
\examples{
data(Jester5k)

getList(Jester5k[1,])
getData.frame(Jester5k[1,])
}
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
