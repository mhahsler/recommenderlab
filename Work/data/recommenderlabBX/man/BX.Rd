\name{BX}
\alias{BX}
\alias{BXBooks}
\docType{data}
\title{Book-Crossing Dataset (BX)}
\description{
The data set contains a 4-week crawl (August / September 2004) from the
Book-Crossing community.  Contains 278,858 users (anonymized) 
providing 1,149,780 ratings (explicit / implicit)
about 271,379 books. 
}
\usage{
    data(BX)
    data(BXBooks)
    }
\format{
  The format is: 
  
  BX: Formal class 'realRatingMatrix' [package "recommenderlab"] 
  BXBooks: data.frame with book meta information (title, author, year, publisher)
}
\details{
44778 x 204680 rating matrix of class 'realRatingMatrix' with 493813 ratings
between 0 and 10.
}
%\source{
%}
\references{
Cai-Nicolas Ziegler, Sean M. McNee, Joseph A. Konstan, Georg Lausen,
"Improving Recommendation Lists Through Topic Diversification,"
Proceedings of the 14th International World Wide Web Conference (WWW '05), May 10-14, 2005, Chiba, Japan.
}
\examples{
data(BX)
BX

hist(getRatings(BX), main="Distribution of ratings", breaks=10)
}
\keyword{datasets}
