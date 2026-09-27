# Dataset documentation

#' @title
#' Jester dataset (5k sample)
#'
#' @description The data set contains a sample of 5000 users from the anonymous
#' ratings data from
#' the Jester Online Joke Recommender System collected between
#' April 1999 and May 2003.
#' @aliases JesterJokes
#' @docType data
#'
#' @usage data(Jester5k)
#'
#' @details \code{Jester5k} contains a 5000 x 100 rating matrix (5000 users and 100 jokes)
#' with ratings between -10.00 and +10.00. All selected users have
#' rated 36 or more jokes.
#'
#' The data also contains the actual jokes in \code{JesterJokes}.
#'
#' @format   The format of \code{Jester5k} is: Formal class 'realRatingMatrix' [package "recommenderlab"]
#'
#'   The format of \code{JesterJokes} is: vector of character strings.
#'
#' @references Ken Goldberg, Theresa Roeder, Dhruv Gupta, and  Chris Perkins.
#' "Eigentaste: A Constant Time Collaborative Filtering Algorithm."
#' Information Retrieval, 4(2), 133-151. July 2001.
#'
#' @examples data(Jester5k)
#' Jester5k
#'
#' ## number of ratings
#' nratings(Jester5k)
#'
#' ## number of ratings per user
#' summary(rowCounts(Jester5k))
#'
#' ## rating distribution
#' hist(getRatings(Jester5k), main="Distribution of ratings")
#'
#' ## 'best' joke with highest average rating
#' best <- which.max(colMeans(Jester5k))
#' cat(JesterJokes[best])
#' @keywords datasets
#' @family datasets
#' @name Jester5k
"Jester5k"

#' @title
#' Anonymous web data from www.microsoft.com
#'
#' @description Records the Vroots visited by users during a one-week period.
#' @docType data
#'
#' @usage data(MSWeb)
#'
#' @details The data set was created by sampling and processing the www.microsoft.com logs.
#' It records site use by 38,000 anonymous, randomly selected users. For each user, it lists all areas of the web
#' site (Vroots) that the user visited during a one-week period in February 1998.
#'
#' This data set contains 32,710 valid users and 285 Vroots.
#'
#' @format   The format is: Formal class \code{"binaryRatingMatrix"}.
#'
#' @source Asuncion, A., Newman, D.J. (2007). UCI Machine Learning Repository, Irvine, CA:
#' University of California, School of Information and Computer Science.
#' \url{https://archive.ics.uci.edu/}
#'
#' @references J. Breese, D. Heckerman., C. Kadie (1998). Empirical Analysis of Predictive
#' Algorithms for Collaborative Filtering, Proceedings of the Fourteenth
#' Conference on Uncertainty in Artificial Intelligence, Madison, WI.
#'
#' @examples data(MSWeb)
#' MSWeb
#'
#' nratings(MSWeb)
#'
#' ## look at first two users
#' as(MSWeb[1:2,], "list")
#'
#' ## items per user
#' hist(rowCounts(MSWeb), main="Distribution of Vroots visited per user")
#' @keywords datasets
#' @family datasets
#' @name MSWeb
"MSWeb"

#' @title
#' MovieLense Dataset (100k)
#'
#' @description The 100k MovieLense
#' ratings data set. The data was collected through the MovieLens web site
#' (movielens.umn.edu) during the seven-month period from September 19th,
#' 1997 through April 22nd, 1998.
#' The data set contains about 100,000 ratings (1-5)
#' from 943 users on 1664 movies. Movie and user metadata is also provided in \code{MovieLenseMeta} and \code{MovieLenseUser}.
#' @aliases MovieLenseMeta
#' @aliases MovieLenseUser
#' @docType data
#'
#' @usage data(MovieLense)
#'
#' @format   The format of \code{MovieLense} is an object of class \code{"realRatingMatrix"}
#'
#'   The format of \code{MovieLenseMeta} is a data.frame with movie title, year, IMDb URL and indicator variables for 19 genres.
#'
#'   The format of \code{MovieLenseUser} is a data.frame with user age, sex, occupation and zip code.
#'
#' @source GroupLens Research, \url{https://grouplens.org/datasets/movielens/}
#'
#' @references Herlocker, J., Konstan, J., Borchers, A., Riedl, J.. An Algorithmic
#' Framework for Performing Collaborative Filtering. Proceedings of the
#' 1999 Conference on Research and Development in Information
#' Retrieval. Aug. 1999.
#'
#' @examples data(MovieLense)
#' MovieLense
#'
#' ## look at the first few ratings of the first user
#' head(as(MovieLense[1,], "list")[[1]])
#'
#' ## visualize part of the matrix
#' image(MovieLense[1:100,1:100])
#'
#' ## number of ratings per user
#' hist(rowCounts(MovieLense))
#'
#' ## number of ratings per movie
#' hist(colCounts(MovieLense))
#'
#' ## mean rating (averaged over users)
#' mean(rowMeans(MovieLense))
#'
#' ## available movie meta information
#' head(MovieLenseMeta)
#'
#' ## available user meta information
#' head(MovieLenseUser)
#' @keywords datasets
#' @family datasets
#' @name MovieLense
"MovieLense"
