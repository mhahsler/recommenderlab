### Define new S4 generics

setGeneric(".splitKnownUnknown",
  function(data, ...)
    standardGeneric(".splitKnownUnknown"))

setGeneric("hasRating",
  function(x, ...)
    standardGeneric("hasRating"))

setGeneric("nratings",
  function(x, ...)
    standardGeneric("nratings"))

setGeneric("getRatings",
  function(x, ...)
    standardGeneric("getRatings"))

setGeneric("getNormalize",
  function(x, ...)
    standardGeneric("getNormalize"))

setGeneric("getRatingMatrix",
  function(x, ...)
    standardGeneric("getRatingMatrix"))

setGeneric("normalize",
  function(x, ...)
    standardGeneric("normalize"))

setGeneric("denormalize",
  function(x, ...)
    standardGeneric("denormalize"))

setGeneric("getData",
  function(x, ...)
    standardGeneric("getData"))

setGeneric("getModel",
  function(x, ...)
    standardGeneric("getModel"))

setGeneric("getRuns",
  function(x, ...)
    standardGeneric("getRuns"))

setGeneric("getConfusionMatrix",
  function(x, ...)
    standardGeneric("getConfusionMatrix"))

setGeneric("getResults",
  function(x, ...)
    standardGeneric("getResults"))

setGeneric("getTopNLists",
  function(x, ...)
    standardGeneric("getTopNLists"))

setGeneric("evaluate",
  function(x, method, ...)
    standardGeneric("evaluate"))

setGeneric("avg",
  function(x, ...)
    standardGeneric("avg"))

setGeneric("binarize",
  function(x, ...)
    standardGeneric("binarize"))

setGeneric("colCounts",
  function(x, ...)
    standardGeneric("colCounts"))

setGeneric("rowCounts",
  function(x, ...)
    standardGeneric("rowCounts"))

setGeneric("rowSds",
  function(x, ...)
    standardGeneric("rowSds"))

setGeneric("colSds",
  function(x, ...)
    standardGeneric("colSds"))

setGeneric("bestN",
  function(x, ...)
    standardGeneric("bestN"))

setGeneric("calcPredictionAccuracy",
  function(x, data, ...)
    standardGeneric("calcPredictionAccuracy"))

setGeneric("evaluationScheme",
  function(data, ...)
    standardGeneric("evaluationScheme"))

setGeneric("removeKnownRatings",
  function(x, ...)
    standardGeneric("removeKnownRatings"))

setGeneric("removeKnownItems",
  function(x, ...)
    standardGeneric("removeKnownItems"))

setGeneric("Recommender",
  function(data, ...)
    standardGeneric("Recommender"))

setGeneric("similarity",
  function(x,
    y = NULL,
    method = NULL,
    args = NULL,
    ...)
    standardGeneric("similarity"))

setGeneric("getData.frame",
  function(from, ...)
    standardGeneric("getData.frame"))

#' @title
#'   List and Data.frame Representation for Recommender Matrix Objects
#'
#' @description Create a list or data.frame representation for various objects
#' used in \pkg{recommenderlab}. These functions are used in addition to
#' available coercion to allow for parameters like \code{decode}.
#' @aliases getList,binaryRatingMatrix-method
#' @aliases getList,realRatingMatrix-method
#' @aliases getList,topNList-method
#' @aliases getData.frame
#' @aliases getData.frame,ratingMatrix-method
#'
#' @usage getList(from, ...)
#' \S4method{getList}{realRatingMatrix}(from, decode = TRUE, ratings = TRUE, ...)
#' \S4method{getList}{binaryRatingMatrix}(from, decode = TRUE, ...)
#' \S4method{getList}{topNList}(from, decode = TRUE, ...)
#'
#' getData.frame(from, ...)
#' \S4method{getData.frame}{ratingMatrix}(from, decode = TRUE, ratings = TRUE, ...)
#'
#' @param from  object to be represented as a list.
#'
#' @param decode  use item names or item IDs (column numbers) for items?
#'
#' @param ratings  include ratings in the list or data.frame?
#'
#' @param ...  further arguments (currently unused).
#'
#' @details Lists have one vector with items (and ratings) per user. The
#' data.frame has one row per rating with the user in the first column,
#' the item as the second and the rating as the third.
#'
#' @return Returns a list or a data.frame.
#'
#' @seealso \code{\linkS4class{binaryRatingMatrix}},
#' \code{\linkS4class{realRatingMatrix}},
#' \code{\linkS4class{topNList}}.
#'
#' @examples data(Jester5k)
#'
#' getList(Jester5k[1,])
#' getData.frame(Jester5k[1,])
#' @family data preparation
#' @name getList
setGeneric("getList",
  function(from, ...)
    standardGeneric("getList"))
