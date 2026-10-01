## create a recommender (find recommender and use data for learning)

#' @title
#' Create a Recommender Model
#'
#' @description Learns a recommender model from given data.
#' @aliases recommenderRegistry
#' @aliases Recommender,ratingMatrix-method
#' @aliases getModel,Recommender-method
#'
#' @usage Recommender(data, ...)
#' \S4method{Recommender}{ratingMatrix}(data, method, parameter=NULL)
#'
#' @param data training data.
#'
#' @param method a character string defining the recommender method to use
#' 	(see details).
#'
#' @param parameter parameters for the recommender algorithm.
#'
#' @param \dots further arguments.
#'
#' @details Recommender uses the registry mechanism from package \pkg{registry}
#' to manage methods. This lets users easily specify and add new methods.
#' The registry is called \code{recommenderRegistry}. See examples section.
#' Methods \code{SVD} and \code{LIBMF} require the suggested packages
#' \pkg{irlba} and \pkg{recosystem}, respectively. Install the corresponding
#' package before using either method.
#'
#' @return An object of class 'Recommender'.
#'
#' @seealso \code{\linkS4class{Recommender}},
#' \code{\linkS4class{ratingMatrix}},
#' \code{\link{predict}}.
#'
#' @examples data("MSWeb")
#' MSWeb10 <- sample(MSWeb[rowCounts(MSWeb) >10,], 100)
#'
#' rec <- Recommender(MSWeb10, method = "POPULAR")
#' rec
#'
#' getModel(rec)
#'
#' ## save and read a recommender model
#' saveRDS(rec, file = "rec.rds")
#' rec2 <- readRDS("rec.rds")
#' rec2
#' unlink("rec.rds")
#'
#' ## look at registry and a few methods
#' recommenderRegistry$get_entry_names()
#'
#' recommenderRegistry$get_entry("POPULAR", dataType = "binaryRatingMatrix")
#'
#' recommenderRegistry$get_entry("SVD", dataType = "realRatingMatrix")
#' @family recommender models
#' @name Recommender
setMethod("Recommender", signature(data = "ratingMatrix"),
function(data, method, parameter = NULL) {
	recom <- recommenderRegistry$get_entry(
		method = method, dataType = class(data))
	if(is.null(recom)) stop(paste("Recommender method", method, 
			"not implemented for data type", class(data),"."))

	## this is expected to return a valid Recommender object
	recom$fun(data = data, parameter = parameter)
})

setMethod("show", signature(object = "Recommender"),
	function(object) {
		cat("Recommender of type", sQuote(object@method), 
			"for", sQuote(object@dataType),
			"\nlearned using", object@ntrain, "users.\n")
		invisible(NULL)
	})
	
setMethod("getModel", signature(x = "Recommender"),
	function(x, ...) x@model)
