## create a recommender (find recommender and use data for learning)

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
