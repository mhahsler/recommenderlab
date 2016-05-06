setMethod("predict", signature(object = "Recommender"),
	function(object, newdata, n = 10, data=NULL, type="topNList", ...) 
	object@predict(object@model, newdata, n = n, data=data, type= type, ...)
)


