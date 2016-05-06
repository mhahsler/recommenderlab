### Define new S4 generics

setGeneric(".splitKnownUnknown",
	function(data, ...) standardGeneric(".splitKnownUnknown"))

setGeneric("nratings",
	function(x, ...) standardGeneric("nratings"))

setGeneric("getRatings",
	function(x, ...) standardGeneric("getRatings"))

setGeneric("getNormalize",
	function(x, ...) standardGeneric("getNormalize"))

setGeneric("normalize",
	function(x, ...) standardGeneric("normalize"))

setGeneric("denormalize",
	function(x, ...) standardGeneric("denormalize"))

setGeneric("getData",
	function(x, ...) standardGeneric("getData"))

setGeneric("getModel",
	function(x, ...) standardGeneric("getModel"))

setGeneric("getRuns",
	function(x, ...) standardGeneric("getRuns"))

setGeneric("getConfusionMatrix",
	function(x, ...) standardGeneric("getConfusionMatrix"))

setGeneric("getTopNLists",
	function(x, ...) standardGeneric("getTopNLists"))

setGeneric("evaluate",
	function(x, method, ...) standardGeneric("evaluate"))

setGeneric("avg",
	function(x, ...) standardGeneric("avg"))

setGeneric("binarize",
	function(x, ...) standardGeneric("binarize"))

setGeneric("colCounts",
	function(x, ...) standardGeneric("colCounts"))

setGeneric("rowCounts",
	function(x, ...) standardGeneric("rowCounts"))

setGeneric("rowSds",
	function(x, ...) standardGeneric("rowSds"))

setGeneric("colSds",
	function(x, ...) standardGeneric("colSds"))

setGeneric("bestN",
	function(x, ...) standardGeneric("bestN"))

setGeneric("calcPredictionAccuracy", 
	function(x, data, ...) standardGeneric("calcPredictionAccuracy"))

setGeneric("evaluationScheme",
	function(data, ...) standardGeneric("evaluationScheme"))

setGeneric("removeKnownRatings",
	function(x, ...) standardGeneric("removeKnownRatings"))

setGeneric("removeKnownItems",
	function(x, ...) standardGeneric("removeKnownItems"))

setGeneric("Recommender",
	function(data, ...) standardGeneric("Recommender"))

setGeneric("similarity",
	function(x, y = NULL, method = NULL, args = NULL, ...) 
	standardGeneric("similarity"))

setGeneric("getData.frame",
	function(from, ...) standardGeneric("getData.frame"))

setGeneric("getList",
	function(from, ...) standardGeneric("getList"))
