
## we have to make sure it's all evaluationResults
setAs("list", "evaluationResultList",
	function(from) {
		if(!all(sapply(from, is, "evaluationResults"))) stop("List can only contain evaluationResults!")
		
		new("evaluationResultList", from)
	})

setMethod("show", signature(object = "evaluationResultList"),
	function(object) {
		writeLines(sprintf("List of evaluation results for %d recommenders:",
				length(object)))
		lapply(object, show)
		invisible(NULL)
	})

## avg
setMethod("avg", signature(x = "evaluationResultList"), 
	function(x, trim = 0, na.rm = FALSE, ...) { 
		lapply(x, avg)
	})

setMethod("[", signature(x = "evaluationResultList", i = "ANY", j = "missing",
			drop = "missing"),
	function(x, i, j, ..., drop) {
            l <- as(as(x, "list")[i], "evaluationResultList")
            names(l) <- names(x)[i]
            l
        })

## work out of the box: names, [[


