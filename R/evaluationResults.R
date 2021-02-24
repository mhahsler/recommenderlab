setMethod("show", signature(object = "evaluationResults"),
  function(object) {
    writeLines(sprintf("Evaluation results for %d folds/samples using method %s.",
      getRuns(object), sQuote(object@method)))
    if(any(!sapply(getModel(object), is.null)))
      writeLines("Result contains predictive models!")

    invisible(NULL)
  })


setMethod("getRuns", signature(x = "evaluationResults"),
  function(x, ...) length(x@results))

setMethod("getModel", signature(x = "evaluationResults"),
  function(x, ...) {
    lapply(x@results, function(y) y@model)
  })

setMethod("getConfusionMatrix", signature(x = "evaluationResults"),
  function(x, ...) {
    lapply(x@results, function(y) y@cm)
  })

setMethod("getResults", signature(x = "evaluationResults"),
  function(x, ...) {
    lapply(x@results, function(y) y@cm)
  })

setMethod("avg", signature(x = "evaluationResults"),
  function(x, trim = 0, na.rm = FALSE, ...) {

    x <- getConfusionMatrix(x)
    if(length(x)>1) {
      avg <- x[[1]]
      for(i in 2:length(x)) avg <- avg+x[[i]]
      x <- avg/length(x)
    }else{
      x <- x[[1]]
    }

    x
  })


