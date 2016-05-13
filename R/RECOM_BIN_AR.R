## recommendations based on association rules

.BIN_AR_param <- list(
  support = 0.1,
  confidence = 0.3,
  maxlen = 2,
  measure = "confidence",
  verbose = FALSE,
  decreasing = TRUE
)

BIN_AR <- function(data, parameter = NULL) {

  ## parameters
  p <- .get_parameters(.BIN_AR_param, parameter)

  ## binaryRatingMatrix stores arules transactions!
  data <- data@data

  rule_base <- apriori(data,
    parameter=list(support=p$support, confidence=p$confidence,
      minlen=2, maxlen=p$maxlen), control=list(verbose=p$verbose))

  ## additional measures
  if(p$measure == "cxs") quality(rule_base) <- cbind(quality(rule_base),
    cxs = quality(rule_base)$confidence * quality(rule_base)$support)

  if(!p$measure %in% names(quality(rule_base))) quality(rule_base) <-
    cbind(quality(rule_base), interestMeasure(rule_base, method = p$measure,
      transactions = data))

  ## sort rule_base
  rule_base <- sort(rule_base, by = p$measure, decreasing=p$decreasing)

  if(p$verbose) cat("\nRule base size:", length(rule_base), "rules.\n")

  if(length(rule_base)<1) warning("Rule base does not contain any rules. Decrease support and/or confidence!")

  model <- c(list(
    description = "AR: rule base",
    rule_base = rule_base
  ), p
  )


  predict <- function(model, newdata, n=10, data=NULL,
    type = c("topNList"), ...) {

    type <- match.arg(type)

    ## newdata are userid
    if(is.numeric(newdata)) {
      if(is.null(data) || !is(data, "ratingMatrix"))
        stop("If newdata is a user id then data needes to be the training dataset.")
      newdata <- data[newdata,, drop=FALSE]
    }

    n <- as.integer(n)
    measure <- model$measure

    reclist <- list()
    m <- is.subset(lhs(model$rule_base), newdata@data)
    for(i in 1:nrow(newdata)) {
      recom <- head(unique(unlist(
        LIST(rhs(sort(model$rule_base[m[,i]], by=measure)),
          decode=FALSE))), n)

      reclist[[i]] <- if(!is.null(recom)) recom else numeric(0)
    }

    names(reclist) <- rownames(newdata)
    new("topNList", items = reclist, itemLabels = colnames(newdata), n = n)
  }

  ## construct recommender object
  new("Recommender", method = "AR", dataType = "binaryRatingMatrix",
    ntrain = nrow(data), model = model, predict = predict)
}

## register recommender
recommenderRegistry$set_entry(
  method="AR", dataType = "binaryRatingMatrix", fun=BIN_AR,
  description="Recommender based on association rules.",
  parameters=.BIN_AR_param
)
