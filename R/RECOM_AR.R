## recommendations based on association rules

.BIN_AR_param <- list(
  support = 0.1,
  confidence = 0.8,
  maxlen = 3,
  maxtime = 5,
  sort_measure = "confidence",
  sort_decreasing = TRUE,
  apriori_control = list(),
  verbose = FALSE
)

BIN_AR <- function(data, parameter = NULL) {

  ## parameters
  p <- getParameters(.BIN_AR_param, parameter)

  p$apriori_control$verbose <- p$verbose

  ## binaryRatingMatrix stores arules transactions!
  data <- data@data

  ## new version of arules warns for maxlen constraint
  rule_base <-  suppressWarnings(
    apriori(data,
    parameter=list(support=p$support, confidence=p$confidence,
      minlen=2, maxlen=p$maxlen, maxtime=p$maxtime), control=p$apriori_control)
    )

  ## additional measures for sorting the rulebase
  if(p$sort_measure == "cxs") quality(rule_base) <- cbind(quality(rule_base),
    cxs = quality(rule_base)$confidence * quality(rule_base)$support)

  if(!p$sort_measure %in% names(quality(rule_base))) quality(rule_base) <-
    cbind(quality(rule_base), interestMeasure(rule_base, method = p$sort_measure,
      transactions = data))

  ## sort rule_base
  rule_base <- sort(rule_base, by = p$sort_measure, decreasing=p$sort_decreasing)

  if(p$verbose) cat("\nRule base size:", length(rule_base), "rules.\n")

  if(length(rule_base)<1) warning("Rule base does not contain any rules. Decrease support and/or confidence!")

  model <- c(list(
    description = "AR: rule base",
    rule_base = rule_base
  ), p
  )

  predict <- function(model, newdata, n=10, data=NULL,
    type = c("topNList", "ratings", "ratingMatrix"), ...) {

    type <- match.arg(type)

    ## newdata are userid
    if(is.numeric(newdata)) {
      if(is.null(data) || !is(data, "ratingMatrix"))
        stop("If newdata is a user id then data needes to be the training dataset.")
      newdata <- data[newdata,, drop=FALSE]
    }

    n <- as.integer(n)
    sort_measure <- model$sort_measure

    m <- is.subset(lhs(model$rule_base), newdata@data)
    reclist <- list()
    for(i in 1:nrow(newdata)) {
      recom <- unique(unlist(
        LIST(rhs(sort(model$rule_base[m[,i]], by = sort_measure)),
          decode=FALSE)))

      reclist[[i]] <- if(!is.null(recom)) recom else integer(0)
    }

    names(reclist) <- rownames(newdata)

    topN <- new("topNList", items = reclist, itemLabels = colnames(newdata), n = ncol(newdata))
    topN <- removeKnownItems(topN, newdata)
    if(type == "topNList") {
      return(bestN(topN, n))
    }

    ### FIXME: The order in the topN list could be converted into a rating (e.g., pos1 -> 1, pos2 -> 0.9, etc.)
    ratings <- new("realRatingMatrix", data = as(topN, "dgCMatrix"))
    returnRatings(ratings, newdata, type, n)

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
