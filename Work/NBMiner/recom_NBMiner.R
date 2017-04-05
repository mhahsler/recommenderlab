library("arulesNBMiner")

## confiugure Java
## sudo R CMD javareconf

## recommendations based on rules from NB-precise rules

build_recom_NBMiner <- function(data, parameter = NULL) {

    ## parameters
    p <- recommenderlab::getParameters(list(
            measure = "precision",
            verb = FALSE,
            NBMiner = NULL
        ), parameter)

    if(is.null(p$NBMiner)) stop("Parameters for NBMiner needed (entry NBMiner)")

    ## make sure NBMiner creates rules
    p$NBMiner@rules <- TRUE
    ## create rule base
    rule_base <- NBMiner(data, parameter=p$NBMiner, control=list(verb = p$verb))

    if(!p$measure %in% names(quality(rule_base))) quality(rule_base) <-
    cbind(quality(rule_base), interestMeasure(rule_base, method = p$measure,
            transactions = data))

    rule_base <- sort(rule_base, by = p$measure, decreasing=p$decreasing)


    model <- c(list(
            description = "NBMiner: rule base obteined by NBMiner.",
            rule_base = rule_base
        ), p
    )


    predict <- function(model, newdata, N=10) {

        reclist <- list()

        m <- is.subset(lhs(model$rule_base), newdata)
        for(i in 1:length(newdata)) {
            recom <- head(unique(
                    unlist(LIST(rhs(
                                sort(model$rule_base[m[,i]], by=model$measure)
                            ), decode=FALSE))
                ),N)

            reclist[[i]]  <- if(!is.null(recom)) recom else numeric(0)
        }

        encode(reclist, itemLabels(newdata))
    }


    structure(list(method = "NBMiner", n_train = length(data),
            model = model, predict = predict),
        class="recommender")

}

## register recommender
recommender_registry$set_entry(method="NBMiner", fun=build_recom_NBMiner,
        description="Recommender based on association rules mined with NBMiner.")

