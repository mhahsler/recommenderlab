library(recommenderlab)
source("recom_NBMiner.R")

#data(Epub)
#Epub_s <- Epub[size(Epub)>3 & size(Epub)<60]
#es <- evaluation_scheme(Epub_s, "cross", k=4, given=3)
#save(es, file = "es_epub.rda")
load("es_epub.rda")

Ns <- c(1,2,3,5,10)


## best
param <- list(
    AR_maxlen3_c08 = list("AR", list(supp=0.01, conf=0.8, maxlen=3)),
    
    NBMiner_maxlen3_pi0.9 = list("NBMiner", 
        list(NBMiner = NBMinerParameters(es$data, pi=0.9, theta=0.5, maxlen=3,
        minlen=2, trim = 0.008, verb = TRUE))),

    UBFC_pearson_25 = list("UBCF", 
        list(method = "pearson", nn = 25, weighted=FALSE)),
        
    IBFC_jaccard_k25_norm = list("IBCF", 
        list(method = "jaccard", k = 25, normalize=TRUE)),

    CLUSTER_pearson_k20 = list("CLUSTER", list(k = 20, method="pearson")),

    RANDOM = list("RANDOM", NULL),
    TOPN = list("TOPN", NULL)
)

res <- lapply(param, FUN = function(p) evaluate(es, p[[1]], N=Ns,
        parameter=p[[2]]))   
save(res, file="results_epub_best.rda")

load("results_epub_best.rda")
res <- evaluation_list(res)



## test AR
if(FALSE) {
##
#r <- recommender(evaluation_data(es), "AR", parameter = p_AR5)
#recommender_model(r)
param <- list(
    AR_maxlen2 = list(supp=0.01, conf=0.5, maxlen=2),
    AR_maxlen3 = list(supp=0.01, conf=0.5, maxlen=3),
    AR_maxlen4 = list(supp=0.01, conf=0.5, maxlen=4)
)    

param <- list(
    AR_maxlen2_c0 = list(supp=0.01, conf=0, maxlen=2),
    AR_maxlen3_c0 = list(supp=0.01, conf=0, maxlen=3),
    AR_maxlen4_c0 = list(supp=0.01, conf=0, maxlen=4)
)
    
    AR_maxlen2_c08 = list(supp=0.01, conf=0.8, maxlen=2),
    AR_maxlen3_c08 = list(supp=0.01, conf=0.8, maxlen=3),
    AR_maxlen5_c08 = list(supp=0.01, conf=0.8, maxlen=5),
    
param <- list(
    AR_maxlen2_c05_lift = list(supp=0.01, conf=0.5, maxlen=2, measure="lift"),
    AR_maxlen3_c05_lift = list(supp=0.01, conf=0.5, maxlen=3, measure="lift"),
    AR_maxlen2_c05_cxs = list(supp=0.01, conf=0.5, maxlen=2, measure="cxs"),
    AR_maxlen3_c05_cxs = list(supp=0.01, conf=0.5, maxlen=3, measure="cxs")
)
    
param <- list(
    AR_maxlen4_c05_lift = list(supp=0.01, conf=0.5, maxlen=4, measure="lift"),
    AR_maxlen4_c05_cxs = list(supp=0.01, conf=0.5, maxlen=4, measure="cxs"),
    AR_maxlen3_c0_lift = list(supp=0.01, conf=0, maxlen=3, measure="lift"),
    AR_maxlen3_c0_cxs = list(supp=0.01, conf=0, maxlen=3, measure="cxs")
)

res2 <- lapply(param, FUN = function(p) evaluate(es, "AR", N=Ns,
        parameter=p))   

res <- c(res,res2)
res <- evaluation_list(res)
plot(res)


save(res, file="results_epub_AR.rda")
}


## test NBMiner
if(FALSE) {

    trim <- 0.02
    NBMinerParameters(es$data, trim=trim, plot=TRUE, verb=TRUE)
    
    p <- NBMinerParameters(es$data, pi=0.8, theta=0.5, maxlen=2, minlen=2,
        trim=trim, rules=TRUE)

    r <- NBMiner(evaluation_data(es), parameter=p)
    

    param <- list(
    NBMiner_maxlen2_pi0.8 = list(NBMiner = 
        NBMinerParameters(es$data, pi=0.8, theta=0.5, maxlen=2, minlen=2, 
            trim=trim, rules=TRUE), verb=TRUE), 

    NBMiner_maxlen3_pi0.8 = list(NBMiner = 
        NBMinerParameters(es$data, pi=0.8, theta=0.5, maxlen=3, minlen=2, 
            trim=trim, rules=TRUE), verb=TRUE), 

    NBMiner_maxlen4_pi0.8 = list(NBMiner = 
        NBMinerParameters(es$data, pi=0.8, theta=0.5, maxlen=4, minlen=2, 
            trim=trim, rules=TRUE), verb=TRUE)
    )

    NBMiner_maxlen2_pi0.5 = list(NBMiner = 
        NBMinerParameters(es$data, pi=0.8, theta=0.5, maxlen=4, minlen=2, 
            trim=trim, rules=TRUE), verb=TRUE), 

    NBMiner_maxlen2_pi0.9 = list(NBMiner = 
        NBMinerParameters(es$data, pi=0.8, theta=0.5, maxlen=4, minlen=2, 
            trim=trim, rules=TRUE), verb=TRUE), 

    NBMiner_maxlen2_pi0.99 = list(NBMiner = 
        NBMinerParameters(es$data, pi=0.8, theta=0.5, maxlen=4, minlen=2, 
            trim=trim, rules=TRUE), verb=TRUE) 
)
    
res <- lapply(param, FUN = function(p) evaluate(es, "NBMiner", N=Ns,
        parameter=p))   
res <- evaluation_list(res)

    save(res, file="results_epub_NBMiner.rda")
}

## test UBCF
if(FALSE) {

param <- list(
    UBFC_jaccard_25 = list(method = "jaccard", nn = 25, weighted=FALSE),
    UBFC_cosine_25 = list(method = "cosine", nn = 25, weighted=FALSE),
    UBFC_pearson_25 = list(method = "pearson", nn = 25, weighted=FALSE),
    UBFC_dice_25 = list(method = "dice", nn = 25, weighted=FALSE),
    UBFC_matching_25 = list(method = "matching", nn = 25, weighted=FALSE),
    #UBFC_euclidean_25 = list(method = "euclidean", nn = 25, weighted=FALSE),
    UBFC_affinity_25 = list(method = "affinity", nn = 25, weighted=FALSE),

    UBFC_jaccard_50 = list(method = "jaccard", nn = 50, weighted=FALSE),
    UBFC_jaccard_15 = list(method = "jaccard", nn = 15, weighted=FALSE),
    UBFC_jaccard_20 = list(method = "jaccard", nn = 15, weighted=FALSE),
    UBFC_jaccard_30 = list(method = "jaccard", nn = 15, weighted=FALSE),
    
    UBFC_affinity_15 = list(method = "affinity", nn = 25, weighted=FALSE)
)

res <- lapply(param, FUN = function(p) evaluate(es, "UBCF", N=Ns,
        parameter=p))   

save(res, file="results_epub_UBFC.rda")
}

## test IBCF
if(FALSE) {

    param <- list(
        IBFC_jaccard_k20 = list(method = "jaccard", k = 20, normalize=FALSE),
        IBFC_jaccard_k25 = list(method = "jaccard", k = 25, normalize=FALSE),
        IBFC_jaccard_k30 = list(method = "jaccard", k = 30, normalize=FALSE),
        IBFC_jaccard_k50 = list(method = "jaccard", k = 50, normalize=FALSE),

        IBFC_jaccard_k25_norm = list(method = "jaccard", k = 25, normalize=TRUE),
        IBFC_affinity_k25 = list(method = "affinity", k = 25, normalize=FALSE),
        IBFC_cosine_k25 = list(method = "cosine", k = 25, normalize=FALSE),
        #IBFC_pearson_k25 = list(method = "pearson", k = 25, normalize=FALSE),
        IBFC_dice_k25 = list(method = "dice", k = 25, normalize=FALSE),
        IBFC_matching_k25 = list(method = "matching", k = 25, normalize=FALSE)
    )

    res <- lapply(param, FUN = function(p) evaluate(es, "IBCF", N=Ns,
            parameter=p))   

    save(res, file="results_epub_IBFC.rda")
}

## test cluster
if(FALSE) {
    param <- list(
        CLUSTER_jaccard_k20 = list(k = 20, method="jaccard"),
        CLUSTER_jaccard_k10 = list(k = 10, method="jaccard"),
        CLUSTER_jaccard_k30 = list(k = 30, method="jaccard"),
        
        CLUSTER_pearson_k20 = list(k = 20, method="pearson"),
        CLUSTER_cosine = list(k = 20, method="cosine"),
        CLUSTER_affinity_k20 = list(k = 20, method="affinity"),
        CLUSTER_dice_k20 = list(k = 20, method="dice"),
        CLUSTER_matching_k20 = list(k = 20, method="matching")
    )

    res <- lapply(param, FUN = function(p) evaluate(es, "CLUSTER", N=Ns,
            parameter=p))   

    save(res, file="results_epub_CLUSTER.rda")
}




force_N <- function(x) {
    .force_N <- function(x) {
        x <- lapply(x, FUN=function(y) {
                Ns <- as.integer(rownames(y))
                missing_FP <- Ns - y[,"TP"] - y[,"FP"]
                y[,"FP"] <- Ns - y[,"TP"] 
                y[,"TN"] <- y[,"TN"] - missing_FP 

                ## fix other measures
                y[, "precision"] <- y[, "TP"] / (y[, "TP"] + y[, "FP"]) 
                y[, "recall"] <- y[, "TP"] / (y[, "TP"] + y[, "FN"]) 
                y[, "TPR"] <- y[, "recall"] 
                y[, "FPR"] <- y[, "FP"] / (y[, "FP"] + y[, "TN"]) 

                y    
            }) 

        class(x) <- "evaluation_results"
        x
    }

    if(is(x, "list")) lapply(x, FUN = function(y) .force_N(y))
    else .force_N(x)
}


