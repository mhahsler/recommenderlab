##*******************************************************************
## dissimilarity for binaryRatingMatrix
setMethod("dissimilarity", signature(x = "binaryRatingMatrix"),
	function(x, y = NULL, method = NULL, args = NULL, which = "users") {
	    
	    args <- .get_parameters(list(alpha=.5), args)

	    which <- tolower(which)
	    if(!is.null(method)) method <- tolower(method)
	    else method <- "jaccard"
	    
	    
	    ## handle karypis and conditional dissimilarities
	    if(method == "karypis") {
		if(!is.null(y) || which != "items") stop("Kaypis dissimilarities are not implemented between users or as a cross-dissimilarity!")

		return(.karypis(as(x, "dgCMatrix"), dist=TRUE, args))
	    }

	    if(method == "conditional") {
		if(!is.null(y) || which != "items") stop("Conditional dissimilarities are not implemented between users or as a cross-dissimilarity!")

		return(.conditional(as(x, "dgCMatrix"), dist=TRUE, args))
	    }

	
	    ## dissimilarity is defined in arules for itemMatrix
	    if(which == "users") which <- "transactions" ## "items" is ok
	    x <- x@data
	    if(!is.null(y)) y <- y@data

	    ## dissimilarity in arules sets the method attribute 
	    arules::dissimilarity(x, y, method, args, which)
	}
	)



##*******************************************************************
## wrapper for realRatingMatrix (transactions)
## Idea by Christopher Koeb

setMethod("dissimilarity", signature(x = "realRatingMatrix"),
	function(x, y = NULL, method = NULL, args = NULL, 
		which = "users") {

	    args <- .get_parameters(list(na_as_zero = FALSE, alpha=.5), args)
	    
	    which <- tolower(which)
	    if(!is.null(method)) method <- tolower(method)
	    else method <- "cosine"


	    ### FIX this code!
	    ## shortcut for Cosine (compute sparse)
	    #if(method=="cosine" && is.null(y)) {
	    #	x <- as(x, "dgCMatrix")
	    #	return(as.dist(1- crossprod(x / sqrt(rowSums(x ^ 2)))))
	    #}
	    
	    ## handle karypis and conditional dissimilarities
	    if(method == "karypis") {
		if(!is.null(y) || which != "items") stop("Kaypis dissimilarities are not implemented between users or as a cross-dissimilarity!")

		return(.karypis(as(x, "dgCMatrix"), dist=TRUE, args))
	    }

	    if(method == "conditional") {
		if(!is.null(y) || which != "items") stop("Conditional dissimilarities are not implemented between users or as a cross-dissimilarity!")

		return(.conditional(as(x, "dgCMatrix"), dist=TRUE, args))
	    }


	    ## do regular distances
	    ## FIXME: we can do some distances faster

	    x <- as(x, "matrix")
	    if(which == "items") x <- t(x) 
	    if(args$na_as_zero) x[is.na(x)] <- 0


	    if(!is.null(y)) { 
		y <- as(y, "matrix")
		if(which == "items") y <- t(y) 
		if(args$na_as_zero) y[is.na(y)] <- 0
	    }

      ### of person we only use 1-pos. corr
	    if(method == "pearson") {
		if(!is.null(y)) y <- t(y)
		pc <- suppressWarnings(cor(t(x), y, method="pearson", 
					use="pairwise.complete.obs"))
		pc[pc<0] <- 0
    #pc[is.na(pc)] <- 0
    if(is.null(y)) pc <- as.dist(pc)
    return(1-pc)
		}

	    ## dist in proxy
	    proxy::dist(x = x, y = y, method = method)
	})

setMethod("similarity", signature(x = "ratingMatrix"),
	function(x, y = NULL, method = NULL, args = NULL, 
		which = "users") {

	    which <- tolower(which)
	    if(!is.null(method)) method <- tolower(method)
	    else method <- "cosine"

	    ## handle karypis and conditional similarities
	    if(method == "karypis") {
		if(!is.null(y) || which != "items") stop("Kaypis similarities are not implemented between users or as a cross-similarity!")

		return(.karypis(as(x, "dgCMatrix"), dist=FALSE, args))
	    }

	    if(method == "conditional") {
		if(!is.null(y) || which != "items") stop("Conditional similarities are not implemented between users or as a cross-similarity!")

		return(.conditional(as(x, "dgCMatrix"), dist=FALSE, args))
	    }
		
	    ## use dissimilarity and convert into a similarity
	    d <- dissimilarity(x, y, method, args, which)

	    ## FIXME: other measures in [0,1]
	    if(!is.null(attr(d, "method")) && tolower(attr(d, "method")) 
		    %in% c("jaccard", "cosine")) {
		sim <- 1-d
	    }else{
		sim <- 1/(1+d)
	    }

	    attr(sim, "type") <- "simil"
	    sim
	})

## conditional similarity (Karypis 2001)
.conditional <- function(x, dist=TRUE, args=NULL){
    n <- ncol(x)

    ## sim(v,u) = freq(uv) / freq(v)
    uv <-  crossprod(x)
    v <- matrix(colSums(x), nrow = n, ncol = n, byrow = FALSE)

    sim <- uv/v

    ## fix if freq was 0
    sim[is.na(sim)] <- 0

    if(dist) sim <- as.dist(1/(1+sim))
    else attr(sim, "type") <- "simil"
    attr(sim, "method") <- "conditional"		
    sim
}
	    
## Karypis similarity
.karypis <- function(x, dist, args=NULL) {
    
    ## get alpha
    args <- .get_parameters(list(alpha = .5), args)
    
    n <- ncol(x)

    ## normalize rows to unit length
    x <- x/rowSums(x)

    ## for users without items
    x[is.na(x)] <- 0

    ## sim(v,u) = 
    ##      sum_{for all i: r_i,v >0} r_i,u / freq(v) / freq(u)^alpha
    uv <-  crossprod(x, x>0)
    v <- matrix(colSums(x), nrow = n, ncol = n, byrow = FALSE)
    u <- t(v) 

    sim <- uv/v/u^args$alpha 

    ##  fix if freq = 0
    sim[is.na(sim)] <- 0
    
    if(dist) sim <- as.dist(1/(1+sim))
    else attr(sim, "type") <- "simil"
    attr(sim, "method") <- "karypis"		
    sim

}

