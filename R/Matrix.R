## convert to and from dgCMatrix to preserve 0s and do not store NAs

dropNA2matrix <- function(x) {
    if(!is(x, "dsparseMatrix")) stop("x needs to be a subclass of dsparseMatrix!")

    if(!is(x, "dgCMatrix")) x <- as(x, "dgCMatrix")

    x@x[x@x==0] <- NA
    zeros<-Matrix::which(is.na(x), arr.ind=TRUE)
    x <- as(x, "matrix")
    x[x==0] <- NA
    x[zeros]<-0
    x
}

dropNA <- function(x) {
    if(!is(x, "dsparseMatrix")
		&& !is(x, "matrix")) 
	stop("x needs to be a subclass of dsparseMatrix or matrix!")

    if(is(x, "matrix")) zeros<-which(x==0, arr.ind=TRUE)
    else { 
	if(!is(x, "dgCMatrix")) x <- as(x, "dgCMatrix")
	
	nas <- which(is.na(x@x))
	x@x[x@x==0] <- NA
	x@x[nas] <- 0
	zeros<-Matrix::which(is.na(x), arr.ind=TRUE)
    }
    x[is.na(x)] <- 0
    x[zeros]<-NA
    x <- drop0(x)
    x[zeros]<-0
    x
}

