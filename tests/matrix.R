library("recommenderlab")

set.seed(1234)

## create a matrix with ratings
db <- matrix(as.numeric(sample(c(NA,0:5)),100, replace=TRUE, prob=c(.7,rep(.3/6,6))),
	nrow=10, ncol=10, dimnames = list(
		users=paste('u', 1:10, sep=''),
		items=paste('i', 1:10, sep='')
		))


## convert
r <- as(db, "realRatingMatrix")

## do we get the original matrix back?
if(!identical(as(r,"matrix"), db)) stop("matrix -> realRatingMatrix ->matrix failed!")

as(r, "data.frame")
as(r, "list")
