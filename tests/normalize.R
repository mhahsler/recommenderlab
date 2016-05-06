library("recommenderlab")

set.seed(1234)

## create a matrix with ratings
db <- matrix(sample(c(NA,0:5),100, replace=TRUE, prob=c(.7,rep(.3/6,6))),
	nrow=10, ncol=10, dimnames = list(
		users=paste('u', 1:10, sep=''),
		items=paste('i', 1:10, sep='')
		))

## do normalization
r <- as(db, "realRatingMatrix")
r_u1 <- normalize(r) 
r_u2 <- normalize(r, method="Z-score")

## check if denormalization works
if(!identical(r, denormalize(r_u1))) stop("normalized failed.")
if(!identical(r, denormalize(r_u2))) stop("normalized failed.")

## correct results?
r_u1_true <- t(apply(db, MARGIN=1, FUN=function(x) x -mean(x,na.rm=TRUE)))
names(dimnames(r_u1_true))[2] <- "items" ## fix dimnames

if(!identical(as(r_u1, "matrix"), r_u1_true)) stop("normalized failed.")

## FIXME: test for Z-score missing


## items

r_i1 <- normalize(r, row=FALSE)
if(!identical(r, denormalize(r_i1))) stop("normalized failed.")

