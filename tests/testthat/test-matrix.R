library("testthat")
library("recommenderlab")

set.seed(1234)

## create a matrix with ratings
db <- matrix(as.numeric(sample(c(NA,0:5),100, replace=TRUE,
  prob=c(.7,rep(.3/6,6)))),
	nrow=10, ncol=10, dimnames = list(
		users=paste('u', 1:10, sep=''),
		items=paste('i', 1:10, sep='')
		))


## convert
r <- as(db, "realRatingMatrix")

## do we get the original matrix back?
expect_identical(as(r,"matrix"), db)

## Coersions:
expect_identical(r, as(as(r, "matrix"), "realRatingMatrix"))
expect_identical(r, as(as(r, "dgCMatrix"), "realRatingMatrix"))
expect_identical(r, as(as(r, "dgTMatrix"), "realRatingMatrix"))
expect_identical(getRatings(r[c("u1", "u2"), colnames(r)]),
  getRatings(as(as(r, "data.frame"), "realRatingMatrix")[c("u1", "u2"),
    colnames(r)]))

## subset
expect_identical(db[2:5,2:5], as(r[2:5,2:5], "matrix"))

## number of ratings
expect_identical(sum(sapply(as(r, "list"), length)), nrow(as(r, "data.frame")))
expect_identical(nratings(r), sum(sapply(as(r, "list"), length)))

## check ratings
expect_identical(as(r, "dgTMatrix")@x, getRatings(r))

