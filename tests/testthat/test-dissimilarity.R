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

x <- as(db, "realRatingMatrix")

s <- similarity(x)
expect_is(s, "dist")
expect_equal(attr(s, "Size"), nrow(x))

s <- similarity(x, x)
expect_is(s, "crossdist")
expect_equal(dim(s), c(nrow(x), nrow(x)))

# which = "items"
s <- similarity(x, which = "items")
expect_is(s, "dist")
expect_equal(attr(s, "Size"), ncol(x))

s <- similarity(x, x, which = "items")
expect_is(s, "crossdist")
expect_equal(dim(s), c(ncol(x), ncol(x)))

## min_matchin, min_predictive
s_cd <- similarity(x, x, min_matching = 2, min_predictive = 1)
s_d <- similarity(x, min_matching = 2, min_predictive = 1)
expect_equivalent(as.vector(s_cd[lower.tri(s_cd)]), as.vector(s_d))

similarity(x, x, which = "items", min_matching = 0, min_predictive = 0)
similarity(x, x, which = "items", min_matching = 2, min_predictive = 0)
similarity(x, x, which = "items", min_matching = 2, min_predictive = 1)
