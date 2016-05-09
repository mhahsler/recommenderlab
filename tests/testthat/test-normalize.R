library("testthat")
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
r_row <- normalize(r)
r_row_z <- normalize(r, method="Z-score")

r_col <- normalize(r, row = FALSE)
r_row_col <- normalize(r_row, row = FALSE)

## check if denormalization works
expect_identical(r, denormalize(r_row))
expect_identical(r, denormalize(r_row_z))
expect_identical(r, denormalize(r_col))
expect_identical(r, denormalize(r_row_col))
expect_identical(r_row, denormalize(r_row_col, row = FALSE))
#expect_identical(r_col, denormalize(r_row_col, row = TRUE))



## correct results?
r_u1_true <- t(apply(db, MARGIN=1, FUN=function(x) x -mean(x,na.rm=TRUE)))
names(dimnames(r_u1_true))[2] <- "items" ## fix dimnames

expect_identical(as(r_row, "matrix"), r_u1_true)

## FIXME: test for Z-score missing

