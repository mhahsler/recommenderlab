library("testthat")
library("recommenderlab")

data("MovieLense")

MovieLense100 <- MovieLense[rowCounts(MovieLense) > 100,]
MovieLense100 <- MovieLense[, colCounts(MovieLense100) > 100,]
train <- MovieLense100[1:500]
test1 <- MovieLense100[501]
test3 <- MovieLense100[501:503]

### test all recommenders
methods <- unique(sapply(recommenderRegistry$get_entries(
  dataType="realRatingMatrix"), "[[", "method"))

for(m in methods) {
  context(paste("Algorithm:", m))
  cat("Algorithm:", m, "\n")
  rec <- Recommender(train, method = m)
  rec

  ### default is top-N list
  pre <- predict(rec, test1, n = 10)
  pre
  l <- as(pre, "list")
  expect_identical(length(l), 1L)
  expect_identical(length(l[[1]]), 10L)


  pre <- predict(rec, test3, n = 10)
  pre
  l <- as(pre, "list")
  expect_identical(length(l), 3L)
  expect_equal(as.integer(sapply(l, length)), c(10L, 10L, 10L))

  ### contains NAs for known ratings
  pre <- predict(rec, test1, n = 10, type = "ratings")
  pre
  expect_gt(sum(is.na(as(pre, "matrix"))), 0L)

  ### full rating matrix
  pre <- predict(rec, test1, n = 10, type = "ratingMatrix")
  pre
  ### there can be NAs
  #expect_equal(sum(is.na(as(pre, "matrix"))), 0L)

  pre <- predict(rec, test3, n = 10, type = "ratings")
  pre
  expect_gt(sum(is.na(as(pre, "matrix"))), 0L)
  pre <- predict(rec, test3, n = 10, type = "ratingMatrix")
  pre
  ### there can be NAs
  #expect_equal(sum(is.na(as(pre, "matrix"))), 0L)
}

### test all binary recommenders
methods <- unique(sapply(recommenderRegistry$get_entries(
  dataType="binaryRatingMatrix"), "[[", "method"))
