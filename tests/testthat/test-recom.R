library("testthat")
library("recommenderlab")

data("MovieLense")

### test all real rating recommenders
context("Test real rating algorithms")
methods <- unique(sapply(
  recommenderRegistry$get_entries(dataType = "realRatingMatrix"),
  "[[",
  "method"
))

if (interactive())
  cat("Available methods for realRatingMatrix:",
    paste(methods, collapse = ", "))

MovieLense100 <- MovieLense[rowCounts(MovieLense) > 100,]
MovieLense100 <- MovieLense[, colCounts(MovieLense100) > 100,]
train <- MovieLense100[1:20]
test1 <- MovieLense100[101]
test3 <- MovieLense100[101:103]

for (m in methods) {
  ### skip hybrid recommender
  if (m == "HYBRID")
    next

  if (interactive())
    cat("Algorithm:", m, "\n")

  if (m == "SVD")
    suppressWarnings(rec <- Recommender(train, method = m))
  else
    rec <- Recommender(train, method = m)


  rec

  ### default is top-N list
  pre <- predict(rec, test1, n = 10)
  pre
  l <- as(pre, "list")
  expect_identical(length(l), 1L)
  expect_identical(length(l[[1]]), 10L)

  ### default is ton-N list
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
  ### RERECOMMEND cannot do it
  if (m != "RERECOMMEND") {
    pre <- predict(rec, test1, n = 10, type = "ratingMatrix")
    pre
    ### there can be NAs
    #expect_equal(sum(is.na(as(pre, "matrix"))), 0L)
  }

  pre <- predict(rec, test3, n = 10, type = "ratings")
  pre
  expect_gt(sum(is.na(as(pre, "matrix"))), 0L)

  ### RERECOMMEND cannot do it
  if (m != "RERECOMMEND") {
    pre <- predict(rec, test3, n = 10, type = "ratingMatrix")
    pre
  }
  ### there can be NAs
  #expect_equal(sum(is.na(as(pre, "matrix"))), 0L)
}

### Test HybridRecommender (just check if it fails)
recom <- HybridRecommender(
  Recommender(train, method = "POPULAR"),
  Recommender(train, method = "RANDOM"),
  Recommender(train, method = "RERECOMMEND"),
  weights = c(.6, .1, .3)
)
#recom
#getModel(recom)

predict(recom, test1)
predict(recom, test3)
predict(recom, test1, type = "ratings")
predict(recom, test3, type = "ratings")

### test all binary recommenders
context("Test binary algorithms")

methods <- unique(sapply(
  recommenderRegistry$get_entries(dataType = "binaryRatingMatrix"),
  "[[",
  "method"
))

if (interactive())
  cat("Available methods for binaryRatingMatrix:",
    paste(methods, collapse = ", "))

MovieLense100_bin <- binarize(MovieLense100, minRating = 3)
train <- MovieLense100_bin[1:50]
test1 <- MovieLense100_bin[101]
test3 <- MovieLense100_bin[101:103]

for (m in methods) {
  ### skip hybrid recommender
  if (m == "HYBRID")
    next


  if (interactive())
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

  ## do I get errors?
  pre <- predict(rec, test1, n = 10, type = "ratings")

  if (interactive()) {
    cat("Prediction range (should be [0,1]):\n")
    print(summary(as.vector(as(pre, "matrix"))))
  }

  if (m != "RERECOMMEND") {
    pre <- predict(rec, test1, n = 10, type = "ratingMatrix")
  }

}

### Test HybridRecommender (just check if it fails)
recom <- HybridRecommender(
  Recommender(train, method = "POPULAR"),
  Recommender(train, method = "RANDOM"),
  Recommender(train, method = "AR"),
  Recommender(train, method = "RERECOMMEND"),
  ### not implemented for binary data
  weights = c(.25, .25, .25, .25)
)
#recom
#getModel(recom)

predict(recom, test1)
predict(recom, test3)
predict(recom, test1, type = "ratings")
predict(recom, test3, type = "ratings")
