library("testthat")
library("recommenderlab")

set.seed(1234)

context("dissimilarity")

# check structure
db <- matrix(
  as.numeric(sample(
    c(NA, 0:5), 100, replace = TRUE,
    prob = c(.7, rep(.3 / 6, 6))
  )),
  nrow = 10,
  ncol = 10,
  dimnames = list(
    users = paste('u', 1:10, sep = ''),
    items = paste('i', 1:10, sep = '')
  )
)

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

## min_matching, min_predictive
s_cd <- similarity(x, x, min_matching = 2, min_predictive = 1)
s_d <- similarity(x, min_matching = 2, min_predictive = 1)
expect_equivalent(as.vector(s_cd[lower.tri(s_cd)]), as.vector(s_d))

# TODO: write a test
similarity(
  x,
  x,
  which = "items",
  min_matching = 0,
  min_predictive = 0
)
similarity(
  x,
  x,
  which = "items",
  min_matching = 2,
  min_predictive = 0
)
similarity(
  x,
  x,
  which = "items",
  min_matching = 2,
  min_predictive = 1
)

## Test measures
compn <- function(x, n = 3)
  round(as.vector(x), n)
sim2sim <- function(x)
  (x + 1) / 2

## realRatingMatrix
x <- rbind(c(1,-1), c(-1, 1))
x
rating <- as(matrix(x, ncol = 2), "realRatingMatrix")
rating

# Cosine
ibcf <- Recommender(rating,
  method = 'IBCF',
  parameter = list(method = 'cosine',
    k = 1))

# this is between items
d <- 2
expect_equal(as(ibcf@model$sim, "matrix")[1, 2], sim2sim(1 - d))
# same as: 1 - proxy::dist(t(x), method = "cosine")
expect_equal(compn(dissimilarity(
  rating, method = "cosine", which = "items"
)), d)
expect_equal(compn(similarity(
  rating, method = "cosine", which = "items"
)), sim2sim(1 - d))

# this is between users
d <- 2
expect_equal(compn(dissimilarity(rating, method = "cosine")), d)
expect_equal(compn(similarity(rating, method = "cosine")), sim2sim(1 - d))

# Others
d <- 2
expect_equal(compn(dissimilarity(rating, method = "pearson")), 2)
expect_equal(compn(similarity(rating, method = "pearson")), sim2sim(1 - d))
expect_equal(compn(dissimilarity(
  rating, method = "pearson", which = "items"
)), 2)
expect_equal(compn(similarity(
  rating, method = "pearson", which = "items"
)), sim2sim(1 - d))

# only between items
# TODO: Make sure these are correct. Especially conditional!!!
#expect_equal(compn(dissimilarity(
#  rating, method = "karypis", which = "items"
#)), 1)
expect_equal(compn(dissimilarity(
  rating, method = "conditional", which = "items"
)), 0)

## binaryRatingMatrix

x <- rbind(c(1, 0), c(0, 1), c(1, 1), c(1, 0))
x
rating <- as(matrix(x, ncol = 2), "binaryRatingMatrix")
rating

# Cosine
ibcf <- Recommender(rating,
  method = 'IBCF',
  parameter = list(method = 'cosine',
    k = 1))

d <- 0.5917517
expect_equal(compn(as(ibcf@model$sim, "matrix")[1, 2]), compn(sim2sim(1 -
    d)))
# same as: 1 - proxy::dist(t(x), method = "cosine")

expect_equal(compn(dissimilarity(
  rating, method = "cosine", which = "items"
)), compn(d))
expect_equal(compn(similarity(
  rating, method = "cosine", which = "items"
)), compn(sim2sim(1 - d)))

# this is between users
d <- c(1.000, 0.293, 0.000, 0.293, 1.000, 0.293)
expect_equal(compn(dissimilarity(rating, method = "cosine")), d)
expect_equal(compn(similarity(rating, method = "cosine")), compn(sim2sim(1 -
    d)))

# Others
res <- c(1.0, 0.5, 0.0, 0.5, 1.0, 0.5)
expect_equal(compn(dissimilarity(rating, method = "jaccard")), res)
expect_equal(compn(dissimilarity(
  rating, method = "jaccard", which = "items"
)), 0.75)

# only between items
# TODO: Make sure these are correct. Especially conditional!!!
#expect_equal(compn(dissimilarity(
#  rating, method = "karypis", which = "items"
#)), 0.826)
expect_equal(compn(dissimilarity(
  rating, method = "conditional", which = "items"
)), 0.667)

