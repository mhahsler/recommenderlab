test_that("all-but-one withholds exactly one observed rating per user", {
  ratings <- matrix(
    c(5, 4, 3, NA, NA,
      4, 5, NA, 3, NA,
      5, NA, 4, 3, NA,
      4, 3, 5, NA, NA,
      3, 5, 4, NA, NA,
      5, 3, 4, NA, NA),
    nrow = 6,
    byrow = TRUE,
    dimnames = list(paste0("u", 1:6), paste0("i", 1:5))
  )
  x <- as(ratings, "realRatingMatrix")

  set.seed(1234)
  scheme <- evaluationScheme(x, method = "split", train = 0.5,
    given = -1)
  known <- as(scheme@knownData, "matrix")
  unknown <- as(scheme@unknownData, "matrix")

  expect_identical(unname(rowCounts(scheme@knownData)), rep(2L, 6))
  expect_identical(unname(rowCounts(scheme@unknownData)), rep(1L, 6))
  expect_true(all(is.na(known) | is.na(unknown)))

  reconstructed <- known
  reconstructed[is.na(reconstructed)] <- unknown[is.na(reconstructed)]
  expect_identical(reconstructed, ratings)
})

test_that("positive given preserves observed zero ratings and user counts", {
  ratings <- matrix(
    c(0, 1, 2, NA, NA,
      3, NA, 4, 5, NA,
      NA, 2, 0, 3, 4,
      1, NA, NA, 5, 0),
    nrow = 4,
    byrow = TRUE,
    dimnames = list(paste0("u", 1:4), paste0("i", 1:5))
  )
  x <- as(ratings, "realRatingMatrix")

  set.seed(1234)
  scheme <- evaluationScheme(x, method = "split", train = 0.5,
    given = 2)
  known <- as(scheme@knownData, "matrix")
  unknown <- as(scheme@unknownData, "matrix")

  expect_identical(unname(rowCounts(scheme@knownData)), rep(2L, 4))
  expect_identical(unname(rowCounts(scheme@unknownData)), c(1L, 1L, 2L, 1L))
  expect_true(all(is.na(known) | is.na(unknown)))

  reconstructed <- known
  reconstructed[is.na(reconstructed)] <- unknown[is.na(reconstructed)]
  expect_identical(reconstructed, ratings)
})

test_that("cross-validation tests each user exactly once when folds are uneven", {
  ratings <- matrix(
    rep(c(1, 2, 3), 7),
    nrow = 7,
    byrow = TRUE,
    dimnames = list(paste0("u", 1:7), paste0("i", 1:3))
  )
  x <- as(ratings, "realRatingMatrix")

  set.seed(1234)
  scheme <- evaluationScheme(x, method = "cross-validation", k = 3,
    given = 1)
  test_users <- lapply(seq_len(3), function(run)
    rownames(getData(scheme, "known", run = run)))
  counts <- table(factor(unlist(test_users), levels = rownames(x)))

  expect_identical(as.integer(counts), rep(1L, nrow(x)))
  expect_lte(diff(range(lengths(test_users))), 1L)
})
