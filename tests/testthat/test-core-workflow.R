test_that("rating matrices preserve missing values and expose useful counts", {
  ratings <- matrix(
    c(5, 0, NA, 4, 3, NA, 2, 5, 1, NA, 4, 0),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(paste0("u", 1:3), paste0("i", 1:4))
  )

  x <- as(ratings, "realRatingMatrix")

  expect_s4_class(x, "realRatingMatrix")
  expect_identical(as(x, "matrix"), ratings)
  expect_identical(rowCounts(x), c(u1 = 3L, u2 = 3L, u3 = 3L))
  expect_identical(nratings(x), 9L)
})

test_that("POPULAR fits a model and predicts unseen items", {
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
  model <- Recommender(x[1:4, ], method = "POPULAR")
  recommendations <- predict(model, x[5, ], n = 2)

  expect_s4_class(model, "Recommender")
  expect_s4_class(recommendations, "topNList")
  expect_length(as(recommendations, "list")[[1]], 2L)
  expect_true(all(!as(recommendations, "list")[[1]] %in% colnames(x)[!is.na(ratings[5, ])]))
})

test_that("rating prediction accuracy reports MAE, MSE, and RMSE", {
  truth <- matrix(
    c(5, 4, NA, 1),
    nrow = 2,
    byrow = TRUE
  )
  prediction <- matrix(
    c(4, 2, NA, 1),
    nrow = 2,
    byrow = TRUE
  )

  accuracy <- calcPredictionAccuracy(
    as(prediction, "realRatingMatrix"),
    as(truth, "realRatingMatrix")
  )

  expect_named(accuracy, c("RMSE", "MSE", "MAE"))
  expect_equal(unname(accuracy), c(sqrt(5 / 3), 5 / 3, 1))
})

test_that("evaluation schemes run a cross-validated recommendation workflow", {
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
  scheme <- evaluationScheme(
    x,
    method = "cross-validation",
    k = 3,
    given = -1,
    goodRating = 4
  )

  expect_s4_class(scheme, "evaluationScheme")
  given <- getData(scheme, "given", run = 1)
  expect_length(given, nrow(x) / 3)
  expect_true(all(given > 0L))
  expect_identical(unname(given), unname(rowCounts(getData(scheme, "known", run = 1))))

  results <- evaluate(
    scheme,
    "POPULAR",
    type = "topNList",
    n = c(1, 2),
    progress = FALSE
  )
  result <- getResults(results)[[1]]

  expect_s4_class(results, "evaluationResults")
  expect_equal(nrow(result), 2L)
  expect_true(all(c("TP", "FP", "FN", "TN", "n") %in% colnames(result)))
  expect_true(all(is.finite(result[, "n"])))
})
