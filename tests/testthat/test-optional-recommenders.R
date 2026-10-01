test_that("optional recommender dependencies report how to install them", {
  expect_error(
    recommenderlab:::.require_recommender_package("missing_recommender_package", "SVD"),
    "Recommender method 'SVD' requires package 'missing_recommender_package'. Install it with install.packages\\('missing_recommender_package'\\)."
  )
})

test_that("optional methods check their dependencies when selected", {
  data("MovieLense")
  train <- MovieLense[1:20, ]

  local_mocked_bindings(
    .require_recommender_package = function(package, method) {
      stop(sprintf("checked %s for %s", package, method))
    },
    .package = "recommenderlab"
  )

  expect_error(Recommender(train, method = "SVD"), "checked irlba for SVD")
  expect_error(Recommender(train, method = "LIBMF"), "checked recosystem for LIBMF")
})
