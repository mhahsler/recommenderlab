library("testthat")
library("recommenderlab")

## Evaluate top-N list for binary recommender

context("Evaluate top-N list for binary recommender")

data(MSWeb)
MSWeb10 <- sample(MSWeb[rowCounts(MSWeb) > 10,], 50)

set.seed(1234)

given <- 3

e <- evaluationScheme(
  MSWeb10,
  method = "split",
  train = 0.9,
  k = 1,
  given = given
)

## create a user-based CF recommender using training data
r <- Recommender(getData(e, "train"), "UBCF")

## create predictions for the test data using known ratings (see given above)
p <- predict(r, getData(e, "known"), type = "topNList", n = 10)
p

check_predictions <- function(e, p, given) {
  acc <-
    calcPredictionAccuracy(p,
      getData(e, "unknown"),
      given = getData(e, "given"),
      byUser = TRUE)
  acc

  for (i in 1:length(p)) {
    ground <- as(getData(e, "unknown"), "matrix")[i, ]
    ground <- factor(ground, levels = c("TRUE", "FALSE"))
    sum(ground == "TRUE")

    # given_items
    given_items <- as(getData(e, "known"), "matrix")[i,]
    ground[given_items]
    sum(given_items)

    g <- getData(e, "given")[i]
    expect_equal(sum(given_items), unname(g))

    if (given > 0) {
      expect_equal(sum(given_items), given)
    }

    # predicted items (given items should have NA)
    pred <- as.logical(as(p, "matrix")[i, ])
    expect_true(all(is.na(pred[given_items])))

    pred[is.na(pred)] <- FALSE

    pred <- factor(pred, levels = c("TRUE", "FALSE"))

    ground <- ground[!given_items]
    pred <- pred[!given_items]

    tbl <- table(ground, pred)
    tbl

    comp <-
      rbind(func = acc[i,][c("TP" , "FP", "FN", "TN", "N")], test = c(as.vector(tbl), sum(tbl)))
    expect_equal(comp[1, ], comp[2, ])
  }
}

check_predictions(e, p, given)

# check evaluate with keepModel = TRUE
res <- evaluate(e, "POPULAR", progress = FALSE)
avg(res)
res <- evaluate(e, list(
  RANDOM = list(name = "RANDOM", param = NULL),
  POPULAR = list(name = "POPULAR", param = NULL),
  UBCF = list(name = "UBCF", param = NULL)
), progress = FALSE)
avg(res)

res <- evaluate(e, "POPULAR", progress = FALSE, keepModel = TRUE)
getModel(res)

res <- evaluate(
  e,
  list(
    RANDOM = list(name = "RANDOM", param = NULL),
    POPULAR = list(name = "POPULAR", param = NULL),
    UBCF = list(name = "UBCF", param = NULL)
  ),
  keepModel = TRUE,
  progress = FALSE
)
getModel(res[[1]])


## Evaluate all-but-x
set.seed(1234)
given <- -1

e <- evaluationScheme(
  MSWeb10,
  method = "split",
  train = 0.9,
  k = 1,
  given = given
)

## create a user-based CF recommender using training data
r <- Recommender(getData(e, "train"), "UBCF")

## create predictions for the test data using known ratings (see given above)
p <- predict(r, getData(e, "known"), type = "topNList", n = 10)
p

check_predictions(e, p, given)


### TODO: Test the results!


# Evaluate recommender for real-valued ratings
context("Evaluate real valued recommenders")
data(Jester5k)

## create 90/10 split (known/unknown) for the first 500 users in Jester5k
e <- evaluationScheme(
  Jester5k[1:500, ],
  method = "split",
  train = 0.9,
  k = 1,
  given = 15
)
e

## create a user-based CF recommender using training data
r <- Recommender(getData(e, "train"), "UBCF")

## create predictions for the test data using known ratings (see given above)
p <- predict(r, getData(e, "known"), type = "ratings")
p

## compute error metrics averaged per user and then averaged over all
## recommendations
calcPredictionAccuracy(p, getData(e, "unknown"))
head(calcPredictionAccuracy(p, getData(e, "unknown"), byUser = TRUE))

## evaluate topNLists instead (you need to specify given and goodRating!)
p <- predict(r, getData(e, "known"), type = "topNList")
p

calcPredictionAccuracy(p,
  getData(e, "unknown"),
  given = 15,
  goodRating = 5)
