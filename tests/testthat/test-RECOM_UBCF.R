library("testthat")
library("recommenderlab")

### Example from vignette
db <- rbind(
  c(NA,  4,  4,  2,  1,  2, NA, NA),
  c( 3, NA, NA, NA,  5,  1, NA, NA),
  c( 3, NA, NA,  3,  2,  2, NA,  3),
  c( 4, NA, NA,  2,  1,  1,  2,  4),
  c( 1,  1, NA, NA, NA, NA, NA,  1),
  c(NA,  1, NA, NA,  1,  1, NA,  1)
)
dimnames(db) <- list(paste0("u", 1:6), paste0("i", 1:8))

u_a <- rbind(
  c(NA, NA,  4,  3, NA,  1, NA,  5)
)
dimnames(u_a) <- list("u_a", paste0("i", 1:8))

r_db <- as(db, "realRatingMatrix")
r_a <- as(u_a, "realRatingMatrix")


sim <- similarity(r_db, r_a, method = "Euclidean")
expect_equivalent(sim, 1/(1+dist(db, u_a)))

### users 1,2 and 4 are the 3 nearest neighbors!
sim

rec <- Recommender(r_db, method = "UBCF",
  param = list(nn = 3, weighted = FALSE, normalize = NULL, method = "Euclidean"))
pred <- predict(rec, r_a, type = "ratings")

as(pred, "matrix")

expect_equivalent(as(pred, "matrix"), c(7/2, 4, NA, NA, 7/3, NA, 2, NA))

### use normalization
# center
rec <- Recommender(r_db, method = "UBCF",
  param = list(nn = 3, weighted = FALSE, normalize = "center", method = "Euclidean"))

# neighborhood is now u1, u2, u4
similarity(getModel(rec)$data,
  normalize(r_a, method = getModel(rec)$normalize), method = getModel(rec)$method)

pred <- predict(rec, r_a, type = "ratings")
as(pred, "matrix")
### FIXME: Test the results
#expect_equivalent(as(pred, "matrix"), ???)

# z-score
rec <- Recommender(r_db, method = "UBCF",
  param = list(nn = 3, weighted = FALSE, normalize = "z-score", method = "Euclidean"))

# neighborhood is now u2, u3, u4
similarity(getModel(rec)$data,
  normalize(r_a, method = getModel(rec)$normalize), method = getModel(rec)$method)

pred <- predict(rec, r_a, type = "ratings")
as(pred, "matrix")
### FIXME: Test the results
#expect_equivalent(as(pred, "matrix"), ???)

# Note: no user in the neighborhood has a rating for i2 -> NA
# here is a way to fix this with a HybridRecommender
rec_pop <- Recommender(r_db, method = "POPULAR")
pred_pop <- predict(rec_pop, r_a, type = "ratings")
as(pred_pop, "matrix")

hybrid <- HybridRecommender(rec, rec_pop, weights = c(0.999, 0.001))
pred3 <- predict(hybrid, r_a, type = "ratings")
as(pred3, "matrix")

### use default settings (weighted, normalization, and Cosine)
rec <- Recommender(r_db, method = "UBCF",
  param = list(nn = 3))

# neighborhood is now u2, u3, u4
similarity(getModel(rec)$data,
  normalize(r_a, method = getModel(rec)$normalize), method = getModel(rec)$method)

pred <- predict(rec, r_a, type = "ratings")
as(pred, "matrix")
### FIXME: Test the results
#expect_equivalent(as(pred, "matrix"), ???)

### use userID for prediction
rec <- Recommender(r_db, method = "UBCF",
  param = list(nn = 1, normalize = NULL, weighted = FALSE))

# User 5 is the most similar
similarity(getModel(rec)$data,
  normalize(r_db[1,], method = getModel(rec)$normalize), method = getModel(rec)$method)

pred <- predict(rec, 1, type = "ratings")
as(pred, "matrix")
expect_equivalent(as(pred, "matrix")[is.na(as(r_db[1,], "matrix"))],
  as(r_db[5,], "matrix")[is.na(as(r_db[1,], "matrix"))])

