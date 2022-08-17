## Item-Based Collaborative Filtering
## Top-N recomender (see Karypis 2001)

.BIN_IBCF_params <- list(
  k = 30,
  method = "Jaccard",
  normalize_sim_matrix = FALSE,
  alpha = 0.5
)

BIN_IBCF <- function(data, parameter = NULL) {
  p <- getParameters(.BIN_IBCF_params, parameter)

  ## this might not fit into memory! Maybe use a sample?
  sim <- as.matrix(similarity(
    data,
    method = p$method,
    which = "items",
    args = list(alpha = p$alpha)
  ))

  ## reduce similarity matrix to keep only the k highest similarities
  diag(sim) <- 0
  ##sim[!is.finite(sim)] <- 0

  ## normalize rows to 1
  if (p$normalize_sim_matrix)
    sim <- sim / rowSums(sim, na.rm = TRUE)

  for (i in 1:nrow(sim))
    sim[i, head(order(sim[i, ], decreasing = FALSE, na.last = FALSE),
      ncol(sim) - p$k)] <- 0

  ## make sparse
  as(drop0(sim), "generalMatrix")

  model <- c(list(description = "IBCF: Reduced similarity matrix",
    sim = sim),
    p)

  predict <- function(model,
    newdata,
    n = 10,
    data = NULL,
    type = c("topNList", "ratings", "ratingMatrix"),
    ...) {
    type <- match.arg(type)

    ## newdata are userid
    if (is.numeric(newdata)) {
      if (is.null(data) || !is(data, "ratingMatrix"))
        stop("If newdata is a user id then data needes to be the training dataset.")
      newdata <- data[newdata, ]
    }

    if (ncol(newdata) != nrow(model$sim))
      stop("number of items in newdata does not match model.")

    n <- as.integer(n)
    sim <- model$sim
    u <- as(newdata, "dgCMatrix")

    ## predict all ratings (average similarity)
    #ratings <- tcrossprod(sim,u)
    ratings <-
      #t(as(tcrossprod(sim, u) / tcrossprod(sim != 0, u != 0), "matrix"))
      t(as(tcrossprod(sim, u), "matrix") / as(tcrossprod(sim != 0, u != 0), "matrix"))
    dimnames(ratings) <- dimnames(newdata)

    returnRatings(ratings, newdata, type, n)
  }

  ## construct recommender object
  new(
    "Recommender",
    method = "IBCF",
    dataType = class(data),
    ntrain = nrow(data),
    model = model,
    predict = predict
  )

}

## register recommender
recommenderRegistry$set_entry(
  method = "IBCF",
  dataType = "binaryRatingMatrix",
  fun = BIN_IBCF,
  description = "Recommender based on item-based collaborative filtering (binary rating data).",
  parameters = .BIN_IBCF_params
)


.REAL_IBCF_params <- list(
  k = 30,
  method = "cosine",
  normalize = "center",
  normalize_sim_matrix = FALSE,
  alpha = 0.5,
  na_as_zero = FALSE
)


REAL_IBCF <- function(data, parameter = NULL) {
  p <- getParameters(.REAL_IBCF_params, parameter)

  if (!is.null(p$normalize))
    data <- normalize(data, method = p$normalize)

  ## this might not fit into memory! Maybe use a sample?
  sim <- as.matrix(similarity(
    data,
    method = p$method,
    which = "items",
    args = list(alpha = p$alpha, na_as_zero = p$na_as_zero)
  ))

  diag(sim) <- NA

  ## normalize rows to sum up to 1 (abs is used for similarities that can be neg.)
  if (p$normalize_sim_matrix) {
    sim <- sim / rowSums(abs(sim), na.rm = TRUE)
  }

  ## reduce similarity matrix to keep only the k highest similarities
  ##sim[!is.finite(sim)] <- NA
  for (i in 1:nrow(sim))
    sim[i, head(order(sim[i, ], decreasing = FALSE, na.last = FALSE),
      ncol(sim) - p$k)] <- NA

  ## make sparse NA -> 0. Having an actual 0 in the reduced matrix is very unlikely.
  sim <- dropNA(sim)

  model <- c(list(description = "IBCF: Reduced similarity matrix",
    sim = sim),
    p)

  predict <- function(model,
    newdata,
    n = 10,
    data = NULL,
    type = c("topNList", "ratings", "ratingMatrix"),
    ...) {
    type <- match.arg(type)

    ## newdata are userid
    if (is.numeric(newdata)) {
      if (is.null(data) || !is(data, "ratingMatrix"))
        stop("If newdata is a user id then data needes to be the training dataset.")
      newdata <- data[newdata, ]
    }

    if (ncol(newdata) != nrow(model$sim))
      stop("number of items in newdata does not match model.")

    n <- as.integer(n)

    if (!is.null(model$normalize))
      newdata <- normalize(newdata, method = model$normalize)

    ## predict all ratings
    sim <- model$sim
    u <- as(newdata, "dgCMatrix")

    ratings <-
      #t(as(tcrossprod(sim, u) / tcrossprod(sim, u != 0), "matrix"))
      t(as(tcrossprod(sim, u), "matrix") / as(tcrossprod(sim, u != 0), "matrix"))

    ratings <- new("realRatingMatrix",
      data = dropNA(ratings),
      normalize = getNormalize(newdata))
    ratings <- denormalize(ratings)

    returnRatings(ratings, newdata, type, n)
  }

  ## construct recommender object
  new(
    "Recommender",
    method = "IBCF",
    dataType = class(data),
    ntrain = nrow(data),
    model = model,
    predict = predict
  )
}

## register recommender
recommenderRegistry$set_entry(
  method = "IBCF",
  dataType = "realRatingMatrix",
  fun = REAL_IBCF,
  description = "Recommender based on item-based collaborative filtering.",
  parameters = .REAL_IBCF_params
)
