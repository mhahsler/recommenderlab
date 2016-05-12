### adapted from package rrecsys by Ludovik Coba and Markus Zanker

funkSVD <- function(x, k = 10, gamma = 0.015, lambda = 0.001,
  min_improvement = 1e-6, min_epochs = 50, max_epochs = 200, verbose = FALSE) {

  x <- as(x, "matrix")

  if (ncol(x) < k || nrow(x) < k)
    stop("k needs to be smaller than the number of users or items.")

  # initilize the user-feature and item-feature matrix
  U <- matrix(0.1, nrow = nrow(x), ncol = k)
  V <- matrix(0.1, nrow = ncol(x), ncol = k)

  #list of indices pointing to ratings on each item
  itemIDX <- lapply(1:nrow(x), function(temp) which(!is.na(x[temp, ])))
  #list of indices pointing to ratings on each user
  userIDX <- lapply(1:ncol(x), function(temp) which(!is.na(x[, temp])))

  # go through all features
  for (f in 1:k) {
    if(verbose) cat("\nTraining feature:", f, "/", k, ": ")

    # convergence check
    last_error <- Inf
    delta_error <- Inf
    epoch <- 0L
    p <- tcrossprod(U, V)

    while (epoch < min_epochs || (epoch < max_epochs &&
        delta_error > min_improvement)) {

      # update user features
      error <- x - p
      temp_U <- U
      for (j in 1:ncol(x)) {
        delta_Uik <- lambda * (error[userIDX[[j]], j] * V[j, f] -
            gamma * U[userIDX[[j]], f])
        U[userIDX[[j]], f] <- U[userIDX[[j]], f] + delta_Uik
      }

      # update item features
      for (i in 1:nrow(x)) {
        delta_Vjk <- lambda * (error[i, itemIDX[[i]]] * temp_U[i, f] -
            gamma * V[itemIDX[[i]], f])
        V[itemIDX[[i]], f] <- V[itemIDX[[i]], f] + delta_Vjk
      }

      ### update error
      p <- tcrossprod(U, V)
      new_error <- sqrt(sum(abs(x - p)^2, na.rm = TRUE)/length(x))
      delta_error <- abs(last_error - new_error)

      last_error <- new_error
      epoch <- epoch + 1L
      if(verbose) cat(".")
  #    if(verbose) cat("\nimprovement:", delta_error)
    }

      if(verbose) cat("\n-> ", epoch, "epochs - final improvement was",
        delta_error, "\n")
  }

  structure(list(U = U, V = V, parameters =
      list(k = k, gamma = gamma, lambda = lambda,
        min_epochs = min_epochs, max_epochs = max_epochs,
        min_improvement = min_improvement)),
    class = "funkSVD")
}

predict.funkSVD <- function(object, newdata, verbose = FALSE, ...) {

  V <- object$V

  k <- object$parameters$k
  gamma <- object$parameters$gamma
  lambda <- object$parameters$lambda
  min_epochs <- object$parameters$min_epochs
  max_epochs <- object$parameters$max_epochs
  min_improvement <- object$parameters$min_improvement

  if(is.vector(newdata)) x <- rbind(newdata)
  else x <- as(newdata, "matrix")

  u <- matrix(0.1, nrow = nrow(x), ncol = k)

  #list of indices pointing to ratings on each item
  itemIDX <- lapply(1:nrow(x), function(temp) which(!is.na(x[temp, ])))

  #list of indices pointing to ratings on each user
  userIDX <- lapply(1:ncol(x), function(temp) which(!is.na(x[, temp])))

  # go through all features
  for (f in 1:k) {
    if(verbose) cat("\nEstimating user feature:", f, "/", k, ": ")

    # convergence check
    last_error <- Inf
    delta_error <- Inf
    epoch <- 0L
    p <- tcrossprod(u, V)

    while (epoch < min_epochs || (epoch < max_epochs &&
        delta_error > min_improvement)) {

      error <- x - p

      # update user features
      for (j in 1:ncol(x)) {
        delta_Uik <- lambda * (error[userIDX[[j]], j] * V[j, f] -
            gamma * u[userIDX[[j]], f])

        u[userIDX[[j]], f] <- u[userIDX[[j]], f] + delta_Uik
      }

      ### update error
      p <- tcrossprod(u, V)
      new_error <- sqrt(sum(abs(x - p)^2, na.rm = TRUE)/length(x))
      delta_error <- abs(last_error - new_error)
      last_error <- new_error
      epoch <- epoch +1L

      if(verbose) cat(".")
 #     if(verbose) cat("\nimprovement:", delta_error)
    }

    if(verbose) cat("\n-> ", epoch, "epochs - final improvement was",
        delta_error, "\n")

  }

  p <- tcrossprod(u, V)
  dimnames(p) <- dimnames(x)
  attr(p, "u") <- u

  p
}
