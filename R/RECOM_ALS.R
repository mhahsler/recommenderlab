# A latent factor based recommender, solved by the
# alternating least squares algorithm
#
# Contributor: Verreet Bregt <Bregt.Verreet@infofarm.be>

# Implementation follows:
# Yunhong Zhou, Dennis Wilkinson, Robert Schreiber, Rong Pan (2008),
# Large-Scale Parallel Collaborative Filtering for the Netflix Prize
# 4th Int'l Conf. Algorithmic Aspects in Information and Management, LNCS 5034
# http://link.springer.com/chapter/10.1007/978-3-540-68880-8_32

### NOTES:
# - The implementation is a basic implementation of the algorithm, for a fixed (chosen) number of iterations and without parallellization.
# - Speed is slower than your other algorithms, but it seems workable.
# - The actual model construction actually only happens when you call "predict".  This is a disadvantage of the ALS algorithm, where you actually can only train the model once you have data about the test users.
# - When you set verbose = TRUE, you will see the converge of the error function printed.
# - The algorithm allows low rmse errors on test data .  For topN predictions, the calculated TPRs are lower than for the POPULAR algorithm though. It seems that in this case, topN movies that are not in the test set (not seen) are marked as false positives. ALS will then  recommend  movies that are not often seen, but where similar people gave good ratings.  While marking NAs as false positives is a reasonable way to handle NAs, it perhaps might be interesting to also provide the option to simply ignore NAs in that calculation.

.REAL_ALS_params <- list(
  normalize = NULL,
  lambda = 0.1,
  n_factors = 10,
  n_iterations = 10,
  min_item_nr = 1,
  seed = NULL
)

#################
# ALS algorithm
#################


# Cost function calculation
cost_function <- function (R, U, M, W, lambda, n_u_i, n_m_j) {
  sum(W * ((R - (U %*% M)) ^ 2)) + lambda * (sum(n_u_i %*% (U ^ 2)) + sum((M ^
      2) %*% n_m_j))
}


REAL_ALS <- function(data, parameter = NULL) {
  p <- getParameters(.REAL_ALS_params, parameter)

  # Here ALS differs from other models: you actually need the data in newdata before you can start constructing your model
  # Hence, the real model construction is put in predict

  model <- c(list(data = data), p)

  predict <- function(model,
    newdata,
    n = 10,
    data = NULL,
    type = c("topNList", "ratings", "ratingMatrix"),
    ...) {
    # Take first value from type
    type <- match.arg(arg = type)  # type <- "topNList"

    # Error messages

    if (is.numeric(newdata)) {
      if (is.null(data) || !is(data, "ratingMatrix"))
        stop("If newdata is a user id then data needs to be the training dataset.")
      newdata <- data[newdata, ]
    }

    if (ncol(newdata) != ncol(model$data@data))
      stop("number of items in newdata does not match model.")

    ##########################
    # Model construction
    ##########################

    # Use both data and newdata to train your model
    # Therefore, the data must first be combined
    #data <- combine_data(model$data@data, newdata@data)
    data <- new("realRatingMatrix",
      data = rbind(as(model$data, "dgCMatrix"), as(newdata, "dgCMatrix")))

    # Normalize the data
    if (!is.null(p$normalize) && is(data, "realRatingMatrix")) {
      normalized_data <- normalize(data, method = p$normalize)
    } else {
      normalized_data <- data
    }


    # The weight matrix W gives a weight 0 (NA) to missing data, and weight 1 to measured data (called I in the paper)
    W <- normalized_data@data
    W@x[!is.na(W@x)] <- 1 # Check this code for non- dgCMatrix
    # W <- dropNA(W)

    # R holds the ratings
    R <- normalized_data@data


    # The matrix dimensions
    n_u            <- dim(R)[1]
    n_m            <- dim(R)[2]

    # Initialize M (movies/ items) with small randomly fluctuating values
    delta_ <- 0.0001
    if (length(p$seed) == 1)
      set.seed(p$seed)
    M <-
      delta_ * matrix(runif(n_m * p$n_factors),
        nrow = p$n_factors,
        ncol = n_m)
    colnames(M) <- colnames(normalized_data)
    # But the first row is initialiazed as the average rating of that movie
    M[1, ] <-
      colSums(R, na.rm = TRUE) / colSums(W) # colMeans() would consider empty spaces in a dgCMatrix as zeroes
    mean_rating <- mean(M[1, ], na.rm = TRUE)
    M[1, ][is.na(M[1, ])] <- mean_rating

    # For U (users) , we initialize with 1s and zeroes, although it does not really matter,
    # because U will be overwritten in the first phase of the loop

    U <- matrix(0, nrow = n_u, ncol = p$n_factors)
    rownames(U) <- rownames(normalized_data)
    U[, 1] <- 1

    # Replace the NAs in rating matrix R with zeroes
    if (class(R) != "dgCMatrix") {
      R <- dropNA(R)
    }


    # The number of ratings for each user
    n_u_i <- rowSums(W)
    # And, the number of ratings for each item
    n_m_j <- colSums(W)



    # Print the cost function

    if (p$verbose == TRUE) {
      cost <- cost_function(R, U, M, W, lambda = p$lambda, n_u_i, n_m_j)
      print(paste0("0th iteration: cost function = ", cost))
    }

    # This loop will try to get U %*% M close to R, with W providing weights for the error calculation

    for (kk in 1:p$n_iterations) {
      # Minimize U %*% M for fixed M, by iterating over m users
      for (ii in 1:n_u) {
        # First drop the M columns and R rows irrelevant for user ii, in order to speed up the calculation
        M_selected <- M[, W[ii,] == 1, drop = FALSE]
        R_selected <- R[ii,][W[ii,] == 1, drop = FALSE]
        # Update U for user ii
        U[ii,] <-
          t(solve(
            M_selected %*% t(M_selected) + p$lambda * n_u_i[ii] * diag(p$n_factors),
            (M_selected %*% R_selected)
          ))
      }
      if (p$verbose == TRUE) {
        cost <- cost_function(R, U, M, W, lambda = p$lambda, n_u_i, n_m_j)
        print(paste0(kk, "th iteration, step 1: cost function = ", cost))
      }
      # Minimize U %*% M for fixed U, by iterating over n items
      for (jj in 1:n_m) {
        if (sum(W[, jj] == 1) > 0) {
          U_selected <- U[W[, jj] == 1, , drop = FALSE]
          R_col_selected <- R[, jj][W[, jj] == 1, drop = FALSE]
          M[, jj] <-
            solve(
              t(U_selected) %*% U_selected + p$lambda * n_m_j[jj] * diag(p$n_factors),
              t(U_selected) %*% R_col_selected
            )
        }
      }
      if (p$verbose == TRUE) {
        cost <- cost_function(R, U, M, W, lambda = p$lambda, n_u_i, n_m_j)
        print(paste0(kk, "th iteration, step 2: cost function = ", cost))
      }
    }

    ratings <- new(
      "realRatingMatrix",
      data = as(U %*% M, "dgCMatrix"),
      normalize = data@normalize
    )
    ratings <- denormalize(ratings)

    ##################################################
    # Here we say that the predicted rating is only used if the item was at least min_item_nr.
    # (The default min_item_nr is 1)
    # Otherwise, use the average rating over all movies
    to_replace              <- colSums(W) < p$min_item_nr
    if (sum(to_replace) > 0) {
      col_means             <-
        colSums(data@data, na.rm = TRUE) / colSums(W) # colMeans() would consider empty spaces in a dgCMatrix as zeroes
      mean_rating           <- mean(col_means, na.rm = TRUE)
      ratings               <- ratings@data
      ratings[, to_replace] <- mean_rating
    }
    ratings <- as(ratings, "realRatingMatrix")

    # During the model construction above, a rating was calculated for each user-item combination
    # Here, it is just a matter of returning the ratings associated with the users in newdata
    ratingMatrix <- ratings[-(1:nrow(model$data)), ]

    # Now return the ratings, as a "topNList", "ratings" or "ratingMatrix"
    returnRatings(ratingMatrix, newdata, type, n)

  }

  ## construct recommender object
  new(
    "Recommender",
    method = "ALS",
    dataType = class(data),
    ntrain = nrow(data),
    model = model,
    predict = predict
  )
}


## register recommender
recommenderRegistry$set_entry(
  method = "ALS",
  dataType = "realRatingMatrix",
  fun = REAL_ALS,
  description = "Recommender for explicit ratings based on latent factors, calculated by alternating least squares algorithm.",
  reference = "Yunhong Zhou, Dennis Wilkinson, Robert Schreiber, Rong Pan (2008). Large-Scale Parallel Collaborative Filtering for the Netflix Prize, 4th Int'l Conf. Algorithmic Aspects in Information and Management, LNCS 5034.",
  parameters = .REAL_ALS_params
)



# ALS for implicit data
#
# Yifan Hu, Yehuda Koren, Chris Volinsky (2008),
# Collaborative Filtering for Implicit Feedback Datasets,
# ICDM '08 Proceedings of the 2008 Eighth IEEE International Conference
# on Data Mining, Pages 263-272


.REAL_ALS_implicit_params <- list(
  lambda = 0.1,
  alpha = 10,
  n_factors = 10,
  n_iterations = 10,
  min_item_nr = 1,
  seed = NULL
)


# Cost function calculation
cost_function_implicit <-
  function (R, U, M, W, lambda, n_u_i, n_m_j) {
    sum((W + 1) * (R - (U %*% M)) ^ 2) + lambda * (sum(n_u_i %*% (U ^ 2)) + sum((M ^
        2) %*% n_m_j))
  }


## Currently, no normalization is foreseen
REAL_ALS_implicit <- function(data, parameter = NULL) {
  p <- getParameters(.REAL_ALS_implicit_params, parameter)

  # Here ALS differs from other models: you actually need the data in newdata before you can start constructing your model
  # Hence, the real model construction is put in predict

  model <- c(list(data = data), p)

  predict <- function(model,
    newdata,
    n = 10,
    data = NULL,
    type = c("topNList", "ratings", "ratingMatrix"),
    ...) {
    # The same function used for realRatingMatrices is also used for binaryRatingMatrices
    if (class(model$data) == "binaryRatingMatrix") {
      model$data <- as(as(model$data, "dgCMatrix"), "realRatingMatrix")
    }
    if (class(newdata) == "binaryRatingMatrix") {
      newdata <- as(as(newdata, "dgCMatrix"), "realRatingMatrix")
    }

    # Take first value from type
    type <- match.arg(arg = type)  # type <- "topNList"


    # Error messages

    if (is.numeric(newdata)) {
      if (is.null(data) || !is(data, "ratingMatrix"))
        stop("If newdata is a user id then data needs to be the training dataset.")
      newdata <- data[newdata, ]
    }

    if (ncol(newdata) != ncol(model$data@data))
      stop("number of items in newdata does not match model.")


    ##########################
    # Model construction
    ##########################

    # Use both data and newdata to train your model
    # Therefore, the data must first be combined
    #data <- combine_data(model$data@data, newdata@data)
    data <- rbind(as(model$data, "dgCMatrix"), as(newdata, "dgCMatrix"))

    ### MFH: There is no NA in implicit data!
    # The rating matrix R assigns 0 (NA) to missing data, and 1 to measured data
    R <- data
    R@x[!is.na(R@x)] <- 1

    # The numbers in data are used to assign weights to these ratings (confidence)
    # Here, the weights w_ui = 1 + a r_ui are used.
    # However, we just save it as a r_ui, to keep it sparse. We add the 1 back to it whenever needed
    W <- p$alpha * data


    # The matrix dimensions
    n_u            <- dim(R)[1]
    n_m            <- dim(R)[2]

    # Initialize M (movies/ items) with small randomly fluctuating values
    delta_ <- 0.0001
    if (length(p$seed) == 1)
      set.seed(p$seed)
    M <-
      delta_ * matrix(runif(n_m * p$n_factors),
        nrow = p$n_factors,
        ncol = n_m)
    colnames(M) <- colnames(data)
    # But the first row is initialiazed as the average rating of that movie
    M[1, ] <-
      colSums(R * (W + 1), na.rm = TRUE) / colSums(W + 1) # colMeans() would consider empty spaces in a dgCMatrix as zeroes
    mean_rating <- mean(M[1, ], na.rm = TRUE)
    M[1, ][is.na(M[1, ])] <- mean_rating

    # For U (users) , we initialize with 1s and zeroes, although it does not really matter,
    # because U will be overwritten in the first phase of the loop

    U <- matrix(0, nrow = n_u, ncol = p$n_factors)
    rownames(U) <- rownames(data)
    U[, 1] <- 1


    # The number of ratings for each user
    n_u_i <- rowSums(R)
    # And, the number of ratings for each item
    n_m_j <- colSums(R)

    # Print the cost function
    if (p$verbose == TRUE) {
      cost <-
        cost_function_implicit(R, U, M, W, lambda = p$lambda, n_u_i, n_m_j)
      print(paste0("0th iteration: cost function = ", cost))
    }



    # This loop will try to get U %*% M close to R, with W providing weights for the error calculation

    for (kk in 1:p$n_iterations) {
      # Minimize U %*% M for fixed M, by iterating over m users
      # Precalculate M %*% t(M)
      M_x_t_M <- M %*% t(M)
      for (ii in 1:n_u) {
        # First drop the M columns and R rows irrelevant for user ii, in order to speed up the calculation
        M_selected <- M[, R[ii,] != 0, drop = FALSE]
        W_selected <- W[ii,][R[ii,] != 0]
        R_selected <-
          R[ii,][R[ii,] != 0] # Because of the way R is defined, this is just gonna be a vector full of ones
        # t(t(M_selected) * W_selected) is faster for big matrices than M_selected %*% diag(W_selected)
        M_selected_weighed <- t(t(M_selected) * W_selected)
        # Update U for user ii
        U[ii,] <-
          t(solve(
            M_x_t_M + M_selected_weighed %*% t(M_selected) + p$lambda * n_u_i[ii] * diag(p$n_factors),
            ((M_selected_weighed + M_selected) %*% R_selected)
          ))
      }
      if (p$verbose == TRUE) {
        cost <-
          cost_function_implicit(R, U, M, W, lambda = p$lambda, n_u_i, n_m_j)
        print(paste0(kk, "th iteration, step 1: cost function = ", cost))
      }
      # Minimize U %*% M for fixed U, by iterating over n items
      # Precalculate t(U) %*% U
      t_U_x_U <- t(U) %*% U
      for (jj in 1:n_m) {
        if (sum(W[, jj] != 0) > 0) {
          U_selected <- U[R[, jj] != 0, , drop = FALSE]
          W_col_selected <- W[, jj][R[, jj] != 0]
          R_col_selected <- R[, jj][R[, jj] != 0]
          t_U_selected_weighed <- t(W_col_selected * U_selected)
          M[, jj] <-
            solve(
              t_U_x_U + t_U_selected_weighed %*% U_selected + p$lambda * n_m_j[jj] * diag(p$n_factors),
              (t_U_selected_weighed + t(U_selected)) %*% R_col_selected
            )
        }
      }
      if (p$verbose == TRUE) {
        cost <-
          cost_function_implicit(R, U, M, W, lambda = p$lambda, n_u_i, n_m_j)
        print(paste0(kk, "th iteration, step 2: cost function = ", cost))
      }
    }



    ratings <- new("realRatingMatrix",
      data = as(U %*% M, "dgCMatrix"))

    ##################################################
    # Here we say that the predicted rating is only used if the item was at least min_item_nr.
    # (The default min_item_nr is 1)
    # Otherwise, set the predicted rating on 0
    to_replace              <- colSums(W) < p$min_item_nr
    if (sum(to_replace) > 0) {
      ratings               <- ratings@data
      ratings[, to_replace] <- 0
    }
    ratings <- as(ratings, "realRatingMatrix")

    # During the model construction above, a rating was calculated for each user-item combination
    # Here, it is just a matter of returning the ratings associated with the users in newdata
    ratingMatrix <- ratings[-(1:nrow(model$data)), ]

    # Now return the ratings, as a "topNList", "ratings" or "ratingMatrix"
    returnRatings(ratingMatrix, newdata, type, n)

  }

  ## construct recommender object
  new(
    "Recommender",
    method = "ALS",
    dataType = class(data),
    ntrain = nrow(data),
    model = model,
    predict = predict
  )
}



## register recommenders
recommenderRegistry$set_entry(
  method = "ALS_implicit",
  dataType = "realRatingMatrix",
  fun = REAL_ALS_implicit,
  description = "Recommender for implicit data based on latent factors, calculated by alternating least squares algorithm.",
  reference = "Yifan Hu, Yehuda Koren, Chris Volinsky (2008). Collaborative Filtering for Implicit Feedback Datasets, ICDM '08 Proceedings of the 2008 Eighth IEEE International Conference on Data Mining, pages 263-272.",
  parameters = .REAL_ALS_implicit_params
)

recommenderRegistry$set_entry(
  method = "ALS_implicit",
  dataType = "binaryRatingMatrix",
  fun = REAL_ALS_implicit,
  description = "Recommender for implicit data based on latent factors, calculated by alternating least squares algorithm.",
  reference = "Yifan Hu, Yehuda Koren, Chris Volinsky (2008). Collaborative Filtering for Implicit Feedback Datasets, ICDM '08 Proceedings of the 2008 Eighth IEEE International Conference on Data Mining, pages 263-272.",
  parameters = .REAL_ALS_implicit_params
)
