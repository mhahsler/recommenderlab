##*******************************************************************
## dissimilarity for binaryRatingMatrix


setMethod("dissimilarity", signature(x = "binaryRatingMatrix"),
  function(x,
    y = NULL,
    method = NULL,
    args = NULL,
    which = "users") {
    args <- getParameters(list(alpha = .5), args)

    which <- match.arg(tolower(which), c("users", "items"))

    if (!is.null(method))
      method <- tolower(method)
    else
      method <- "jaccard"


    ## handle Karypis and conditional dissimilarities
    if (method == "karypis") {
      if (!is.null(y) ||
          which != "items")
        stop(
          "Kaypis dissimilarities are not implemented between users or as a cross-dissimilarity!"
        )

      return(.karypis(as(x, "dgCMatrix"), dist = TRUE, args))
    }

    if (method == "conditional") {
      if (!is.null(y) ||
          which != "items")
        stop(
          "Conditional dissimilarities are not implemented between users or as a cross-dissimilarity!"
        )

      return(.conditional(as(x, "dgCMatrix"), dist = TRUE, args))
    }


    ## dissimilarity is defined in arules for itemMatrix
    if (which == "users")
      which <- "transactions" ## "items" is OK
    x <- x@data
    if (!is.null(y))
      y <- y@data

    ## dissimilarity in arules sets the method attribute
    d <- arules::dissimilarity(x, y, method, args, which)

    ## cross dissimilarity?
    if (!is.null(y))
      d <- structure(as(d, "matrix"), method = method)
    d
  })



##*******************************************************************
## wrapper for realRatingMatrix (transactions)
## Idea by Christopher Koeb

setMethod("dissimilarity", signature(x = "realRatingMatrix"),
  function(x,
    y = NULL,
    method = NULL,
    args = NULL,
    which = "users") {
    args <- getParameters(list(na_as_zero = FALSE, alpha = .5), args)

    which <- match.arg(tolower(which), c("users", "items"))

    if (!is.null(method))
      method <- tolower(method)
    else
      method <- "cosine"

    ### FIX this code!
    ## shortcut for Cosine (compute sparse)
    #if(method=="cosine" && is.null(y)) {
    #	x <- as(x, "dgCMatrix")
    #	return(as.dist(1- crossprod(x / sqrt(rowSums(x ^ 2)))))
    #}

    ## handle karypis and conditional dissimilarities
    if (method == "karypis") {
      if (!is.null(y) ||
          which != "items")
        stop(
          "Kaypis dissimilarities are not implemented between users or as a cross-dissimilarity!"
        )

      return(.karypis(as(x, "dgCMatrix"), dist = TRUE, args))
    }

    if (method == "conditional") {
      if (!is.null(y) ||
          which != "items")
        stop(
          "Conditional dissimilarities are not implemented between users or as a cross-dissimilarity!"
        )

      return(.conditional(as(x, "dgCMatrix"), dist = TRUE, args))
    }

    ## do regular distances
    ## FIXME: we can do some distances faster

    x <- as(x, "matrix")
    if (which == "items")
      x <- t(x)
    if (args$na_as_zero)
      x[is.na(x)] <- 0


    if (!is.null(y)) {
      y <- as(y, "matrix")
      if (which == "items")
        y <- t(y)
      if (args$na_as_zero)
        y[is.na(y)] <- 0
    }

    ### of person we only use 1 - cor
    if (method == "pearson") {
      if (!is.null(y))
        y <- t(y)
      suppressWarnings(pcd <- 1 - cor(t(x), y, method = "pearson",
        use = "pairwise.complete.obs"))
      if (is.null(y))
        pcd <- as.dist(pcd)
      else
        class(pcd) <- "crossdist"
      return(pcd)
    }

    ## use dist in proxy
    proxy::dist(x = x, y = y, method = method)
  })


## FIXME: Add minimum number of matching items/min number of predictive items
setMethod("similarity", signature(x = "ratingMatrix"),
  function(x,
    y = NULL,
    method = NULL,
    args = NULL,
    which = "users",
    min_matching = 0,
    min_predictive = 0) {
    which <- match.arg(tolower(which), c("users", "items"))
    args <-
      getParameters(list(na_as_zero = FALSE, alpha = .5), args)

    if (!is.null(method))
      method <- tolower(method)
    else
      method <- "cosine"

    ## handle Karypis and conditional similarities
    if (method == "karypis") {
      if (!is.null(y) ||
          which != "items")
        stop("Kaypis similarities are not implemented between users or as a cross-similarity!")

      if (min_matching > 0 ||
          min_predictive > 0)
        warning("min_matching and min_predictive for this method not implemented yet.")

      return(.karypis(as(x, "dgCMatrix"), dist = FALSE, args))
    }

    if (method == "conditional") {
      if (!is.null(y) ||
          which != "items")
        stop(
          "Conditional similarities are not implemented between users or as a cross-similarity!"
        )

      if (min_matching > 0 ||
          min_predictive > 0)
        warning("min_matching and min_predictive for this method not implemented yet.")

      return(.conditional(as(x, "dgCMatrix"), dist = FALSE, args))
    }

    xm <- as(x, "matrix")
    if (which == "items")
      xm <- t(xm)
    if (args$na_as_zero)
      xm[is.na(xm)] <- 0

    ym <- NULL
    if (!is.null(y)) {
      ym <- as(y, "matrix")
      if (which == "items")
        ym <- t(ym)
      if (args$na_as_zero)
        ym[is.na(ym)] <- 0
    }

    ### of person is just cor
    if (method == "pearson") {
      sim <-
        suppressWarnings(cor(
          t(xm),
          if (!is.null(ym))
            t(ym)
          else
            NULL,
          method = "pearson",
          use = "pairwise.complete.obs"
        ))
      if (is.null(y))
      {
        sim <- as.dist(sim)
        class(sim) <- c("simil", "dist")
      } else
        class(sim) <- c("crosssimil", "crossdist")
    } else {
      ## use simil in proxy (class is "simil" "dist" or "crosssimil" "crossdist")
      sim <- proxy::simil(x = xm, y = ym, method = method)
    }

    # change from [-1, 1] to [0, 1]
    if (method == "pearson" || method == "cosine")
      sim <- (sim + 1) / 2


    if (min_matching > 0 || min_predictive > 0) {
      x_has_r <- hasRating(x)
      y_has_r <- if (!is.null(y))
        hasRating(y)
      else
        x_has_r

      if (which == "items") {
        x_has_r <- t(x_has_r)
        y_has_r <- t(y_has_r)
      }

      ### set similarities with less than min_matching items to NA
      if (min_matching > 0) {
        shared <-
          as.matrix(tcrossprod(as(x_has_r, "dsparseMatrix"), as(y_has_r, "dsparseMatrix")))
        if (is.matrix(sim))
          sim[shared < min_matching] <- NA
        else
          sim[as.dist(shared) < min_matching] <- NA
      }

      ### set similarities with less than min_predictive items to NA
      if (min_predictive > 0) {
        predictive <- as.matrix(rowSums(x_has_r) - shared)
        if (is.matrix(sim))
          sim[predictive < min_predictive] <- NA
        else
          sim[as.dist(predictive) < min_predictive] <- NA
      }
    }

    sim
  })

## conditional similarity (Karypis 2001)
.conditional <- function(x, dist = TRUE, args = NULL) {
  n <- ncol(x)

  ## sim(v,u) = freq(uv) / freq(v)
  uv <-  as.matrix(crossprod(x))
  v <- matrix(colSums(x),
    nrow = n,
    ncol = n,
    byrow = FALSE)

  sim <- uv / v

  ## fix if freq was 0
  sim[is.na(sim)] <- 0

  if (dist)
    sim <- as.dist(1 / (1 + sim))
  else
    attr(sim, "type") <- "simil"
  attr(sim, "method") <- "conditional"
  sim
}

## Karypis similarity
## FIXME: check this!!!
.karypis <- function(x, dist, args = NULL) {
  ## get alpha (na_as_zero is ignored)
  stop("Karypis similarity not implemented!")

  args <-
    getParameters(list(na_as_zero = NULL, alpha = .5), args)

  n <- ncol(x)

  ## normalize rows to unit length
  #x <- x / rowSums(x)

  ## for users without items
  x[is.na(x)] <- 0

  ## sim(v,u) =
  ##      sum_{for all i: r_i,v >0} r_i,u / freq(v) / freq(u)^alpha
  uv <-  as.matrix(crossprod(x, x > 0))
  v <- matrix(colSums(x),
    nrow = n,
    ncol = n,
    byrow = FALSE)
  u <- t(v)

  sim <- uv / v / u ^ args$alpha

  ##  fix if freq = 0
  sim[is.na(sim)] <- 0

  if (dist)
    sim <- as.dist(1 / (1 + sim))
  else
    attr(sim, "type") <- "simil"
  attr(sim, "method") <- "karypis"
  sim

}
