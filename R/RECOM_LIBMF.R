# LIBMF via recosystem

.REAL_LIBMF_params <- list(
  #normalize=NULL,
  dim = 10,
  costp_l2 = 0.01,
  costq_l2 = 0.01,
  nthread = 1,
  verbose = FALSE
)


REAL_LIBMF <- function(data, parameter = NULL) {
  p <- getParameters(.REAL_LIBMF_params, parameter)

  model <- c(list(data = data), p)

  predict <- function(model,
    newdata,
    n = 10,
    data = NULL,
    type = c("topNList", "ratings", "ratingMatrix"),
    ...) {
    dat <-
      as(rbind(as(model$data, "dgCMatrix"), as(newdata, "dgCMatrix")), "dgTMatrix")

    r <- Reco()
    r$train(
      data_memory(dat@i, dat@j, dat@x),
      opts = list(
        dim = p$dim,
        costp_l2 = p$costp_l2,
        costq_l2 = p$costq_l2,
        nthread = p$nthread,
        verbose = p$verbose
      )
    )

    rows <- (1:nrow(newdata)) + nrow(model$data) - 1L
    cols <- 1:ncol(model$data) - 1L
    i <-  rep(rows, each = length(cols))
    j <- rep(cols, times = length(rows))
    x <- r$predict(data_memory(i, j), out_memory())

    ratings <-
      new("realRatingMatrix", data = as(
        new(
          "dgTMatrix",
          i = i - nrow(model$data),
          j = j,
          x = x,
          Dim = newdata@data@Dim,
          Dimnames = newdata@data@Dimnames
        ),
        "dgCMatrix"
      ))

    returnRatings(ratings, newdata, type, n)
  }

  ## construct recommender object
  new(
    "Recommender",
    method = "LIBFM",
    dataType = class(data),
    ntrain = nrow(data),
    model = model,
    predict = predict
  )
}



## register recommenders
recommenderRegistry$set_entry(
  method = "LIBMF",
  dataType = "realRatingMatrix",
  fun = REAL_LIBMF,
  description = "Matrix factorization with LIBMF via package recosystem (https://cran.r-project.org/web/packages/recosystem/vignettes/introduction.html).",
  parameters = .REAL_LIBMF_params
)
