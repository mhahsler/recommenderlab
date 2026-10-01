.require_recommender_package <- function(package, method) {
  if (!requireNamespace(package, quietly = TRUE))
    stop(sprintf(
      "Recommender method '%s' requires package '%s'. Install it with install.packages('%s').",
      method, package, package
    ), call. = FALSE)
}
