## setup registry for recommender methods
recommenderRegistry <- registry(registry_class="recommender_registry",
    entry_class="recommender_method")

recommenderRegistry$set_field("method", type = "character",
    is_key = TRUE, index_FUN = match_partial_ignorecase)
recommenderRegistry$set_field("dataType", type = "character",
    is_key = TRUE, index_FUN = match_exact)
recommenderRegistry$set_field("fun", type = "function",
    is_key = FALSE)
recommenderRegistry$set_field("description", type = "character",
    is_key = FALSE)
recommenderRegistry$set_field("reference", type = "character",
    is_key = FALSE)
recommenderRegistry$set_field("parameters", type = "list",
    is_key = FALSE)

print.recommender_method <- function(x, ...) {
  with(x, {
    writeLines(strwrap(sprintf("Recommender method: %s for %s\nDescription: %s\nReference: %s",
                       method, dataType, description, reference), exdent = 2))

    if(is.list(parameters)) {
      writeLines("Parameters:")

      parameters <- lapply(parameters, FUN =
          function(p) capture.output(dput(p, control = list()))[1])

      print(as.data.frame(parameters))
    } else writeLines("Parameters: None")
  })}

