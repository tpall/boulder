#' Extract data from json file downloaded from Estonian Health Statistics database
#'
#' @param x A complex json file.
#' @return Returns data.frame in the long format.
#' @examples
#'
#' library(jsonlite)
#' pk10js <- fromJSON(system.file("extdata", "PK10.json", package = "boulder", mustWork = TRUE))
#' pk10 <- json_to_df(pk10js)
#'
json_to_df <- function(x) {
  id <- x$dataset$dimension$id
  ids <- x$dataset$dimension[id]
  ids_cat <- lapply(ids, "[[", "category")
  ids_lab <- lapply(ids_cat, "[[", "label")
  ids_df <- do.call(expand.grid, ids_lab)
  ids_df$label <- x$dataset$label
  ids_df$source <- x$dataset$source
  ids_df$updated <- x$dataset$updated
  ids_df$value <- x$dataset$value
  return(ids_df)
  }
