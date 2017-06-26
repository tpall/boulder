#' Extract data from json file downloaded from Estonian Health Statistics database
#'
#' @param jsonfile A path to json file, character string.
#' @return Returns data.frame in the long format.
#' @examples
#'
#' pk10 <- json_to_df(system.file("extdata", "PK10.json", package = "boulder", mustWork = TRUE))
#'
#' @export
json_to_df <- function(jsonfile) {

  if(!requireNamespace("jsonlite", quietly = TRUE)){
    stop("Please install missing package jsonlite.")
  }

  x <- jsonlite::fromJSON(jsonfile)
  id <- x$dataset$dimension$id
  ids <- x$dataset$dimension[id]
  dim_cat <- lapply(ids, "[[", "category")
  dim_lab <- lapply(dim_cat, "[[", "label")
  dim_df <- do.call(expand.grid, dim_lab)
  ds_df <- as.data.frame(x$dataset[setdiff(names(x$dataset), "dimension")])
  data.frame(dim_df, ds_df)
}
