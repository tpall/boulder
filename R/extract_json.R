#' Extract data from json file downloaded from Estonian Health Statistics and Health Research database
#'
#' @param jsonfile A path to json file, character string.
#' @return Returns data.frame in the long format.
#' @examples
#'
#' path_to_PK10.json <- system.file("extdata", "PK10.json", package = "boulder", mustWork = TRUE)
#' pk10 <- json_to_df(path_to_PK10.json)
#' @export
json_to_df <- function(jsonfile) {

  # Import from json
  x <- jsonlite::fromJSON(jsonfile)

  # Variable ids
  id <- x$dataset$dimension$id
  ids <- x$dataset$dimension[id]

  # Variables from dimension
  dim_cat <- lapply(ids, "[[", "category")
  dim_lab <- lapply(dim_cat, "[[", "label")
  dim_dc <- do.call(expand.grid, dim_lab)
  dim_ul <- sapply(dim_dc, unlist)
  dim_df <- dplyr::as_data_frame(dim_ul, row.names = FALSE)

  # Dataset metadata and values
  ds_nd <- x$dataset[setdiff(names(x$dataset), c("dimension","status"))]
  ds_ul <- sapply(ds_nd, unlist)
  ds_df <- dplyr::as_data_frame(ds_ul)

  # Merge two data.frames
  dplyr::bind_cols(dim_df, ds_df)
}
