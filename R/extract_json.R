
#' Convert json to data_frame.
#' Extract data from json file downloaded from Estonian Health Statistics and Health Research database
#'
#' @param json A path to json file or json string, a character string.
#' @return Returns data_frame in the long format.
#' @examples
#'
#' path_to_PK10.json <- system.file("extdata", "PK10.json", package = "boulder", mustWork = TRUE)
#' pk10 <- json_to_df(path_to_PK10.json)
#'
#' @import jsonlite
#' @import dplyr
#' @importFrom rlang syms
#' @importFrom utils head tail
#' @importFrom reshape2 melt
#' @export
#'
json_to_df <- function(json) {

  ## Import from json
  x <- jsonlite::fromJSON(json)

  ## Extract dataset
  dataset <- x[[names(x)]]

  ## Get data source
  source <- dataset$source

  ## Warn when data source in not "Estonian Health Board"
  if(!stringr::str_detect(source, "terviseamet")){
    message(sprintf("Data source is not Estonian Health Board, www.terviseamet.ee, but %s.", source))
  }

  ## Variables dimensions and ids
  dimension <- dataset$dimension
  ids <- dimension$id
  size <- dimension$size

  ## Extract categories
  categories <- lapply(ids, function(i){
    unlist(dimension[[i]]$category$label)
  })

  ## Extract category labels
  labels <- vapply(ids, function(x) dimension[[x]]$label, character(1))

  ## Add labels to categories, otherwise script will break
  names(categories) <- labels

  ## Check length of categories
  sizes <- vapply(categories, length, integer(1))
  sizesok <- all(mapply(all.equal, size, sizes))

  if(!sizesok) {
    stop("Something is wrong with category dimensions!")
  }

  ## Extract values
  values <- as.data.frame(matrix(dataset$value, ncol = tail(size, 1), byrow = T))

  ## Assign column names to values
  colnames(values) <- categories[[tail(labels, 1)]]

  ## Other variables
  rowvars <- head(labels, -1)

  ## Recreate variables combinations
  rowvars_cat <- expand.grid(categories[rowvars])
  arrangeby <- rlang::syms(rowvars)
  vars <- dplyr::arrange(rowvars_cat, !!!arrangeby)
  data <- dplyr::bind_cols(vars, values)

  ## Extract dataset label
  label <- stringr::str_extract(dataset$label, "^.*(?= by)")

  ## Melt data and compose data frame
  meltedata <- reshape2::melt(data)
  colnames(meltedata)[colnames(meltedata)=="variable"] <- tail(labels, 1)
  meltedata$label <- label
  meltedata$source <- source
  meltedata$updated <- dataset$updated
  meltedata
}

