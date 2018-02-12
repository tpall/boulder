
#' Run query using GET
#'
#' @description Wrapper around \pkg{httr}::GET
#'
#' @param url the url of the page to retrieve.
#' @param path optional path appended to url, defaults to \code{NULL}.
#'
#' @import httr
query_api <- function(path = NULL, url) {

  if (!is.null(path)) {
    url <- httr::parse_url(url)
    url$path <- file.path(url$path, path)
    url <- httr::build_url(url)
  }

  resp <- httr::GET(url, httr::accept_json())
  httr::stop_for_status(resp, task = "download")
  return(resp)
}

#' Extract json as data frame from request content
#'
#' Checks if request object type is `application/json`, retrieves contents and converts from json to data frame.
#'
#' @param resp request object.
#'
#' @return a data_frame
#'
#' @import httr
#' @import jsonlite
#'
get_json <- function(resp) {
  if (httr::http_type(resp) != "application/json") {
    stop("Input type is not json", call. = FALSE)
  }
  json <- httr::content(resp, "text")
  df <- jsonlite::fromJSON(json, simplifyDataFrame = TRUE)

  if (all(class(df) == "list")) {
    df <- df$variables
  }

  dplyr::as_data_frame(df)
}
