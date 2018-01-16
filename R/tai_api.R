
#' Fix html character encoding
#' @param x a character vector
#'
unescape_html <- function(x) {
  stringr::str_c("<x>", x, "</x>") %>%
    purrr::map(~ xml2::read_html(.x)) %>%
    purrr::map_chr(~ xml2::xml_text(.x))
}

#' Interact with TAI API.
#'
#' @param path API path relative to http://pxweb.tai.ee
#'
#' @return Returns API response object with three slots:
#' - content parsed query result
#' - path query path
#' - response query response
#'
tai_api <- function(path) {
  url <- httr::modify_url("http://pxweb.tai.ee", path = path)

  resp <- httr::GET(url)
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(resp, "text"))

  if (httr::http_error(resp)) {
    stop(
      sprintf(
        "TAI API request failed [%s]\n%s\n<%s>",
        status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }

  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "tai_api"
  )
}

#' Print TAI API object
#' @param x object of class tai_api
#' @importFrom utils "str"
#'
print.tai_api <- function(x) {
  cat("<Tervise Arengu Instituut ", x$path, ">\n", sep = "")
  str(x$content)
  invisible(x)
}

#' List of available databases at TAI, wrapper around tai_api
#' @return content a data.frame of database ids and title
#' @return path the path used in API query
#' @return response html response
#'
get_available_databases <- function() {
  path <- "/PXWeb2015/api/v1/et"
  tai_api(path)
}

#' Get database nodes
#' @param dbi TAI database id
#' @return a data.frame with id, type - types of node object l and t where l is s sublevel and t is a table, text - textual description
#'
get_nodes <- function(dbi) {
  path <- file.path("/PXWeb2015/api/v1/et/", dbi)
  tai_api(path)
}

#' Get database node ids or table metadata
#' @param dbi TAI database id
#' @param node node id
#' @param table table id
#' @param lang language. Default is Estonian 'et', 'en' English.
#' @details Table metadata result has a title property and an array of variables.
#' The variable object has four properties: code, text, elimination and time.
#' The code and text properties are mandatory properties, while the elimination and time are optional.
#' If time or elimination is not specified, the default value of “n” is used.
#' The properties time and elimination could have either TRUE (yes) or FALSE (no) specified.
#' If a variable has elimination set to “TRUE”, one can then omit selecting a value for that variable.
#' There can only be one variable that has the time property set to “t” for a table.
#' It also contains two lists.
#' One that contains the codes for the all the values of which the variable can assume and one list of all the presentation text for the values.
#' @references \url{http://www.scb.se/contentassets/79c32c72783a4f67b202ad3189f921b9/api-description.pdf}
#' @return a data.frame with id, type - types of node object l and t where l is s sublevel and t is a table, text - textual description
#'
get_tables <- function(dbi, node, table = NULL, lang = c("et", "en")) {

  # Set language
  lang <- match.arg(lang)
  path <- file.path("/PXWeb2015/api/v1/", lang)

  if (is.null(table)) {
    path <- file.path(path, dbi, node)
  } else {
    if (!stringr::str_detect(table, "px$")) {
      table <- stringr::str_c(table, "px", sep = ".")
    }
    path <- file.path(path, dbi, node, table)
  }

  tai_api(path)
}

