
#' Extract table from downloaded content.
#' @param tab a table to be unwrapped.
#'
unwrap_content <- function(tab) {
  dplyr::mutate(tab, tables = purrr::map(tables, "content")) %>%
    tidyr::unnest(tables, .drop = FALSE, .sep = "_")
}

#' List available variables
#'
#' Lists available databases, nodes and variables in TAI/National Institute for Health Development database.
#'
#' @param local whether to return package internal table of available datasets. A logical, defaults to TRUE.
#' @param lang Language. Estonian 'et' or english 'en'. Affects table titles and variable names.
#' @param verbose Display messages to track query progress, defaults to TRUE.
#'
#' @return A data_frame with following columns:
#' \describe{
#'   \item{Database}{Database name as available at TAI.}
#'   \item{Node}{Database node name.}
#'   \item{Name}{Variable name. This name should be used to download variable values.}
#'   \item{Title}{Description of the variable.}
#'   \item{Updated}{Time of the last update.}
#' }
#'
#' @examples
#' \dontrun{
#' vars <- list_variables()
#'
#' # In RStudio, it is convenient to use View function
#' View(vars)
#' }
#'
#' @importFrom magrittr "%>%" set_colnames
#'
list_variables <- function(local = TRUE, lang = c("et", "en"), verbose = FALSE){

  lang <- match.arg(lang)

  if (local) {
    if (lang == "et") return(vars_et)
    if (lang == "en") return(vars_en)
  }

  # Get databases
  if (verbose) {
    message("Getting list of available databases.")
  }

  db <- get_available_databases()
  db_content <- dplyr::as_data_frame(db$content)

  # Get nodes
  if (verbose) {
    message(cat("Found following databases:\n", paste(db_content$text, collapse = "\n ")))
    message("Getting list of database nodes.")
  }

  db_nodes <- dplyr::mutate(db_content, nodes = purrr::map(dbid, ~ get_nodes(.x)$content)) %>%
    tidyr::unnest(nodes, .drop = FALSE, .sep = "_")

  # Fix spaces in URL
  db_nodes <- dplyr::mutate_at(db_nodes, "nodes_id", ~ stringr::str_replace_all(.x, " ", "%20"))

  # Get tables at nodes
  if (verbose) {
    message(cat("Found following nodes:\n", paste(db_nodes$nodes_text, collapse = "\n ")))
    message("Getting list of tables.")
  }

  # Split db_nodes into max 30 rows, otherwise the server might choke
  db_nodessplit <- split(db_nodes, c(rep(1, 30), rep(2, nrow(db_nodes) - 30)))

  qfun <- ~ dplyr::mutate(.x, tables = purrr::map2(dbid, nodes_id, ~get_tables(dbi = .x, node = .y, lang = lang)))

  db_tables1 <- purrr::map(db_nodessplit[1], qfun)
  db_tables2 <- purrr::map(db_nodessplit[2], qfun)
  db_tables <- dplyr::bind_rows(db_tables1, db_tables2)

  db_tables <- unwrap_content(db_tables)

  # Filter paths with tables (leave out empty paths)
  db_pxtables <- dplyr::filter(db_tables, stringr::str_detect(tables_id, "px$"))

  # Paths that need deeper dig
  db_deeptables <- dplyr::filter(db_tables, !stringr::str_detect(tables_id, "px$"))
  db_deeptables <- dplyr::mutate(db_deeptables, nodes_id = file.path(nodes_id, tables_id)) %>%
    dplyr::select(-dplyr::contains("tables")) %>%
    dplyr::mutate(tables = purrr::map2(dbid, nodes_id, ~ get_tables(dbi = .x, node = .y, lang = lang)))

  db_deeptables <- unwrap_content(db_deeptables)
  db_deeptables <- dplyr::filter(db_deeptables, purrr::map_lgl(tables_id, stringr::str_detect, "px$"))

  # Merge results
  db_tables <- dplyr::bind_rows(db_pxtables, db_deeptables)

  # Format for output
  db_tables  <- dplyr::select(db_tables, text, nodes_id, tables_text, tables_updated) %>%
    set_colnames(c("Database","Node", "tables_text", "Updated")) %>%
    tidyr::separate(tables_text, into = c("Name", "Title"), sep = ":") %>%
    dplyr::mutate_at(dplyr::vars(Name, Title), trimws)

  ## Set the name for the class
  class(db_tables) <- append(class(db_tables), "tai_tables")

  return(db_tables)
}


