#' List available variables
#'
#' Lists available databases, nodes and variables in TAI/National Institute for Health Development database.
#'
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
#' @importFrom magrittr "%>%"
#' @export
#'
list_variables <- function(lang = c("et","en"), verbose = TRUE){

  lang <- match.arg(lang)

  # Get databases
  if(verbose){
    message("Getting list of available databases.")
  }

  db <- get_available_databases()
  db_content <- dplyr::as_data_frame(db$content)

  # Get nodes
  if(verbose){
    message("Getting list of database nodes.")
  }

  db_nodes <- dplyr::mutate(db_content, nodes = purrr::map(dbid, ~get_nodes(.x)$content)) %>%
    tidyr::unnest(nodes, .drop = FALSE, .sep = "_")

  # Fix spaces in URL
  db_nodes <- dplyr::mutate_at(db_nodes, "nodes_id", ~stringr::str_replace_all(.x, " ", "%20"))
  # Fix node name
  db_nodes <- dplyr::mutate_at(db_nodes, "nodes_text", ~stringr::str_replace_all(.x, "\\&auml\\;", "\u00E4"))

  # Get tables at nodes
  if(verbose){
    message("Getting list of tables.")
  }

  db_tables <- dplyr::mutate(db_nodes, tables = purrr::map2(dbid, nodes_id, ~get_tables(dbi = .x, node = .y, lang = lang)))
  db_tables <- dplyr::mutate(db_tables, tables = purrr::map(tables, "content")) %>%
    tidyr::unnest(tables, .drop = FALSE, .sep = "_")

  # Filter paths with tables (leave out empty paths)
  db_tables <- dplyr::filter(db_tables, stringr::str_detect(tables_id, "px$"))

  # Format for output
  db_tables  <- dplyr::select(db_tables, text, nodes_id, tables_text, tables_updated) %>%
    magrittr::set_colnames(c("Database","Node", "tables_text", "Updated")) %>%
    tidyr::separate(tables_text, into = c("Name", "Title"), sep = ": ")

  return(db_tables)
}


