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

  # Split db_nodes into max 30 rows, otherwise the server might choke
  db_nodessplit <- split(db_nodes, c(rep(1, 30), rep(2, nrow(db_nodes) - 30)))

  qfun <- ~dplyr::mutate(.x, tables = purrr::map2(dbid, nodes_id, ~get_tables(dbi = .x, node = .y, lang = lang)))

  db_tables1 <- purrr::map(db_nodessplit[1], qfun)
  db_tables2 <- purrr::map(db_nodessplit[2], qfun)
  db_tables <- dplyr::bind_rows(db_tables1, db_tables2)

  unwrap_content <- function(tab) {
    dplyr::mutate(tab, tables = purrr::map(tables, "content")) %>%
      tidyr::unnest(tables, .drop = FALSE, .sep = "_")
  }

  db_tables <- unwrap_content(db_tables)

  # Filter paths with tables (leave out empty paths)
  db_pxtables <- dplyr::filter(db_tables, stringr::str_detect(tables_id, "px$"))

  # Paths that need deeper dig
  db_deeptables <- dplyr::filter(db_tables, !stringr::str_detect(tables_id, "px$"))
  db_deeptables <- dplyr::mutate(db_deeptables, nodes_id = file.path(nodes_id, tables_id)) %>%
    dplyr::select(-dplyr::contains("tables")) %>%
    dplyr::mutate(tables = purrr::map2(dbid, nodes_id, ~get_tables(dbi = .x, node = .y, lang = lang)))

  db_deeptables <- unwrap_content(db_deeptables)
  db_deeptables <- dplyr::filter(db_deeptables, purrr::map_lgl(tables_id, stringr::str_detect, "px$"))

  # Merge results
  db_tables <- dplyr::bind_rows(db_pxtables, db_deeptables)

  # Format for output
  db_tables  <- dplyr::select(db_tables, text, nodes_id, tables_text, tables_updated) %>%
    magrittr::set_colnames(c("Database","Node", "tables_text", "Updated")) %>%
    tidyr::separate(tables_text, into = c("Name", "Title"), sep = ":") %>%
    dplyr::mutate_at(dplyr::vars(Name, Title), trimws)

  return(db_tables)
}


