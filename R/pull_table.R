
#' Extract table from downloaded content.
#' @param tab a table to be unwrapped.
#'
unwrap_content <- function(tab) {
  dplyr::mutate(tab, tables = purrr::map(tables, "content")) %>%
    tidyr::unnest(tables, .drop = FALSE, .sep = "_")
}

#' Map get_tables function
#' @param x dbid
#' @param y nodes_id
map_get_tables <- function(x, y) {
  purrr::map2(x, y, ~ get_tables(dbi = .x, node = .y))
}

#' Run api query
#' @param x a table with database and node ids
#' @param sleep sleep time between queries in seconds to avoid server choking
#'
query_fun <- function(x, sleep) {
  tab <- dplyr::mutate(x, tables = map_get_tables(dbid, nodes_id))
  Sys.sleep(sleep)
  tab
}

#' List available data tables
#'
#' Lists databases, nodes and tables in Estonian National Institute for Health Development (Tervise Arengu Instituut, TAI) database.
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
#' @export
#'
get_all_tables <- function(local = TRUE, lang = c("et", "en"), verbose = FALSE){

  lang <- match.arg(lang)

  if (local) {
    if (lang == "et") return(vars_et)
    if (lang == "en") return(vars_en)
  }

  # Get databases
  if (verbose) {
    message("Getting list of available databases.")
  }

  db <- get_databases()
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
  n_nodes <- nrow(db_nodes)
  db_nodessplit <- split(db_nodes, sort(rep_len(1:floor(n_nodes / 10), n_nodes)))

  db_tables <- try(purrr::map(db_nodessplit, query_fun, sleep = 0.3), silent = TRUE)

  if (inherits(db_tables, "try-error")) {
    stop("Database nodes download failed. Please retry!")
  }

  db_tables <- dplyr::bind_rows(db_tables) %>% unwrap_content()

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

#' Recode downloaded variables
#' @param vector_to_modify A vector to modify.
#' @param replacements Replacements.
#' @importFrom glue glue
recode_vars <- function(vector_to_modify, replacements) {
  rs <- glue::glue("'{replacements}' = '{names(replacements)}'")
  rs <- stringr::str_c(rs, collapse = ",")
  expr <- glue::glue("dplyr::recode(vector_to_modify, {rs})")
  eval(parse(text = expr))
}

#' Check vars that can be eliminated
#' @param x metadata from pxweb database, a data frame.
eliminate <- function(x) {
  dplyr::filter(x, purrr::map_lgl(elimination, isTRUE))
}

# Create list for pxweb query
px_querylist <- function(code, values) {
  list(code = code,
       selection = list(filter = "item",
                        values = values))
}

#' Download data table from TAI
#'
#' Downloads data table in Estonian National Institute for Health Development (Tervise Arengu Instituut, TAI) database.
#'
#' @param tabname a table Name from list_variables(), a character string.
#' @param tablist list of available database tables, a data_frame generated by
#' get_all_tables() function. Defaults to NULL, in which case internal
#' table is used.
#' @param lang Language, a character string. "et" -- Estonian, "en" -- English.
#'
#' @examples
#' # Download data for table 'RK01'
#' rk01 <- pull_table("RK01", lang = "en")
#' rk01
#'
#' @importFrom magrittr "%>%"
#' @export
#'
pull_table <- function(tabname, tablist = NULL, lang = c("et", "en")) {

  lang <- match.arg(lang)

  if (is.null(tablist)) {
    tablist <- get_all_tables(lang = lang)
  }

  if (!"tai_tables" %in% class(tablist)) {
    stop("Please supply table Name generated by get_all_tables() function.")
  }

  if (!(tabname %in% tablist$Name)) {
    stop("Supplied table Name not present in database.")
  }

  # Download and process metadata
  md <- dplyr::filter(tablist, Name == tabname) %>%
    dplyr::mutate(resp = purrr::pmap(list(Database, Node, Name),
                                     get_tables,
                                     lang = lang),
                  metadata = purrr::map(resp, ~ .x$content$variables))

  md_unnested <- md %>% tidyr::unnest(metadata)

  # Variables legend
  valuecodes <- md_unnested %>%
    dplyr::select(code, text, values, valueTexts) %>%
    tidyr::unnest() %>%
    dplyr::group_by(code, text) %>%
    tidyr::nest() %>%
    dplyr::mutate(data = purrr::map2(data, code, ~ {colnames(.x)[1] <- .y; .x}))

  # Filter out summary stats with values > 0
  metadata <- md_unnested %>%
    eliminate() %>%
    tidyr::unnest() %>%
    dplyr::filter(values > 0) %>%
    dplyr::select(code, values) %>%
    dplyr::group_by(code) %>%
    tidyr::nest(.key = "values") %>%
    dplyr::mutate(values = purrr::map(values, 1))

  # Create json structure
  query <- metadata %>%
    dplyr::mutate(query = purrr::map2(code, values, px_querylist)) %>%
    dplyr::pull(query)

  json <- list(query = query,
               response = list(format = "json")) %>%
    jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)

  # Construct query URL
  dbi <- md$Database
  node <- md$Node
  tab <- stringr::str_c(md$Name, ".px")
  path <- file.path("/PXWeb2015/api/v1/", lang, dbi, node, tab)
  url <- httr::modify_url("http://pxweb.tai.ee", path = path)

  # Run query
  resp <- httr::POST(url = url,
                     body = json,
                     httr::content_type_json())

  # Stop if query not successful
  httr::stop_for_status(resp, task = "download table.")

  jsonresponse <- httr::content(resp, "text") %>% jsonlite::fromJSON()
  data <- jsonresponse$data
  value <- dplyr::as_data_frame(data) %>%
    tidyr::unnest(values) %>%
    dplyr::mutate_at("values", readr::parse_number, na = c(".", "..", "", "NA")) %>%
    dplyr::select(value = values)
  keys <- do.call(rbind, data$key) %>%
    dplyr::as_data_frame()
  keyslist <- keys %>% rlang::as_list(.)

  repl <- valuecodes$data %>%
    purrr::map(~ split(.x[[1]], .x[[2]])) %>%
    purrr::map(unlist)

  recoded_vars <- dplyr::data_frame(keyslist, repl) %>%
    dplyr::mutate(recoded = purrr::map2(keyslist, repl, recode_vars)) %>%
    dplyr::pull(recoded) %>%
    dplyr::bind_cols()

  # Name variables according to chosen language
  colnames(recoded_vars) <- valuecodes$text

  # Merge recoded variables with values
  dplyr::bind_cols(recoded_vars, value)
}
