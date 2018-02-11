
#' Convert json to data_frame.
#' Extract data from json file downloaded from Estonian Health Statistics and Health Research database
#'
#' @param json A path to json file or json string, a character string.
#' @param tidy Logical. Return data in wide or long (tidy) format, see Details. Defaults to FALSE.
#' @return Returns data_frame in the long format.
#' @details Returns data in the wide format by default because retrieved data
#' can contain data intermingled with summary data. When table is large, it might
#' be more convenient and efficient to identify and remove rows with summary
#' variables before converting table into long format.
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
json_to_df <- function(json, tidy = FALSE) {

  ## Import from json
  x <- jsonlite::fromJSON(json)

  ## Extract dataset
  dataset <- x[[names(x)]]

  ## Get data source
  source <- dataset$source

  ## Message about data source
  if (!stringr::str_detect(source, "terviseamet")) {
    message(sprintf("Data source is %s.", source))
  }

  ## Variables dimensions and ids
  dimension <- dataset$dimension
  ids <- dimension$id
  size <- dimension$size

  ## Extract categories
  categories <- lapply(ids, function(i) {
    unlist(dimension[[i]]$category$label)
  })

  ## Extract category labels
  labels <- vapply(ids, function(x) dimension[[x]]$label, character(1))

  ## Add labels to categories, otherwise script will break
  names(categories) <- labels

  ## Check length of categories
  sizes <- vapply(categories, length, integer(1))
  sizesok <- all(mapply(all.equal, size, sizes))

  if (!sizesok) {
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
  label <- stringr::str_extract(dataset$label, "^.*(?= by|---)") %>% trimws()

  ## Melt data and compose data frame
  if (tidy) {
    data <- reshape2::melt(data)
    colnames(meltedata)[colnames(data) == "variable"] <- tail(labels, 1)
  }

  data$label <- label
  data$source <- source
  data$updated <- dataset$updated
  dplyr::as_data_frame(data)
}

#' Extract ICD10 string.
#' Extracts ICD10 string and expands range when range of codes is shown.
#' @param x character string containing ICD10 code.
#' @return Character vector with ICD10 codes.
#' @examples
#' icd <- extract_icd("All malignant neoplasms (C00-C97)")
#'
#' @import stringr
#' @import readr
#' @importFrom stats na.omit
#' @export
#'
extract_icd <- function(x) {

  icd <- unlist(stringr::str_extract_all(x, "[A-Z]?[0-9]{2}(\\.[0-9])?"))

  if (length(icd) == 0) return(NA)
  if (length(icd) == 1) return(icd)

  dash <- stringr::str_detect(x, "-")
  except <- stringr::str_detect(x, "except")
  slash <- stringr::str_detect(x, "\\/")

  chr <- unique(stringr::str_extract(icd, "[A-Z]"))
  chr <- stats::na.omit(chr)

  if (dash) {
    num_range <- readr::parse_number(icd)
    num_seq <- as.character(seq(num_range[1], num_range[2]))
    num_seq <- ifelse(stringr::str_length(num_seq) == 1, stringr::str_c("0", num_seq), num_seq)
    num_seq <- stringr::str_c(chr, num_seq)
  }

  if (except) {
    num_seq <- setdiff(num_seq, icd[3])
  }

  if (slash) {
    num_seq <- c(num_seq, stringr::str_c(chr, icd[3]))
  }

  num_seq
}


#' Evaluates whether intersect between two vectors with ICD10 codes is longer than zero.
#' @param x List of length two. Two character vectors with ICD10 codes.
#' @return logical.
#' @import stringr
#' @import readr
#' @importFrom purrr map
#' @importFrom magrittr %>%
icd_intersect <- function(x) {

  firstchr <- purrr::map(x, ~unique(stringr::str_extract(.x, "^[A-Z]"))) %>%
    unlist() %>%
    unique()

  if (length(firstchr) > 1) return(FALSE)

  nums <- purrr::map(x, readr::parse_number)
  equals_one <- vapply(nums, length, numeric(1)) == 1

  if (sum(equals_one) == 1) {
    nums[[which(!equals_one)]] <- floor(nums[[which(!equals_one)]])
  }

  length(intersect(nums[[1]], nums[[2]])) > 0
}

#' Tries to identify nested ICD10 codes.
#' @param site vector of character strings with embedded ICD10 codes.
#' @return data_frame with two columns: "Parent" and "Site".
#' @importFrom purrr map
#' @importFrom dplyr as_data_frame
#' @importFrom utils combn
#' @importFrom magrittr set_colnames
#'
icd_sums <- function(site) {
  icd <- purrr::map(site, extract_icd)
  icd_intersects <- utils::combn(icd, 2,
                                 icd_intersect,
                                 simplify = FALSE)
  site_intersects <- utils::combn(site, 2, simplify = FALSE)

  site_intersects_filtered <- site_intersects[unlist(icd_intersects)]
  do.call(rbind, site_intersects_filtered) %>%
    dplyr::as_data_frame() %>%
    set_colnames(c("Parent", "Site"))
}
