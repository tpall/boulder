
#' Extract ICD10 string.
#' Extracts ICD10 string and expands range when range of codes is shown.
#' @param x character string containing ICD10 code.
#' @examples
#' icd <- extract_icd("All malignant neoplasms (C00-C97)")
#' @return Character vector with ICD10 codes.
#' @import stringr
#' @import readr
#' @importFrom stats na.omit
#' @export
extract_icd <- function(x){

  icd <- unlist(stringr::str_extract_all(x, "[A-Z]?[0-9]{2}(\\.[0-9])?"))

  if(length(icd) == 0) return(NA)
  if(length(icd) == 1) return(icd)

  dash <- stringr::str_detect(x, "-")
  except <- stringr::str_detect(x, "except")
  slash <- stringr::str_detect(x, "\\/")

  chr <- unique(stringr::str_extract(icd, "[A-Z]"))
  chr <- stats::na.omit(chr)

  if(dash){
    num_range <- readr::parse_number(icd)
    num_seq <- as.character(seq(num_range[1], num_range[2]))
    num_seq <- ifelse(stringr::str_length(num_seq) == 1, stringr::str_c("0", num_seq), num_seq)
    num_seq <- stringr::str_c(chr, num_seq)
  }

  if(except){
    num_seq <- setdiff(num_seq, icd[3])
  }

  if(slash){
    num_seq <- c(num_seq, stringr::str_c(chr, icd[3]))
  }

  num_seq
}
