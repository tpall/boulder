% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract.R
\name{icd_sums}
\alias{icd_sums}
\title{Tries to identify nested ICD10 codes.}
\usage{
icd_sums(site)
}
\arguments{
\item{site}{vector of character strings with embedded ICD10 codes.}
}
\value{
data_frame with two columns: "Parent" and "Site".
}
\description{
Tries to identify nested ICD10 codes.
}
