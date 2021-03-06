% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pull_table.R
\name{get_all_tables}
\alias{get_all_tables}
\title{List available data tables}
\usage{
get_all_tables(local = TRUE, lang = c("et", "en"), verbose = FALSE)
}
\arguments{
\item{local}{whether to return package internal table of available datasets. A logical, defaults to \code{TRUE}.}

\item{lang}{Language. Estonian 'et' or english 'en'. Affects table titles and variable names.}

\item{verbose}{Display messages to track query progress, defaults to \code{TRUE}.}
}
\value{
A data_frame with following columns:
\describe{
\item{Database}{Database name as available at TAI.}
\item{Node}{Database node name.}
\item{Name}{Variable name. This name should be used to download variable values.}
\item{Title}{Description of the variable.}
\item{Updated}{Time of the last update.}
}
}
\description{
Lists databases, nodes and tables in Estonian National Institute for Health Development (Tervise Arengu Instituut, TAI) database.
}
\examples{
\dontrun{
tabs <- get_all_tables()

# In RStudio, it is convenient to use View function
View(tabs)
}

}
