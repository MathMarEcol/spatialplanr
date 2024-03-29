% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{splnr_match_names}
\alias{splnr_match_names}
\title{Substitute numbers for all_names in regionalisations}
\usage{
splnr_match_names(dat, nam)
}
\arguments{
\item{dat}{\code{sf} data frame with one column of numeric/integer corresponding to \code{nam}}

\item{nam}{Named character vector of names corresponding to column of dat to recode}
}
\value{
An \code{sf} dataframe with numeric regionalisations substituted for category names
}
\description{
Many regionalisations have numeric values in the shape files that correspond
to a vector of names. Here we provide a function to quickly replace the
numbers with names.
}
\examples{
dat <- dat_region \%>\% dplyr::select(-cellID)
nam <- c("Region1" = "SE Aust", "Region2" = "Tas", "Region3" = "NE Aust")
df <- splnr_match_names(dat, nam)
}
