% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/splnr_deprecated.R
\name{splnr_plot_cost}
\alias{splnr_plot_cost}
\title{Plot cost}
\usage{
splnr_plot_cost(
  Cost,
  Cost_name = "Cost",
  legendTitle = "Cost",
  paletteName = "YlGnBu",
  plotTitle = ""
)
}
\arguments{
\item{Cost}{An \code{sf} object of cost for \code{prioritizr}}

\item{Cost_name}{Name of the cost column}

\item{legendTitle}{A character value for the title of the legend. Can be empty ("").}

\item{paletteName}{A string (or number) for the color palette to use. Available palettes can be found at https://ggplot2.tidyverse.org/reference/scale_brewer.html.}

\item{plotTitle}{A character value for the title of the plot. Can be empty ("").}
}
\value{
A ggplot object of the plot
}
\description{
\code{splnr_plot_cost()} allows to plot cost within each planning units of a planning region in a customisable way using \code{ggplot2}. This function requires an \code{sf} object with a cost column and outputs a \code{ggobject}. It can be combined with the \code{spatialplanr} function \code{\link[=splnr_gg_add]{splnr_gg_add()}}.
}
\details{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}
}
\examples{
\dontrun{
dat_problem <- prioritizr::problem(dat_species_bin \%>\% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
  features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
  cost_column = "Cost"
) \%>\%
  prioritizr::add_min_set_objective() \%>\%
  prioritizr::add_relative_targets(0.3) \%>\%
  prioritizr::add_binary_decisions() \%>\%
  prioritizr::add_default_solver(verbose = FALSE)

dat_soln <- dat_problem \%>\%
  prioritizr::solve.ConservationProblem()

dat_cost <- dat_soln \%>\%
  dplyr::mutate(Cost = runif(n = dim(.)[[1]]))

(splnr_plot_cost(dat_cost))
}
}
