% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/splnr_plotting.R
\name{splnr_plot_comparison}
\alias{splnr_plot_comparison}
\title{Plot solution comparison}
\usage{
splnr_plot_comparison(
  soln1,
  soln2,
  legendTitle = "Scenario 2 compared to Scenario 1:"
)
}
\arguments{
\item{soln1}{The first \code{prioritizr} solution}

\item{soln2}{The second \code{prioritizr} solution}

\item{legendTitle}{A character value for the title of the legend. Can be empty ("").}
}
\value{
A ggplot object of the plot
}
\description{
Conservation planning often requires the comparison of the outputs of the solutions of different conservation problems. One way to compare solutions is by spatially visualising the different planning units that were selected in two separate solutions to conservation problems.
\code{splnr_plot_comparison()} allows to map the differences of two solutions in customisable way using \code{ggplot2}. This function requires two separate \code{sf} objects each containing a \code{solution_1} column indicating the binary solution (selected vs not selected) of a \code{prioritizr} conservation problem. It outputs a \code{ggobject} and can be combined with the \code{spatialplanr} function \code{\link[=splnr_gg_add]{splnr_gg_add()}}.
}
\examples{
# 30 \% target for problem/solution 1
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

# 50 \% target for problem/solution 2
dat_problem2 <- prioritizr::problem(
  dat_species_bin \%>\%
    dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
  features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
  cost_column = "Cost"
) \%>\%
  prioritizr::add_min_set_objective() \%>\%
  prioritizr::add_relative_targets(0.5) \%>\%
  prioritizr::add_binary_decisions() \%>\%
  prioritizr::add_default_solver(verbose = FALSE)

dat_soln2 <- dat_problem2 \%>\%
  prioritizr::solve.ConservationProblem()

(splnr_plot_comparison(dat_soln, dat_soln2))
}
