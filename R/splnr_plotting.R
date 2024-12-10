#' Plot prioritizr solution
#'
#' `splnr_plot_solution()` allows to plot the solution of a `prioritizr` conservation problem with our without in a customisable way using `ggplot2`. This function requires a solution as an `sf` object with a column called `solution_1` and outputs a `ggobject`. It can be combined with the `spatialplanr` function [splnr_gg_add()].
#'
#' @param soln The `prioritizr` solution
#' @param colorVals A `list` object of named vectors that will match the color value with the according name. "TRUE" stands for selected planning units.
#' @param showLegend A logical command on whether to show the legend of the solution (Default: TRUE).
#' @param legendLabels Character values (number of zones + 1) of what the legend should be labelled.
#' @param plotTitle A character value for the title of the plot. Can be empty ("").
#' @param legendTitle A character value for the title of the legend. Can be empty ("").
#' @param zones A logical value, indicating whether the spatial plan contains zones or not (default = FALSE).
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @examples
#' dat_problem <- prioritizr::problem(dat_species_bin %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
#'   features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
#'   cost_column = "Cost"
#' ) %>%
#'   prioritizr::add_min_set_objective() %>%
#'   prioritizr::add_relative_targets(0.3) %>%
#'   prioritizr::add_binary_decisions() %>%
#'   prioritizr::add_default_solver(verbose = FALSE)
#'
#' dat_soln <- dat_problem %>%
#'   prioritizr::solve.ConservationProblem()
#'
#' splnr_plot_solution(dat_soln)
#' # example 2
#' t2 <- matrix(NA, ncol = 2, nrow = 5) # create targets
#' t2[, 1] <- 0.1
#' t2[, 2] <- 0.05
#'
#' z2 <- prioritizr::zones(
#'   "zone 1" = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
#'   "zone 2" = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5")
#' )
#' # when giving sf input, we need as many cost columns as we have zones
#' p2 <- prioritizr::problem(
#'   dat_species_bin %>% dplyr::mutate(
#'     Cost1 = runif(n = dim(.)[[1]]),
#'     Cost2 = runif(n = dim(.)[[1]])
#'   ),
#'   z2,
#'   cost_column = c("Cost1", "Cost2")
#' ) %>%
#'   prioritizr::add_min_set_objective() %>%
#'   prioritizr::add_relative_targets(t2) %>%
#'   prioritizr::add_binary_decisions() %>%
#'   prioritizr::add_default_solver(verbose = FALSE)
#'
#' s2 <- p2 %>%
#'   prioritizr::solve.ConservationProblem()
#' (splnr_plot_solution(s2,
#'   zones = TRUE, colorVals = c("#c6dbef", "#3182bd", "black"),
#'   legendLabels = c("Not selected", "Zone 1", "Zone 2")
#' ))
splnr_plot_solution <- function(soln, colorVals = c("#c6dbef", "#3182bd"),
                                showLegend = TRUE, legendLabels = c("Not selected", "Selected"),
                                plotTitle = "Solution", legendTitle = "Planning Units",
                                zones = FALSE) {
  assertthat::assert_that(
    inherits(soln, c("sf", "data.frame")),
    is.logical(showLegend),
    length(colorVals) == length(legendLabels),
    is.character(plotTitle) | is.call(plotTitle),
    is.character(legendTitle),
    is.logical(zones)
  )

  if (zones == FALSE) {
    soln <- soln %>%
      dplyr::select("solution_1") %>%
      dplyr::mutate(solution = as.factor(.data$solution_1))
    nrows <- 2
  } else if (zones == TRUE) {
    oldName <- soln %>%
      dplyr::select(tidyselect::starts_with(c("solution"))) %>%
      sf::st_drop_geometry() %>%
      tibble::as_tibble() %>%
      names()

    newName <- gsub("1_zone", "", oldName) # to make data a bit nicer to work with
    nrows <- (length(newName) + 1)

    solnNewNames <- soln %>%
      dplyr::rename_at(dplyr::vars(tidyselect::all_of(oldName)), ~newName) %>%
      dplyr::select(tidyselect::starts_with(c("solution")))

    for (i in 2:(length(newName))) {
      solnNewNames <- solnNewNames %>%
        dplyr::mutate(
          !!rlang::sym(newName[i]) := dplyr::case_when(
            !!rlang::sym(newName[i]) == 1 ~ i,
            !!rlang::sym(newName[i]) == 0 ~ 0
          )
        )
    }

    soln <- solnNewNames %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        solution = sum(dplyr::c_across(cols = tidyselect::starts_with("solution_"))),
        solution = factor(.data$solution, levels = 0:(length(newName)))
      )
  } else {
    cat("The zones attribute requires a logical input. Please set to TRUE or FALSE.")
  }

  # quick check
  if (nlevels(soln$solution) != length(colorVals)) {
    cat("Number of colour values needs to be the same as the number of levels in the solution column.")
  }

  if (nlevels(soln$solution) != length(legendLabels)) {
    cat("Number of legend labels needs to be the same as the number of levels in the solution column.")
  }

  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = soln, ggplot2::aes(fill = .data$solution), colour = NA, size = 0.1, show.legend = showLegend) +
    ggplot2::coord_sf(xlim = sf::st_bbox(soln)$xlim, ylim = sf::st_bbox(soln)$ylim) +
    ggplot2::scale_fill_manual(
      name = legendTitle,
      values = colorVals,
      labels = legendLabels,
      aesthetics = c("fill"),
      guide = ggplot2::guide_legend(
        override.aes = list(linetype = 0),
        nrow = nrows,
        order = 1,
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    ggplot2::labs(subtitle = plotTitle)

  return(gg)

}


#' Plot cost overlay
#'
#' `splnr_plot_costOverlay()` allows to plot the cost of each planning units of a planning region on top of the solution of a conservation problem created with `prioritizr` in a customisable way using `ggplot2`. This function requires a solution as an `sf` object with a column called `solution_1` as well as a cost column and outputs a `ggobject`. It can be combined with the `spatialplanr` function [splnr_gg_add()].
#'
#' @param soln The `prioritizr` solution
#' @param Cost An `sf` object of cost for `prioritizr`.In case `prioritizr`solution does not contain cost, alternative cost object has to be provided here that was used to generate solution (default: NA).
#' @param Cost_name Name of the cost column
#' @param plotTitle A character value for the title of the plot. Can be empty ("").
#' @param legendTitle A character value for the title of the legend. Can be empty ("").
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @examples
#' dat_problem <- prioritizr::problem(dat_species_bin %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
#'   features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
#'   cost_column = "Cost"
#' ) %>%
#'   prioritizr::add_min_set_objective() %>%
#'   prioritizr::add_relative_targets(0.3) %>%
#'   prioritizr::add_binary_decisions() %>%
#'   prioritizr::add_default_solver(verbose = FALSE)
#'
#' dat_soln <- dat_problem %>%
#'   prioritizr::solve.ConservationProblem()
#'
#' splnr_plot_costOverlay(soln = dat_soln)
splnr_plot_costOverlay <- function(soln, Cost = NA, Cost_name = "Cost",
                                   legendTitle = "Cost",
                                   plotTitle = "Solution overlaid with cost") {

  assertthat::assert_that(
    inherits(soln, "sf"),
    is.data.frame(Cost) || is.na(Cost),
    is.character(Cost_name),
    is.character(legendTitle),
    is.character(plotTitle)
  )

  if (!is.data.frame(get("Cost"))) { # potentially needed for app later
    if (!Cost_name %in% colnames(soln)) {
      cat("Cost column not found. Please check your solution data frame for your column of interest.")
    } else {
      Cost <- soln %>%
        dplyr::select(!!rlang::sym(Cost_name))
    }
  }

  soln <- soln %>%
    dplyr::select("solution_1") %>%
    dplyr::filter(.data$solution_1 == 1)

  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = soln, fill = "black", colour = NA, size = 0.0001) +
    ggplot2::geom_sf(data = Cost, ggplot2::aes(fill = !!rlang::sym(Cost_name)), alpha = 0.5, colour = NA, size = 0.0001) +
    ggplot2::scale_fill_gradient(
      name = legendTitle,
      # palette = "Oranges",
      low = "#fff5eb",
      high = "#d94801", # "#f16913",
      limits = c(
        0,
        as.numeric(stats::quantile(dplyr::pull(Cost, Cost_name), 0.99, na.rm = TRUE))
      ),
      # direction = 1,
      oob = scales::squish,
      # guide = ggplot2::guide_colourbar(
      #   title.position = "bottom",
      #   title.hjust = 0.5,
      #   order = 1,
      #   barheight = grid::unit(0.03, "npc"),
      #   barwidth = grid::unit(0.25, "npc"))
    ) +
    ggplot2::coord_sf(xlim = sf::st_bbox(Cost)$xlim, ylim = sf::st_bbox(Cost)$ylim) +
    ggplot2::labs(subtitle = plotTitle)

  return(gg)
}





#' Plot solution comparison
#'
#' Conservation planning often requires the comparison of the outputs of the solutions of different conservation problems. One way to compare solutions is by spatially visualising the different planning units that were selected in two separate solutions to conservation problems.
#' `splnr_plot_comparison()` allows to map the differences of two solutions in customisable way using `ggplot2`. This function requires two separate `sf` objects each containing a `solution_1` column indicating the binary solution (selected vs not selected) of a `prioritizr` conservation problem. It outputs a `ggobject` and can be combined with the `spatialplanr` function [splnr_gg_add()].
#'
#' @param soln1 The first `prioritizr` solution
#' @param soln2 The second `prioritizr` solution
#' @param legendTitle A character value for the title of the legend. Can be empty ("").
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' # 30 % target for problem/solution 1
#' dat_problem <- prioritizr::problem(dat_species_bin %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
#'   features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
#'   cost_column = "Cost"
#' ) %>%
#'   prioritizr::add_min_set_objective() %>%
#'   prioritizr::add_relative_targets(0.3) %>%
#'   prioritizr::add_binary_decisions() %>%
#'   prioritizr::add_default_solver(verbose = FALSE)
#'
#' dat_soln <- dat_problem %>%
#'   prioritizr::solve.ConservationProblem()
#'
#' # 50 % target for problem/solution 2
#' dat_problem2 <- prioritizr::problem(
#'   dat_species_bin %>%
#'     dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
#'   features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
#'   cost_column = "Cost"
#' ) %>%
#'   prioritizr::add_min_set_objective() %>%
#'   prioritizr::add_relative_targets(0.5) %>%
#'   prioritizr::add_binary_decisions() %>%
#'   prioritizr::add_default_solver(verbose = FALSE)
#'
#' dat_soln2 <- dat_problem2 %>%
#'   prioritizr::solve.ConservationProblem()
#'
#' (splnr_plot_comparison(dat_soln, dat_soln2))
splnr_plot_comparison <- function(soln1, soln2, legendTitle = "Scenario 2 compared to Scenario 1:") {

  assertthat::assert_that(
    inherits(soln1, "sf"),
    inherits(soln2, "sf"),
    is.character(legendTitle)
  )

  soln <- soln1 %>%
    dplyr::select("solution_1") %>%
    dplyr::bind_cols(soln2 %>%
                       dplyr::as_tibble() %>%
                       dplyr::select("solution_1") %>%
                       dplyr::rename(solution_2 = "solution_1")) %>%
    dplyr::mutate(Combined = .data$solution_1 + .data$solution_2) %>%
    dplyr::mutate(
      Compare = dplyr::case_when(
        Combined == 2 ~ "Same",
        solution_1 == 1 & solution_2 == 0 ~ "Removed (-)",
        solution_1 == 0 & solution_2 == 1 ~ "Added (+)"
      ),
      Compare = factor(.data$Compare, levels = c("Added (+)", "Same", "Removed (-)"))
    ) %>%
    dplyr::filter(!is.na(.data$Compare))

  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = soln, ggplot2::aes(fill = .data$Compare), colour = NA, size = 0.0001) +
    ggplot2::coord_sf(xlim = sf::st_bbox(soln)$xlim, ylim = sf::st_bbox(soln)$ylim) +
    ggplot2::scale_fill_manual(
      name = legendTitle,
      values = c("Added (+)" = "Red", "Same" = "ivory3", "Removed (-)" = "Blue"), drop = FALSE
    )

  return(gg)
}



#' Plot selection frequency of a planning unit in an array of prioritisations
#'
#' When multiple spatial plans are generated, we are often interested in how many times a planning unit is selected across an array of solutions. This array can either be made up of the solutions to different conservation problems or generated through a [portfolio approach](https://prioritizr.net/reference/portfolios.html) with `prioritizr`.
#' Either way, this function requires an `sf` object input that contains a column (`selFreq`) with the selection frequency of each planning unit that can be generated with the `spatialplanr`function `splnr_get_selFreq()`. `splnr_plot_selectionFreq()` allows to visualize this selection frequency using `ggplot2`. It outputs a `ggobject` and can be combined with the `spatialplanr` function `splnr_gg_add()`.
#'
#' @param selFreq An `sf` object containing the selection frequency of a planning unit from an array of solutions
#' @param paletteName A string (or number) for the color palette to use. Available palettes can be found at https://ggplot2.tidyverse.org/reference/scale_brewer.html.
#' @param plotTitle A character value for the title of the plot. Can be empty ("").
#' @param legendTitle A character value for the title of the legend. Can be empty ("").
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' dat_problem <- prioritizr::problem(dat_species_bin %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
#'   features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
#'   cost_column = "Cost"
#' ) %>%
#'   prioritizr::add_min_set_objective() %>%
#'   prioritizr::add_relative_targets(0.3) %>%
#'   prioritizr::add_binary_decisions() %>%
#'   prioritizr::add_default_solver(verbose = FALSE)
#'
#' # create conservation problem that contains a portfolio of solutions
#' dat_soln_portfolio <- dat_problem %>%
#'   prioritizr::add_cuts_portfolio(number_solutions = 5) %>%
#'   prioritizr::solve.ConservationProblem()
#'
#' selFreq <- splnr_get_selFreq(solnMany = dat_soln_portfolio, type = "portfolio")
#' (splnr_plot_selectionFreq(selFreq))
splnr_plot_selectionFreq <- function(selFreq,
                                     plotTitle = "", paletteName = "Greens",
                                     legendTitle = "Selection \nFrequency") {

  assertthat::assert_that(
    inherits(selFreq, c("sf", "data.frame")),
    is.character(plotTitle),
    is.character(legendTitle)
  )

  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = selFreq, ggplot2::aes(fill = .data$selFreq), colour = NA) +
    ggplot2::scale_fill_brewer(
      name = legendTitle,
      palette = paletteName, aesthetics = "fill", # c("colour", "fill"),
      guide = ggplot2::guide_legend(
        override.aes = list(linetype = 0),
        title.position = "top"
      )
    ) +
    ggplot2::coord_sf(
      xlim = c(sf::st_bbox(selFreq)$xmin, sf::st_bbox(selFreq)$xmax),
      ylim = c(sf::st_bbox(selFreq)$ymin, sf::st_bbox(selFreq)$ymax),
      expand = TRUE
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 12, colour = "black"),
      axis.text.x = ggplot2::element_text(size = 12, colour = "black"),
      axis.title.x = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(size = 12),
      legend.text = ggplot2::element_text(size = 12),
      axis.title.y = ggplot2::element_blank()
    ) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::labs(title = plotTitle)

  return(gg)
}

#' Plot importance score
#'
#' [Importance scores](https://prioritizr.net/reference/importance.html) are a mean to reflect the irreplaceability of a planning unit in the solution of a `prioirtizr` conservation problem. Based on the `prioritizr` package, `splnr_plot_importanceScore()` allows to visualize three different types of importance scores with `ggplot2` that should be used based on the conservation problem at hand. The `prioritizr` development team generally recommend using the [replacement cost score](https://prioritizr.net/reference/eval_replacement_importance.html), however this might be not be feasible for conservation problems with many planning units or features.
#'
#' The function outputs a `ggobject` and can be combined with the `spatialplanr` function [splnr_gg_add()].
#'
#' @param soln The `prioritizr` solution
#' @param pDat The `prioritizr` problem
#' @param method The method for calcualting importance scores. Can be either "Ferrier" for the Ferrier Score, which can only be used with the minimum set objective function, "RWR" for Rarity Weighted Richness Score, or "RC" for Replacement Cost which takes longer than the other approaches due to its iterative process.
#' @param colorMap A character string indicating the color map to use (see https://ggplot2.tidyverse.org/reference/scale_viridis.html for all options)
#' @param decimals The number of decimals shown in the plot. Ferrier Score often requires a higher number of decimals (>4) than the other two approaches (2) for this analysis to work.
#' @param plotTitle A character value for the title of the plot. Can be empty ("").
#' @param legendTitle A character value for the title of the legend. Can be empty ("").
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' dat_problem <- prioritizr::problem(dat_species_bin %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
#'   features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
#'   cost_column = "Cost"
#' ) %>%
#'   prioritizr::add_min_set_objective() %>%
#'   prioritizr::add_relative_targets(0.3) %>%
#'   prioritizr::add_binary_decisions() %>%
#'   prioritizr::add_default_solver(verbose = FALSE)
#'
#' dat_soln <- dat_problem %>%
#'   prioritizr::solve.ConservationProblem()
#'
#' (splnr_plot_importanceScore(soln = dat_soln, pDat = dat_problem, method = "Ferrier", decimals = 4))
splnr_plot_importanceScore <- function(soln,
                                       pDat,
                                       method = "Ferrier",
                                       plotTitle = "",
                                       colorMap = "A",
                                       decimals = 4,
                                       legendTitle = "Importance Score") {

  assertthat::assert_that(
    inherits(soln, c("data.frame", "tbl_df", "tbl")),
    inherits(pDat, c("R6", "ConservationProblem")),
    is.character(method),
    is.character(plotTitle),
    is.character(colorMap),
    is.numeric(decimals),
    is.character(legendTitle)
  )

  assertthat::assert_that(
    method %in% c("Ferrier", "RWR", "RC"))

  soln <- soln %>% tibble::as_tibble()

  if (method == "Ferrier") {
    cat("Ferrier Score.")
    scored_soln <- prioritizr::eval_ferrier_importance(pDat, soln[, "solution_1"])

    scored_soln <- scored_soln %>%
      dplyr::select("total") %>%
      dplyr::mutate(geometry = soln$geometry) %>%
      dplyr::rename(score = "total") %>%
      sf::st_as_sf()
  } else if (method == "RWR") {
    cat("Rarity Wighted Richness.")
    scored_soln <- prioritizr::eval_rare_richness_importance(pDat, soln[, "solution_1"]) %>%
      dplyr::mutate(geometry = soln$geometry) %>%
      dplyr::rename(score = "rwr") %>%
      sf::st_as_sf()
  } else if (method == "RC") {
    cat("Replacement cost.")
    scored_soln <- prioritizr::eval_replacement_importance(pDat, soln[, "solution_1"]) %>%
      dplyr::mutate(geometry = soln$geometry) %>%
      dplyr::rename(score = "rc") %>%
      sf::st_as_sf()
  } else {
    cat("Invalid importance score method supplied.")
  }

  selectedfs <- scored_soln %>%
    dplyr::filter(.data$score != 0)

  quant95fs <- round(stats::quantile(selectedfs$score, 0.95), decimals)
  seq95fs <- seq(0, quant95fs, length.out = 5)
  lab <- c(seq95fs[1], seq95fs[2], seq95fs[3], seq95fs[4], paste0("\u2265", quant95fs, sep = " "))

  scored_soln$score[scored_soln$score >= quant95fs] <- quant95fs

  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = scored_soln, ggplot2::aes(fill = .data$score), colour = NA) +
    ggplot2::scale_fill_viridis_c(
      option = colorMap,
      direction = -1, breaks = seq95fs, labels = lab,
      guide = ggplot2::guide_colourbar(
        title = legendTitle,
        # title.position = "right",
        # barwidth = 2, barheight = 10
      )
    ) + # , oob=squish)
    ggplot2::coord_sf(
      xlim = c(sf::st_bbox(scored_soln)$xmin, sf::st_bbox(scored_soln)$xmax),
      ylim = c(sf::st_bbox(scored_soln)$ymin, sf::st_bbox(scored_soln)$ymax),
      expand = TRUE
    ) +
    # ggplot2::theme(
    #   legend.title = ggplot2::element_text(angle = -90, hjust = 0.5),
    #   text = ggplot2::element_text(size = 20),
    #   axis.title = ggplot2::element_blank()
    # ) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::labs(title = plotTitle)

  return(gg)
}

#' Plot correlation matrices
#'
#' Conservation planning often requires the comparison of the outputs of the solutions of different conservation problems.
#' One way to compare solutions is by correlating the solutions using Cohen's Kappa. `splnr_plot_corrMat()` allows to visualize the correlation matrix of the different solutions (for example produced with the `spatialplanr` function [splnr_get_kappaCorrData()]).
#'
#' @param x A correlation matrix of `prioritizr` solutions
#' @param colourGradient A list of three colour values for high positive, no and high negative correlation
#' @param legendTitle A character value for the title of the legend. Can be empty ("").
#' @param AxisLabels A list of labels of the solutions to be correlated (Default: NULL). Length needs to match number of correlated solutions.
#' @param plotTitle A character value for the title of the plot. Can be empty ("").
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' # 30 % target for problem/solution 1
#' dat_problem <- prioritizr::problem(dat_species_bin %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
#'   features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
#'   cost_column = "Cost"
#' ) %>%
#'   prioritizr::add_min_set_objective() %>%
#'   prioritizr::add_relative_targets(0.3) %>%
#'   prioritizr::add_binary_decisions() %>%
#'   prioritizr::add_default_solver(verbose = FALSE)
#'
#' dat_soln <- dat_problem %>%
#'   prioritizr::solve.ConservationProblem()
#'
#' # 50 % target for problem/solution 2
#' dat_problem2 <- prioritizr::problem(
#'   dat_species_bin %>%
#'     dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
#'   features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
#'   cost_column = "Cost"
#' ) %>%
#'   prioritizr::add_min_set_objective() %>%
#'   prioritizr::add_relative_targets(0.5) %>%
#'   prioritizr::add_binary_decisions() %>%
#'   prioritizr::add_default_solver(verbose = FALSE)
#'
#' dat_soln2 <- dat_problem2 %>%
#'   prioritizr::solve.ConservationProblem()
#'
#' CorrMat <- splnr_get_kappaCorrData(list(dat_soln, dat_soln2), name_sol = c("soln1", "soln2"))
#'
#' (splnr_plot_corrMat(CorrMat, AxisLabels = c("Solution 1", "Solution 2")))
splnr_plot_corrMat <- function(x, colourGradient = c("#BB4444", "#FFFFFF", "#4477AA"),
                               legendTitle = "Correlation \ncoefficient",
                               AxisLabels = NULL, plotTitle = "") {

  assertthat::assert_that(
    is.matrix(x),
    length(colourGradient) == 3,
    is.character(legendTitle),
    is.null(AxisLabels) || (is.character(AxisLabels) && length(AxisLabels) == nrow(x)),
    is.character(plotTitle)
  )

  if ((class(AxisLabels)[[1]] == "character") & (nrow(x) != length(AxisLabels))) {
    print("Not enough labels for the length of the matrix. Please check your labels.")
  }

  if (requireNamespace("ggcorrplot", quietly = TRUE) == FALSE){
    stop("To run splnr_plot_corrMat you will need to install the package ggcorrplot.")
  }

  gg <- ggcorrplot::ggcorrplot(x,
                               outline.color = "black",
                               lab = TRUE
  ) +
    ggplot2::scale_fill_gradient2(
      low = colourGradient[3],
      mid = colourGradient[2],
      high = colourGradient[1],
      limits = c(-1, 1),
      guide = ggplot2::guide_colourbar(
        title = legendTitle,
        barwidth = 2, barheight = 10
      )
    ) +
    ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 45)) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.title = ggplot2::element_text(),
      legend.text = ggplot2::element_text(color = "black", size = 10),
      panel.grid = ggplot2::element_blank(),
      # panel.grid.major = element_line(color = "grey86"),
      panel.border = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(color = "black", size = 12),
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(color = "black", size = 12)
    ) +
    ggplot2::labs(title = plotTitle)

  if (class(AxisLabels)[[1]] == "character") {
    gg <- gg +
      ggplot2::scale_x_discrete(
        guide = ggplot2::guide_axis(angle = 45),
        labels = AxisLabels
      ) +
      ggplot2::scale_y_discrete(labels = AxisLabels)
  }

  return(gg)
}
