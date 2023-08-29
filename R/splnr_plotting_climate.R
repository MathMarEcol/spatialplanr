#' Plot climate data
#'
#' @param df An `sf` object with climate metric information with
#' @param colInterest column of data frame that contains the metric informatin
#' @param PlanUnits Planning Units as an `sf` object
#' @param landmass An `sf` object of land polygon
#' @param colorMap A character string indicating the color map to use (see https://ggplot2.tidyverse.org/reference/scale_viridis.html for all options)
#' @param colorPUs A color value for the outline of planning units.
#' @param plotTitle A character value for the title of the plot. Can be empty ("").
#' @param legendTitle A character value for the title of the legend. Can be empty ("").
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @examples
#' splnr_plot_climData(dat_clim, dat_clim$metric, dat_PUs)
splnr_plot_climData <- function(df, colInterest, PlanUnits, landmass = NA,
                                colorMap = "C",
                                colorPUs = "grey80",
                                plotTitle = " ", legendTitle = "Climate metric") {
  df <- df %>%
    dplyr::mutate(metric = colInterest)

  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = df %>% sf::st_as_sf(), ggplot2::aes(fill = .data$metric), colour = NA) +
    ggplot2::scale_fill_viridis_c(
      name = legendTitle,
      option = colorMap # ,
      # guide = ggplot2::guide_colourbar(
      # title.position = "bottom",
      #  title.hjust = 0.5,
      #  order = 1,
      # barheight = grid::unit(0.03, "npc"),
      # barwidth = grid::unit(0.25, "npc"))
      # )
    ) +
    ggplot2::geom_sf(data = PlanUnits, colour = colorPUs, fill = NA, size = 0.1, show.legend = FALSE)

  if (class(landmass)[[1]] == "sf") {
    gg <- gg + ggplot2::geom_sf(data = landmass, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE)
  }

  gg <- gg +
    ggplot2::theme_bw() +
    ggplot2::coord_sf(xlim = sf::st_bbox(PlanUnits)$xlim, ylim = sf::st_bbox(PlanUnits)$ylim) +
    ggplot2::labs(subtitle = plotTitle)
}


#' Basic Kernel Density Plots for climate-smart spatial plans
#' @param soln The `prioirtizr` solution containing a "metric" column containing the used climate metric information
#'
#' @importFrom rlang :=
#'
#' @return A ggplot object of the plot
#' @noRd
#' @keywords internal
#'
splnr_plot_climKernelDensity_Basic <- function(soln) {
  soln$approach <- "Ridge" # Need a dummy variable here.

  ggRidge <- ggplot2::ggplot() +
    ggridges::stat_density_ridges(
      data = soln %>% dplyr::filter(.data$solution_1 == 1) %>% dplyr::mutate(solution_1 = "Selected"),
      ggplot2::aes(x = .data$metric, y = .data$approach, fill = .data$solution_1),
      # fill = "#3182bd",
      color = "#194361", quantile_lines = TRUE, quantiles = 2,
      show.legend = TRUE
    ) +
    ggridges::stat_density_ridges(
      data = soln %>% dplyr::filter(.data$solution_1 == 0) %>% dplyr::mutate(solution_1 = "Not Selected"),
      ggplot2::aes(x = .data$metric, y = .data$approach, fill = .data$solution_1),
      # fill = "#c6dbef",
      color = "#3182bd", quantile_lines = TRUE, quantiles = 2,
      alpha = 0.5,
      show.legend = TRUE
    ) +
    ggplot2::scale_x_continuous(
      name = "Climate resilience metric",
      breaks = c(min(soln$metric), max(soln$metric)),
      labels = c("more climate-resilient", "less climate-resilient")
    ) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::labs(
      x = "Climate resilience metric",
      y = "Proportion of planning units"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.ticks = ggplot2::element_line(color = "black", size = 1),
      text = ggplot2::element_text(size = 20),
      axis.line = ggplot2::element_line(colour = "black", size = 1),
      axis.text.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 20),
      axis.title = ggplot2::element_text(size = 20),
      legend.title = ggplot2::element_text(color = "black", angle = 270, hjust = 0.5),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 20)
    ) +
    ggplot2::scale_fill_manual(
      name = "",
      values = c("Not Selected" = "#c6dbef", "Selected" = "#3182bd"),
      aesthetics = "fill",
      guide = ggplot2::guide_legend(
        override.aes = list(linetype = 0),
        nrow = 1
      )
    )
}

#' Fancy Kernel Density Plots for climate-smart spatial plans
#' @param solution_list A list of `prioirtizr` solutions (e.g. solution_list = list(s1, s2)) containing a "metric" column containing the used climate metric information
#' @param names A list of names of the solutions (names = c("Input 1", "Input 2"))
#' @param colorMap A character string indicating the color map to use (see https://ggplot2.tidyverse.org/reference/scale_viridis.html for all options)
#' @param legendTitle A character value for the title of the legend. Can be empty ("").
#' @param xAxisLab A characted value for the x Axis label depending on the climate metric input
#'
#' @return A ggplot object of the plot
#' @noRd
#' @keywords internal
#'
splnr_plot_climKernelDensity_Fancy <- function(solution_list, names,
                                               colorMap = "C",
                                               legendTitle = expression("\u0394 \u00B0C y"^"-1" * ""),
                                               xAxisLab = expression("Climate warming (\u0394 \u00B0C y"^"-1" * ")")) {
  list_sol <- list()
  group_name <- "approach"

  for (i in 1:length(names)) {
    list_sol[[i]] <- solution_list[[i]] %>%
      tibble::as_tibble() %>%
      dplyr::select("solution_1", "metric") %>%
      dplyr::rename(!!rlang::sym(names[i]) := .data$metric) %>%
      tidyr::pivot_longer(!!rlang::sym(names[i]), names_to = group_name, values_to = "metric")
  }

  df <- do.call(rbind, list_sol) %>%
    dplyr::mutate(approach = forcats::fct_relevel(.data$approach, rev))

  ggRidge <- ggplot2::ggplot() +
    ggridges::geom_density_ridges_gradient(
      data = df %>% dplyr::filter(.data$solution_1 == 1),
      ggplot2::aes(
        x = .data$metric,
        y = .data$approach,
        fill = ggplot2::after_stat(.data$x),
      ), scale = 1
    ) +
    ggplot2::scale_fill_viridis_c(name = legendTitle, option = colorMap) +
    ggridges::geom_density_ridges(
      data = df %>% dplyr::filter(.data$solution_1 == 0),
      ggplot2::aes(x = .data$metric, y = .data$approach),
      alpha = 0.25, linetype = "dotted", scale = 1
    ) +

    # geom_vline(xintercept = climate$mean_climate_warming,
    #            linetype = "dashed", color = "tan1", size = 0.5) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(mult = c(0.01, 0))) +
    ggplot2::labs(x = xAxisLab) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.ticks = ggplot2::element_line(color = "black", size = 1),
      axis.line = ggplot2::element_line(colour = "black", size = 1),
      axis.text = ggplot2::element_text(color = "black", size = 14),
      axis.title.x = ggplot2::element_text(size = 14),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      # legend.key.height = unit(1, "inch"),
      legend.text = ggplot2::element_text(size = 15, color = "black"),
      legend.title = ggplot2::element_text(size = 15, color = "black")
    )
}


#' Kernel Density Plots for climate-smart spatial plans
#' @param type The plotting style of the kernel density plots. Either "Publication" which gives axis information etc., or "App" which condenses the information in the plot to simplify it for stakeholders.
#' @param soln For type "Publication": A list of `prioirtizr` solutions (e.g. solution_list = list(s1, s2)) containing a "metric" column containing the used climate metric information; For type "App": needs to be a prioritizr solution
#' @param names A list of names of the solutions (names = c("Input 1", "Input 2"))
#' @param colorMap A character string indicating the color map to use (see https://ggplot2.tidyverse.org/reference/scale_viridis.html for all options)
#' @param legendTitle A character value for the title of the legend. Can be empty ("").
#' @param xAxisLab A characted value for the x Axis label depending on the climate metric input
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @examples
#' target <- dat_species_bin %>%
#'   dplyr::select(-"cellID") %>%
#'   sf::st_drop_geometry() %>%
#'   colnames() %>%
#'   data.frame() %>%
#'   setNames(c("feature")) %>%
#'   dplyr::mutate(target = 0.3)
#'
#' CPA <- splnr_climate_PriorityApproach(
#'   featuresDF = dat_species_bin,
#'   metricDF = dat_clim,
#'   targetsDF = target,
#'   direction = -1,
#'   refugiaTarget = 1
#' )
#'
#' out_sf <- CPA$Features  %>%
#'   dplyr::mutate(Cost_None = rep(1, 780)) %>%
#'   dplyr::left_join(dat_clim %>%
#'     sf::st_drop_geometry(), by = "cellID")
#'
#' usedFeatures <- out_sf %>%
#'   sf::st_drop_geometry() %>%
#'   dplyr::select(-tidyselect::starts_with("Cost_"), -"cellID", -"metric") %>%
#'   names()
#'
#' p1 <- prioritizr::problem(out_sf, usedFeatures, "Cost_None") %>%
#'   prioritizr::add_min_set_objective() %>%
#'   prioritizr::add_relative_targets(CPA$Targets$target) %>%
#'   prioritizr::add_binary_decisions() %>%
#'   prioritizr::add_default_solver(verbose = FALSE)
#'
#' dat_solnClim <- prioritizr::solve.ConservationProblem(p1)
#' splnr_plot_climKernelDensity(dat_solnClim, type = "Basic")
#' splnr_plot_climKernelDensity(soln = list(dat_solnClim),  names = c("Input 1"), type = "Normal")
splnr_plot_climKernelDensity <- function(soln,
                                         names = NA,
                                         type = "Normal",
                                         colorMap = "C",
                                         legendTitle = expression("\u0394 \u00B0C y"^"-1" * ""),
                                         xAxisLab = expression("Climate warming (\u0394 \u00B0C y"^"-1" * ")")) {

  if (type == "Normal") {
    if (inherits(soln,"list") == FALSE){
      cat("Please provide a list of solutions when using this plot type.")
    } else if (inherits(soln,"list")){
      ggclimDens <-  splnr_plot_climKernelDensity_Fancy(solution_list = soln, names = names, colorMap = colorMap,
                                                        legendTitle = legendTitle, xAxisLab = xAxisLab)
    }
  } else if (type == "Basic") {
    if (inherits(soln,"sf") == FALSE){
      cat("Please provide an sf object.")
    } else if (inherits(soln,"sf")){
      ggclimDens <-  splnr_plot_climKernelDensity_Basic(soln = soln)
    }
  }
}
