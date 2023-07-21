
#' Plot prioritizr solution
#'
#' @param soln The `prioritizr` solution
#' @param PlanUnits Planning Units as an `sf` object
#' @param landmass An `sf` object of land polygon
#' @param colorVals A `list` object of named vectors that will match the color value with the according name. "TRUE" stands for selected planning units.
#' @param colorPUs A color value for the outline of planning units.
#' @param showLegend A logical command on whether to show the legend of the solution (Default: TRUE).
#' @param plotTitle A character value for the title of the plot. Can be empty ("").
#' @param legendTitle A character value for the title of the legend. Can be empty ("").
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @examples
#' (splnr_plot_Solution(dat_soln, dat_PUs))
# dat_soln %>%
#     splnr_plot_Solution(dat_soln)
splnr_plot_Solution <- function(soln, PlanUnits, landmass = NA,
                                colorVals = c("TRUE" = "#3182bd", "FALSE" = "#c6dbef"),
                                colorPUs = "grey80", showLegend = TRUE,
                                plotTitle = "Solution", legendTitle = "Planning Units"
                                ){

  soln <- soln %>%
    dplyr::select(.data$solution_1) %>%
    dplyr::mutate(solution_1 = as.logical(.data$solution_1)) # Making it logical helps with the plotting

  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = soln, ggplot2::aes(fill = .data$solution_1), colour = NA, size = 0.1, show.legend = showLegend) +
    ggplot2::geom_sf(data = PlanUnits, colour = colorPUs, fill = NA, size = 0.1, show.legend = FALSE)

  if (!is.na(landmass)){
    gg <- gg + ggplot2::geom_sf(data = landmass, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE)
  }

  gg <- gg +
    ggplot2::coord_sf(xlim = sf::st_bbox(PlanUnits)$xlim, ylim = sf::st_bbox(PlanUnits)$ylim) +
    ggplot2::scale_colour_manual(name = legendTitle,
                                 values = colorVals,
                                 labels = c("Not Selected", "Selected"),
                                 aesthetics = "fill", #c("colour", "fill"),
                                 guide = ggplot2::guide_legend(override.aes = list(linetype = 0),
                                                               nrow = 2,
                                                               order = 1,
                                                               direction = "horizontal",
                                                               title.position = "top",
                                                               title.hjust = 0.5)) +
    ggplot2::theme_bw() +
    ggplot2::labs(subtitle = plotTitle)

}


#' Plot Planning Units
#'
#' @param PlanUnits Planning Units as an `sf` object
#' @param landmass An `sf` object of land polygon
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @examples
#' (splnr_plot_PUs(dat_PUs))
splnr_plot_PUs <- function(PlanUnits, landmass = NA){
  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = PlanUnits, colour = "grey80", fill = NA, size = 0.1, show.legend = FALSE)

  if (class(landmass)[[1]] == "sf"){
    gg <- gg + ggplot2::geom_sf(data = landmass, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE)
  }

  gg <- gg +
    ggplot2::coord_sf(xlim = sf::st_bbox(PlanUnits)$xlim, ylim = sf::st_bbox(PlanUnits)$ylim) +
    ggplot2::theme_bw() +
    ggplot2::labs(subtitle = "Planning Units")

}


#' Plot MPAs
#'
#' @param df An `sf` object of marine protected areas
#' @param landmass An `sf` object of land polygon
#' @param colorVals A `list` object of named vectors that will match the color value with the according name. "TRUE" stands for selected planning units.
#' @param colorPUs A color value for the outline of planning units.
#' @param showLegend A logical command on whether to show the legend of the solution (Default: TRUE).
#' @param plotTitle A character value for the title of the plot. Can be empty ("").
#' @param legendTitle A character value for the title of the legend. Can be empty ("").
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @examples
#' (splnr_plot_MPAs(dat_mpas))
splnr_plot_MPAs <- function(df, landmass = NA,
                            colorVals = c("TRUE" = "blue", "FALSE" = "white"),
                            colorPUs = "grey80", showLegend = TRUE,
                            plotTitle = "Locked In Areas", legendTitle = ""){

  if (isa(df$wdpa, "logical") == FALSE){
    df <- df %>%
      dplyr::mutate(wdpa = as.logical(.data$wdpa))
  }

  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = df, ggplot2::aes(fill = .data$wdpa), colour = "grey80", size = 0.1, show.legend = showLegend) +

  if (class(landmass)[[1]] == "sf"){
    gg <- gg + ggplot2::geom_sf(data = landmass, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE)
  }

  gg<- gg + ggplot2::scale_colour_manual(values = c("TRUE" = "blue",
                                            "FALSE" = "grey50")) +
    ggplot2::scale_fill_manual(name = legendTitle,
                               values = colorVals,
                               labels = c("No MPA", "MPA"),
                               aesthetics = "fill", #c("colour", "fill"),
                               guide = ggplot2::guide_legend(override.aes = list(linetype = 0),
                                                             nrow = 2,
                                                             order = 1,
                                                             direction = "horizontal",
                                                             title.position = "top",
                                                             title.hjust = 0.5)) +
    ggplot2::theme_bw() +
    ggplot2::coord_sf(
      xlim = sf::st_bbox(df)$xlim,
      ylim = sf::st_bbox(df)$ylim) +
    ggplot2::labs(subtitle = plotTitle)

}


#' Plot cost
#'
#' @param Cost An `sf` object of cost for `prioritizr`
#' @param Cost_name Name of the cost column
#' @param landmass An `sf` object of land polygon
#' @param paletteName A string (or number) for the color palette to use. Available palettes can be found at https://ggplot2.tidyverse.org/reference/scale_brewer.html.
#' @param plotTitle A character value for the title of the plot. Can be empty ("").
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @examples
#' dat_cost <- dat_soln %>%
#'               dplyr::mutate(Cost = runif(n = dim(.)[[1]]))
#' (splnr_plot_cost(dat_cost))
splnr_plot_cost <- function(Cost, Cost_name = "Cost", landmass = NA,
                            paletteName = "YlGnBu", plotTitle = "Cost (USD)"){

  # col_name = stringr::str_subset(colnames(Cost), "geometry", negate = TRUE)

  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = Cost, ggplot2::aes_string(fill = Cost_name), colour = "grey80", size = 0.1, show.legend = TRUE)

  if (!is.na(landmass)){
    gg <- gg +
      ggplot2::geom_sf(data = landmass, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE)
  }

  gg <- gg +
    ggplot2::coord_sf(xlim = sf::st_bbox(Cost)$xlim, ylim = sf::st_bbox(Cost)$ylim) +
    ggplot2::scale_fill_distiller(palette = paletteName,
                                  aesthetics = c("colour", "fill"),
                                  limits = c(0,
                                             as.numeric(stats::quantile(dplyr::pull(Cost, Cost_name), 0.99))),
                                  oob = scales::squish) +
    ggplot2::theme_bw() +
    ggplot2::labs(subtitle = plotTitle)

}

#' Plot cost overlay
#'
#' @param soln The `prioritizr` solution
#' @param Cost An `sf` object of cost for `prioritizr`.In case `prioritizr`solution does not contain cost, alternative cost object has to be provided here that was used to generate solution (default: NA).
#' @param Cost_name Name of the cost column
#' @param landmass An `sf` object of land polygon
#' @param plotTitle A character value for the title of the plot. Can be empty ("").
#' @param legendTitle A character value for the title of the legend. Can be empty ("").
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @examples
#' (splnr_plot_costOverlay(soln = dat_soln))
splnr_plot_costOverlay <- function(soln, Cost = NA, Cost_name = "Cost", landmass = NA,
                                   legendTitle = "Cost",
                                   plotTitle = "Solution overlaid with cost"){

  if (!is.data.frame(get("Cost"))){ #potentially needed for app later
    if(! Cost_name %in% colnames(soln)) {
      cat("Cost column not found. Please check your solution data frame for your column of interest.");
    } else {
      Cost <- soln %>%
        dplyr::select(!!rlang::sym(Cost_name))
    }
  }

  soln <- soln %>%
    dplyr::select(.data$solution_1) %>%
    dplyr::filter(.data$solution_1 == 1)

  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = soln, fill = "black", colour = NA, size = 0.0001) +
    ggplot2::geom_sf(data = Cost, ggplot2::aes_string(fill = Cost_name), alpha = 0.5, colour = NA, size = 0.0001)

  if (!is.na(landmass)){
    gg <- gg +
      ggplot2::geom_sf(data = landmass, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE)
  }

  gg <- gg +
    ggplot2::scale_fill_gradient(name = legendTitle,
                                 # palette = "Oranges",
                                 low = "#fff5eb",
                                 high = "#d94801", #"#f16913",
                                 limits = c(0,
                                            as.numeric(stats::quantile(dplyr::pull(Cost, Cost_name), 0.99))),
                                 # direction = 1,
                                 # oob = scales::squish,
                                 # guide = ggplot2::guide_colourbar(
                                 #   title.position = "bottom",
                                 #   title.hjust = 0.5,
                                 #   order = 1,
                                 #   barheight = grid::unit(0.03, "npc"),
                                 #   barwidth = grid::unit(0.25, "npc"))
    ) +
    ggplot2::labs(subtitle = plotTitle)
}


#' Plot solution comparison
#'
#' @param soln1 The first `prioritizr` solution
#' @param soln2 The second `prioritizr` solution
#' @param landmass An `sf` object of land polygon
#' @param legendTitle A character value for the title of the legend. Can be empty ("").
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' (splnr_plot_comparison(dat_soln, dat_soln2))
splnr_plot_comparison <- function(soln1, soln2, landmass = NA,
                                  legendTitle = "Scenario 2 compared to Scenario 1:"){

  soln <- soln1 %>%
    dplyr::select(.data$solution_1) %>%
    dplyr::bind_cols(soln2 %>%
                       dplyr::as_tibble() %>%
                       dplyr::select(.data$solution_1) %>%
                       dplyr::rename(solution_2 = .data$solution_1)) %>%
    dplyr::mutate(Combined = .data$solution_1 + .data$solution_2) %>%
    dplyr::mutate(Compare = dplyr::case_when(Combined == 2 ~ "Same",
                                             solution_1 == 1 & solution_2 == 0 ~ "Removed (-)",
                                             solution_1 == 0 & solution_2 == 1 ~ "Added (+)"),
                  Compare = factor(.data$Compare, levels = c("Added (+)", "Same", "Removed (-)"))) %>%
    dplyr::filter(!is.na(.data$Compare))

  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = soln, ggplot2::aes(fill = .data$Compare), colour = NA, size = 0.0001)

  if (!is.na(landmass)){
    gg <- gg +
      ggplot2::geom_sf(data = landmass, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE)
  }

  gg <- gg +
    ggplot2::coord_sf(xlim = sf::st_bbox(soln)$xlim, ylim = sf::st_bbox(soln)$ylim) +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_manual(name=legendTitle,
                               values = c("Added (+)" = "Red", "Same" = "ivory3", "Removed (-)" = "Blue"), drop = FALSE)

}


#' Plot number of features
#'
#' @param df An `sf` object of features
#' @param landmass An `sf` object of land polygon
#' @param paletteName A string (or number) for the color palette to use. Available palettes can be found at https://ggplot2.tidyverse.org/reference/scale_brewer.html.
#' @param colorPUs A color value for the outline of planning units.
#' @param showLegend A logical command on whether to show the legend of the solution (Default: TRUE).
#' @param plotTitle A character value for the title of the plot. Can be empty ("").
#' @param legendTitle A character value for the title of the legend. Can be empty ("").
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @examples
#' (splnr_plot_featureNo(dat_species_bin))
splnr_plot_featureNo <- function(df, landmass = NA,
                                 colorPUs = "grey80", showLegend = TRUE,
                                 paletteName = "YlGnBu",
                                 plotTitle = "Number of Features", legendTitle = "Features"){

  if("cellID" %in% colnames(df)) {
    df <- df %>%
      dplyr::select(-cellID)
  }

  df <- df %>%
    dplyr::as_tibble() %>%
    # NOTE I have changed tidyselect:::where() to where. I think I added it as a global function somewhere else so it shouldn't be needed here....
    dplyr::mutate(FeatureSum = rowSums(dplyr::across(where(is.numeric)), na.rm = TRUE)) %>%
    sf::st_as_sf(sf_column_name = "geometry") %>%
    dplyr::select(.data$FeatureSum)

  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = df, ggplot2::aes(fill = .data$FeatureSum), colour = colorPUs, size = 0.1, show.legend = showLegend)

  if (!is.na(landmass)){
    gg <- gg +
      ggplot2::geom_sf(data = landmass, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE)
  }

  gg <- gg +
    ggplot2::coord_sf(xlim = sf::st_bbox(df)$xlim, ylim = sf::st_bbox(df)$ylim) +
    ggplot2::scale_fill_distiller(name = legendTitle,
                                  palette = paletteName,
                                  aesthetics = c("fill"),
                                  # limits = c(0,
                                  #            as.numeric(quantile(Cost$Cost, 0.99))),
                                  oob = scales::squish#,
                                  #trans = "log10" #produces infinity if cells have 0 features
                                  ) +
    ggplot2::theme_bw() +
    ggplot2::labs(subtitle = plotTitle)

}

#' Plot Longhurst Provinces
#'
#' @param PlanUnits Planning Units as an `sf` object
#' @param landmass An `sf` object of land polygon
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @examples
splnr_plot_longhurst <- function(PlanUnits, landmass){
  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = PlanUnits, colour = "grey80", ggplot2::aes(fill = .data$ProvDescr), size = 0.1, show.legend = TRUE) +
    ggplot2::geom_sf(data = landmass, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    ggplot2::coord_sf(xlim = sf::st_bbox(PlanUnits)$xlim, ylim = sf::st_bbox(PlanUnits)$ylim) +
    ggplot2::theme_bw() +
    ggplot2::labs(subtitle = "Longhurst Provinces")

}
