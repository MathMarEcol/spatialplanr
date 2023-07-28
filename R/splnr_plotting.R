
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

  if (class(landmass)[[1]] == "sf"){
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
    ggplot2::geom_sf(data = df, ggplot2::aes(fill = .data$wdpa), colour = "grey80", size = 0.1, show.legend = showLegend)

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

  if (class(landmass)[[1]] == "sf"){
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
    ggplot2::geom_sf(data = Cost, ggplot2::aes_string(fill = Cost_name), alpha = 0.5, colour = NA, size = 0.0001)+
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
    )

  if (class(landmass)[[1]] == "sf"){
    gg <- gg +
      ggplot2::geom_sf(data = landmass, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE)
  }

  gg <- gg +
    ggplot2::coord_sf(xlim = sf::st_bbox(soln)$xlim, ylim = sf::st_bbox(soln)$ylim) +
    ggplot2::labs(subtitle = plotTitle)
}


#' Plot binary feature
#'
#' @param df A `data frame` with binary feature information
#' @param colInterest column of data frame that contains binary information of feature to plot
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
#' (splnr_plot_binFeature(dat_species_bin,  dat_species_bin$Spp1, dat_PUs))
splnr_plot_binFeature <- function(df, colInterest, PlanUnits, landmass = NA,
                                  colorVals = c("Suitable" = "#3182bd", "Not Suitable" = "#c6dbef"),
                                  colorPUs = "grey80", showLegend = TRUE,
                                  plotTitle = " ", legendTitle = "Habitat"){

  df <-  df %>%
    dplyr::mutate(pred_bin = ifelse(is.na(colInterest), 0, colInterest),
                  pred_bin = dplyr::if_else(.data$pred_bin == 1, "Suitable", "Not Suitable"),
                  pred_bin = factor(.data$pred_bin, levels = c("Suitable", "Not Suitable")))

  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = df, ggplot2::aes(fill = .data$pred_bin), colour = NA, size = 0.001, show.legend = showLegend) +
    ggplot2::geom_sf(data = PlanUnits, colour = colorPUs, fill = NA, size = 0.1, show.legend = FALSE)

  if (class(landmass)[[1]] == "sf"){
    gg <- gg + ggplot2::geom_sf(data = landmass, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE)
  }

  gg <- gg +
    ggplot2::coord_sf(xlim = sf::st_bbox(PlanUnits)$xlim, ylim = sf::st_bbox(PlanUnits)$ylim) +
    ggplot2::scale_colour_manual(name = legendTitle,
                                 values = colorVals,
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

#' Plot how well targets are met
#'
#' @param df A `df` containing the target information (resulting from the splnr_prepTargetData() function)
#' @param nr Number of rows of the legend
#' @param setTarget A number in percent (%) if all the features have the same set target
#' @param plotTitle A character value for the title of the plot. Can be empty ("").
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @examples
#' #not including incidental species coverage
#' dfNInc <- splnr_prepTargetData(soln = dat_soln, pDat = dat_problem, allDat = dat_species_bin,
#'                           Category = Category_vec, solnCol = "solution_1")
#'
#' (splnr_plot_targets(dfNInc, nr = 1, setTarget = 30, plotTitle = "Target: "))
#' #including incidental species coverage
#' dfInc <- splnr_prepTargetData(soln = dat_soln, pDat = dat_problem, allDat = dat_species_bin2,
#'                          Category = Category_vec2, solnCol = "solution_1")
#' (splnr_plot_targets(dfInc, nr = 1, setTarget = 30, plotTitle = "Target: "))
splnr_plot_targets <- function(df, nr = 1, setTarget = NA,
                               plotTitle = "") {

  uniqueCat <- unique(df$category[!is.na(df$category)])

  colr <- tibble::tibble(Category = uniqueCat,
                         Colour = viridis::viridis(length(uniqueCat))) %>%
    tibble::deframe()

  df <- df %>%
    dplyr::filter(.data$feature != "DummyVar")

  gg_target <- ggplot2::ggplot() +
    ggplot2::geom_bar(data = df, stat = "identity", ggplot2::aes(x = .data$feature, y = .data$value, fill = .data$category), na.rm = TRUE) +
    ggplot2::geom_bar(data = df, stat = "identity", ggplot2::aes(x = .data$feature, y = .data$incidental_held), na.rm = TRUE, fill = "NA", colour = "black") +
    ggplot2::labs(title = plotTitle, x = "Feature", y = "Representation of features \nin total selected area (%)") +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(limits = c(0, ymax <- max(c(df$value, df$incidental_held), na.rm = TRUE) + 10), expand = c(0,0)) + #only works for min shortfall without incidental yet
    ggplot2::scale_fill_manual(values = colr,
                               guide = ggplot2::guide_legend(nrow = nr)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 16, colour = "black"),
                   axis.text.y = ggplot2::element_text(size = 16, colour = "black"),
                   axis.title.x = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 16),
                   axis.title.y = ggplot2::element_text(size = 16),
                   title = ggplot2::element_text(size = 16),
                   legend.position = c(0.5, 0.95),
                   legend.direction = "horizontal",
                   legend.background = ggplot2::element_rect(fill = "NA"))

  if (!(is.na(setTarget))) {
    gg_target <- gg_target +
      ggplot2::geom_abline(slope = 0, intercept = setTarget, col = "black", lty = 2, size = 1.5) +
      ggplot2::labs(title = paste0(plotTitle, setTarget, "%"))
  }

}

#' Plot circular barplot
# Inputs:
#' @param df data frame that should have the following column names: feature, value, group
# feature: individual bars
# value: value plotted in the y-axis
# group: grouping factors
#' @param legend_color vector list of colors; should have the group names and their corresponding colors
#' @param legend_list list of groups/legends of groups
#' @param indicateTargets logical on whether to show where the targets were set
#' @param impTarget target of the important features (in %)
#' @param repTarget target of the representative features (in %)
#' @param colTarget string with a colour value for the indicator line
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @examples
#' s1 <- dat_soln %>% #DISCLAIMER: THIS SOLUTION IS NOT ACTUALLY RUN WITH THESE TARGETS YET
#'   tibble::as_tibble()
#'
#' p1 <- dat_problem
#'
#' df_rep_imp <- prioritizr::eval_feature_representation_summary(p1, s1[, 'solution_1'])%>%
#'   dplyr::select(feature, relative_held) %>%
#'   dplyr::mutate(relative_held = relative_held*100)
#'
#' imp_layers <- c("Spp1", "Spp3")
#'
#' target <- data.frame(feature = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5")) %>%
#'   dplyr::mutate(class = dplyr::if_else(.data$feature %in% imp_layers, "important", "representative")) %>%
#'   dplyr::mutate(target = dplyr::if_else(class == "important", 50/100, 30/100))
#'
#' df <- merge(df_rep_imp, target) %>%
#'   dplyr::select(-target) %>%
#'   na.omit() %>%
#'   dplyr::rename(value = relative_held) %>%
#'   dplyr::rename(group = class)
#'
#' colors <- c('important' = 'darkgreen',
#'             'representative' = 'darkred')
#' legends <- c('Important', 'Representative')
#'
#' (splnr_plot_circBplot(df, legend_list = legends, legend_color = colors, impTarget = 50, repTarget = 30))
splnr_plot_circBplot <- function(df, legend_color, legend_list, indicateTargets = TRUE,
                              impTarget = NA, repTarget = NA, colTarget = "red") {

  # Adding rows to each group, creating space between the groups
  groups <- unique(df$group)
  NA_rows <- list()
  for(i in 1:length(groups)) {
    NA_rows[[i]] <- data.frame(feature = NA, value = 0, group = groups[i])
  }

  data <- df %>%
    dplyr::bind_rows(do.call(dplyr::bind_rows, NA_rows)) %>%
    dplyr::group_by(.data$group) %>%
    dplyr::arrange(.data$feature)

  # Set a number of 'empty bar' to add at the end of each group
  empty_bar <- 2
  to_add <- data.frame(matrix(NA, empty_bar * length(unique(data$group)), ncol(data)) )
  colnames(to_add) <- colnames(data)
  to_add$group <- rep(levels(as.factor(data$group)), each = empty_bar)
  data <- rbind(data, to_add)
  data <- data %>% dplyr::arrange(.data$group)
  data$id <- seq(1, nrow(data))

  # Labels for each of the bars (features)

  # Get the name and the y position of each label
  label_data <- data
  # Calculate the angle of the labels
  number_of_bar <- nrow(label_data)
  angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar # Subtracting 0.5 so the labels are not found in the extreme left or right
  # Calculate the alignment of labels: right or left
  # If I am on the left part of the plot, my labels have currently an angle < -90
  label_data$hjust<-ifelse( angle < -90, 1, 0)
  # Flip angle BY to make them readable
  label_data$angle<-ifelse(angle < -90, angle+180, angle)

  # For the percentage lines
  grid_data <- data %>%
    dplyr::group_by(.data$group) %>%
    dplyr::summarize(start = min(.data$id), end = max(.data$id) - empty_bar) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(title = mean(c(.data$start, .data$end)))
  grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1.5
  grid_data$start <- grid_data$end - 1
  grid_data <- grid_data[-1,]

  # Make the plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = as.factor(.data$id), y = .data$value, fill = .data$group)) +

    # plotting the bars
    ggplot2::geom_bar(ggplot2::aes(x = as.factor(.data$id), y = .data$value, fill = .data$group),
                      stat = "identity",
                      position = 'dodge') +

    # defining colors of the bars
    ggplot2::scale_fill_manual(name = "Features",
                               values = legend_color,
                               labels = legend_list) +

    # Add text showing the value of each 100/75/50/25 lines
    ggplot2::geom_segment(data = grid_data,
                          ggplot2::aes(x = .data$end, y = 25, xend = .data$start, yend = 25),
                          colour = "grey50",
                          alpha = 1,
                          size = 0.5 ,
                          inherit.aes = FALSE ) +
    ggplot2::geom_segment(data = grid_data,
                          ggplot2::aes(x = .data$end, y = 50, xend = .data$start, yend = 50),
                          colour = "grey50",
                          alpha = 1,
                          size = 0.5,
                          inherit.aes = FALSE ) +
    ggplot2::geom_segment(data = grid_data,
                          ggplot2::aes(x = .data$end, y = 75, xend = .data$start, yend = 75),
                          colour = "grey50",
                          alpha = 1,
                          size = 0.5,
                          inherit.aes = FALSE ) +
    ggplot2::geom_segment(data = grid_data,
                          ggplot2::aes(x = .data$end, y = 100, xend = .data$start, yend = 100),
                          colour = "grey50",
                          alpha = 1,
                          size = 0.5,
                          inherit.aes = FALSE ) +
    ggplot2::annotate("text", x = rep(max(data$id-1),4),
                      y = c(25, 50, 75, 100),
                      label = c(25, 50, 75, 100),
                      color = "grey50",
                      size= 4,
                      angle = 0, #-5
                      fontface = "bold",
                      hjust=0.5) +

    # setting limitations of actual plot
    ggplot2::ylim(-130,130) + #-140, 130
    ggplot2::theme_minimal() +
    ggplot2::coord_polar() +

    ggplot2::geom_text(data = label_data, ggplot2::aes(x = .data$id, y = .data$value + 10, label = .data$feature,
                                                       hjust = .data$hjust), color = "black",
                       fontface = "bold", alpha = 0.6, size = 2.5, angle = label_data$angle,
                       inherit.aes = FALSE ) +

    # # Defining colors of these lines
    # ggplot2::scale_color_manual(name = "Features",
    #                             values = palette) +

    ggplot2::theme(
      legend.position = "bottom",
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.margin = ggplot2::unit(rep(0.5,4), "cm")
    )

  if (indicateTargets == TRUE) {
    if(is.na(impTarget) | is.na(repTarget)) {
      print("Please provide the targets you want to indicate.")
    }
    p <- p +
      ggplot2::geom_abline(slope=0, intercept= impTarget,  col = colTarget,lty=2)  +
      ggplot2::geom_abline(slope=0, intercept= repTarget,  col = colTarget,lty=2)
  }
}



#' Plot solution comparison
#'
#' @param soln1 The first `prioritizr` solution
#' @param soln2 The second `prioritizr` solution
#' @param landmass An `sf` object of land polygon
#' @param PlanUnits Planning Units as an `sf` object
#' @param colorPUs A color value for the outline of planning units.
#' @param legendTitle A character value for the title of the legend. Can be empty ("").
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' (splnr_plot_comparison(dat_soln, dat_soln2))
splnr_plot_comparison <- function(soln1, soln2, landmass = NA, PlanUnits = NA, colorPUs = "grey80",
                                  legendTitle = "Scenario 2 compared to Scenario 1:"){

  soln <- soln1 %>%
    dplyr::select("solution_1") %>%
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

  if (class(landmass)[[1]] == "sf"){
    gg <- gg +
      ggplot2::geom_sf(data = landmass, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE)
  }

  if (class(PlanUnits)[[1]] == "sf"){
    gg <- gg + ggplot2::geom_sf(data = PlanUnits, colour = colorPUs, fill = NA, size = 0.1, show.legend = FALSE) +
      ggplot2::coord_sf(xlim = sf::st_bbox(PlanUnits)$xlim, ylim = sf::st_bbox(PlanUnits)$ylim)

  } else {

    gg <- gg +
      ggplot2::coord_sf(xlim = sf::st_bbox(soln)$xlim, ylim = sf::st_bbox(soln)$ylim)
  }

  gg <- gg +
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
#' @importFrom rlang .data
#' @examples
#' (splnr_plot_featureNo(dat_species_bin))
splnr_plot_featureNo <- function(df, landmass = NA,
                                 colorPUs = "grey80", showLegend = TRUE,
                                 paletteName = "YlGnBu",
                                 plotTitle = "Number of Features", legendTitle = "Features"){

  df <- df %>%
    dplyr::as_tibble() %>%
    dplyr::select(-tidyselect::any_of(c("cellID"))) %>%
    # NOTE I have changed tidyselect:::where() to where. I think I added it as a global function somewhere else so it shouldn't be needed here....
    dplyr::mutate(FeatureSum = rowSums(dplyr::across(where(is.numeric)), na.rm = TRUE)) %>%
    sf::st_as_sf(sf_column_name = "geometry") %>%
    dplyr::select(.data$FeatureSum)

  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = df, ggplot2::aes(fill = .data$FeatureSum), colour = colorPUs, size = 0.1, show.legend = showLegend)

  if (class(landmass)[[1]] == "sf"){
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

#' Plot selection frequency of a planning unit in an array of prioritisations
#'
#' @param selFreq An `sf` object containing the selection frequency of a planning unit from an array of solutions
#' @param landmass An `sf` object of land polygon
#' @param paletteName A string (or number) for the color palette to use. Available palettes can be found at https://ggplot2.tidyverse.org/reference/scale_brewer.html.
#' @param plotTitle A character value for the title of the plot. Can be empty ("").
#' @param legendTitle A character value for the title of the legend. Can be empty ("").
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' dat_soln_portfolio <- dat_problem %>%
#'   prioritizr::add_top_portfolio(number_solutions = 5) %>% #create conservation problem that contains a portfolio of solutions
#'   prioritizr:::solve.ConservationProblem()
#'
#' selFreq <- dat_soln_portfolio %>%  # calculate selection frequency
#' sf::st_drop_geometry() %>%
#' dplyr::mutate(selFreq = as.factor(rowSums(dplyr::select(., dplyr::starts_with("solution_"))))) %>%
#' sf::st_as_sf(geometry = dat_soln_portfolio$geometry) %>%
#' dplyr::select(selFreq)
#'
#' (splnr_plot_selectionFreq(selFreq))
#' }
splnr_plot_selectionFreq <- function(selFreq, landmass = NA,
                                     plotTitle = "", paletteName = "Greens",
                                     legendTitle = "Selection \nFrequency"
){
  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = selFreq, ggplot2::aes(fill = .data$selFreq), colour = NA) +

    if (class(landmass)[[1]] == "sf"){
      gg <- gg +
        ggplot2::geom_sf(data = landmass, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE)
    }

  gg <- gg +
    ggplot2::scale_fill_brewer(name = legendTitle,
                               palette = paletteName, aesthetics = "fill", #c("colour", "fill"),
                               guide = ggplot2::guide_legend(override.aes = list(linetype = 0),
                                                             title.position = "top")) +
    ggplot2::coord_sf(xlim = c(sf::st_bbox(selFreq)$xmin, sf::st_bbox(selFreq)$xmax),
                      ylim = c(sf::st_bbox(selFreq)$ymin, sf::st_bbox(selFreq)$ymax),
                      expand = TRUE) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 16, colour = "black"),
      axis.text.x = ggplot2::element_text(size = 16, colour = "black"),
      axis.title.x = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(size = 16),
      legend.text = ggplot2::element_text(size = 16),
      axis.title.y = ggplot2::element_blank()) +
    ggplot2::scale_x_continuous(expand = c(0,0)) +
    ggplot2::scale_y_continuous(expand = c(0,0)) +
    ggplot2::labs(title = plotTitle)

}

#' Plot Ferrier importance score (ONLY WORKS FOR MINIMUM SET OBJECTIVE FUNCTION)
#' @param soln The `prioritizr` solution
#' @param pDat The `prioritizr` problem
#' @param colorMap A character string indicating the color map to use (see https://ggplot2.tidyverse.org/reference/scale_viridis.html for all options)
#' @param plotTitle A character value for the title of the plot. Can be empty ("").
#' @param legendTitle A character value for the title of the legend. Can be empty ("").
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' (splnr_plot_impScoreFerrierPlot(dat_soln, dat_problem))
splnr_plot_impScoreFerrierPlot <- function(soln, pDat, plotTitle = "", colorMap = "A",
                                           legendTitle = "Importance Score \n(Ferrier Score)"
){
  soln <- soln %>% tibble::as_tibble()
  fsoln <- prioritizr::eval_ferrier_importance(pDat, soln[, "solution_1"])

  fsoln <- fsoln%>%
    dplyr::select("total") %>%
    dplyr::mutate(geometry = soln$geometry) %>%
    sf::st_as_sf()

  selectedfs <- fsoln %>%
    dplyr::filter(.data$total != 0)

  quant95fs <- round(stats::quantile(selectedfs$total, 0.95), 4)
  seq95fs <- seq(0,quant95fs, length.out = 5)
  lab <- c(seq95fs[1], seq95fs[2], seq95fs[3], seq95fs[4], paste0("\u2265", quant95fs, sep = " "))

  fsoln$total[fsoln$total >=quant95fs] <- quant95fs

  gg_fs <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = fsoln, ggplot2::aes(fill = .data$total), colour = NA) +
    ggplot2::scale_fill_viridis_c(option = colorMap,
                                  direction = -1, breaks = seq95fs, labels = lab,
                                  guide = ggplot2::guide_colourbar(title.position = "right", title = legendTitle,
                                                                   barwidth = 2, barheight = 20))+#, oob=squish)
    ggplot2::coord_sf(xlim = c(sf::st_bbox(fsoln)$xmin, sf::st_bbox(fsoln)$xmax),
                      ylim = c(sf::st_bbox(fsoln)$ymin, sf::st_bbox(fsoln)$ymax),
                      expand = TRUE) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.title = ggplot2::element_text(angle = -90, hjust = 0.5),
      text = ggplot2::element_text(size = 20),
      axis.title = ggplot2::element_blank()) +
    ggplot2::scale_x_continuous(expand = c(0,0)) +
    ggplot2::scale_y_continuous(expand = c(0,0)) +
    ggplot2::labs(title = plotTitle)
}

#' Plot Rarity weighted richness importance score (can be used with minimum shortfall OBJECTIVE FUNCTION)
#' @param soln The `prioritizr` solution
#' @param pDat The `prioritizr` problem
#' @param colorMap A character string indicating the color map to use (see https://ggplot2.tidyverse.org/reference/scale_viridis.html for all options)
#' @param plotTitle A character value for the title of the plot. Can be empty ("").
#' @param legendTitle A character value for the title of the legend. Can be empty ("").
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' (splnr_plot_impScoreRWRPlot(dat_soln, dat_problem))
splnr_plot_impScoreRWRPlot <- function(soln, pDat, plotTitle = "", colorMap = "A",
                                       legendTitle = "Importance Score \n(Rarity Weighted Richness Score)"
){
  soln <- soln %>% tibble::as_tibble()
  rwrsoln <- prioritizr::eval_rare_richness_importance(pDat, soln[, "solution_1"]) %>%
    dplyr::mutate(geometry = soln$geometry) %>%
    sf::st_as_sf()

  selectedRWR <- rwrsoln %>%
    dplyr::filter(.data$rwr != 0)

  quant95 <- round(stats::quantile(selectedRWR$rwr, 0.95), 2) #get importance score at 95th percentile of all selected planning units
  seq95 <- seq(0,quant95, length.out = 5)
  lab <- c(seq95[1], seq95[2], seq95[3], seq95[4], paste0("\u2265", quant95, sep = " "))

  rwrsoln$rwr[rwrsoln$rwr >=quant95] <- quant95

  gg_impScore <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = rwrsoln, ggplot2::aes(fill = .data$rwr), colour = NA) +
    ggplot2::scale_fill_viridis_c(option = colorMap,
                                  direction = -1, breaks = seq95, labels = lab,
                                  guide = ggplot2::guide_colourbar(title.position = "right", title = legendTitle,
                                                                   barwidth = 2, barheight = 20))+#, oob=squish)
    ggplot2::coord_sf(xlim = c(sf::st_bbox(rwrsoln)$xmin, sf::st_bbox(rwrsoln)$xmax),
                      ylim = c(sf::st_bbox(rwrsoln)$ymin, sf::st_bbox(rwrsoln)$ymax),
                      expand = TRUE) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.title = ggplot2::element_text(angle = -90, hjust = 0.5),
      text = ggplot2::element_text(size = 20),
      axis.title = ggplot2::element_blank()) +
    ggplot2::scale_x_continuous(expand = c(0,0)) +
    ggplot2::scale_y_continuous(expand = c(0,0)) +
    ggplot2::labs(title = plotTitle)
}

#' Replacement cost importance score (takes too long to use in App)
#' @param soln The `prioritizr` solution
#' @param pDat The `prioritizr` problem
#' @param colorMap A character string indicating the color map to use (see https://ggplot2.tidyverse.org/reference/scale_viridis.html for all options)
#' @param plotTitle A character value for the title of the plot. Can be empty ("").
#' @param legendTitle A character value for the title of the legend. Can be empty ("").
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' (splnr_plot_impScoreRCPlot(dat_soln, dat_problem))
#' }
splnr_plot_impScoreRCPlot <- function(soln, pDat, plotTitle = "", colorMap = "A",
                                      legendTitle = "Importance Score \n(Replacement Cost Score)"
){
  soln <- soln %>% tibble::as_tibble()
  rcsoln <- prioritizr::eval_replacement_importance(pDat, soln[, "solution_1"]) %>%
    dplyr::mutate(geometry = soln$geometry) %>%
    sf::st_as_sf()

  selectedRC <- rcsoln %>%
    dplyr::filter(.data$rc != 0)

  quant95 <- round(stats::quantile(selectedRC$rc, 0.95), 2) #get importance score at 95th percentile of all selected planning units
  seq95 <- seq(0,quant95, length.out = 5)
  lab <- c(seq95[1], seq95[2], seq95[3], seq95[4], paste0("\u2265", quant95, sep = " "))

  rcsoln$rc[rcsoln$rc >=quant95] <- quant95

  gg_impScore <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = rcsoln, ggplot2::aes(fill = .data$rc), colour = NA) +
    ggplot2::scale_fill_viridis_c(option = colorMap,
                                  direction = -1, breaks = seq95, labels = lab,
                                  guide = ggplot2::guide_colourbar(title.position = "right", title = legendTitle,
                                                                   barwidth = 2, barheight = 20))+#, oob=squish)
    ggplot2::coord_sf(xlim = c(sf::st_bbox(rcsoln)$xmin, sf::st_bbox(rcsoln)$xmax),
                      ylim = c(sf::st_bbox(rcsoln)$ymin, sf::st_bbox(rcsoln)$ymax),
                      expand = TRUE) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.title = ggplot2::element_text(angle = -90, hjust = 0.5),
      text = ggplot2::element_text(size = 20),
      axis.title = ggplot2::element_blank()) +
    ggplot2::scale_x_continuous(expand = c(0,0)) +
    ggplot2::scale_y_continuous(expand = c(0,0)) +
    ggplot2::labs(title = plotTitle)
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
