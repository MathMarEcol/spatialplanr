#' Prepare data to plot how well targets are met
#'
#' @param soln The `prioritizr` solution
#' @param pDat The `prioritizr` problem
#' @param climsmart logical denoting whether spatial planning was done climate-smart (and targets have to be calculated differently)
#' @param solnCol Name of the column with the solution
#'
#' @return `tbl_df` dataframe
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' pDat <- prioritizr::problem(dat_species_bin %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
#'   features = c("Spp1", "Spp2", "Spp3"),
#'   cost_column = "Cost"
#' ) %>%
#'   prioritizr::add_min_set_objective() %>%
#'   prioritizr::add_relative_targets(0.3) %>%
#'   prioritizr::add_binary_decisions() %>%
#'   prioritizr::add_default_solver(verbose = FALSE)
#'
#' soln <- pDat %>%
#'   prioritizr::solve.ConservationProblem()
#'
#' df <- splnr_get_featureRep(
#'   soln = soln,
#'   pDat = pDat
#' )
splnr_get_featureRep <- function(soln, pDat,
                                 climsmart = FALSE, solnCol = "solution_1") {
  s_cols <- pDat$data$features[[1]]

  # Get data for features not chosen
  not_selected <- soln %>%
    dplyr::select(
      -tidyselect::starts_with(c("Cost", "solution_")),
      -tidyselect::any_of(c("metric", "cellID")),
      -tidyselect::any_of(s_cols)
    ) %>%
    sf::st_drop_geometry()

  ns_cols <- not_selected %>%
    colnames()

  if (length(ns_cols) > 0) {
    ns1 <- not_selected %>%
      dplyr::select(c(tidyselect::all_of(ns_cols))) %>%
      dplyr::mutate(solution = dplyr::pull(soln, !!rlang::sym(solnCol)))

    area_feature <- ns1 %>%
      dplyr::select(-c("solution")) %>%
      tidyr::pivot_longer(cols = tidyselect::everything(), names_to = "feature", values_to = "total_amount") %>%
      dplyr::group_by(.data$feature) %>%
      dplyr::summarise(total_amount = sum(.data$total_amount))

    selected_feature <- ns1 %>%
      dplyr::filter(.data$solution == 1) %>%
      dplyr::select(-c("solution")) %>%
      tidyr::pivot_longer(cols = tidyselect::everything(), names_to = "feature", values_to = "absolute_held") %>%
      dplyr::group_by(.data$feature) %>%
      dplyr::summarise(absolute_held = sum(.data$absolute_held))

    ns1 <- dplyr::left_join(area_feature, selected_feature, by = "feature") %>%
      dplyr::mutate(
        relative_held = (.data$absolute_held / .data$total_amount),
        incidental = TRUE
      )
  } else {
    ns1 <- tibble::tibble(
      feature = "DummyVar",
      total_amount = 0,
      absolute_held = 0,
      relative_held = 0,
      incidental = TRUE
    )
  }

  ## Now do the selected features

  s1 <- soln %>%
    dplyr::rename(solution = !!rlang::sym(solnCol)) %>%
    tibble::as_tibble()

  s1 <- prioritizr::eval_feature_representation_summary(pDat, s1[, "solution"]) %>%
    dplyr::select(-"summary")

  if (climsmart == TRUE) {
    s1 <- s1 %>%
      dplyr::select(-.data$relative_held) %>%
      dplyr::mutate(
        feature = stringr::str_remove_all(.data$feature, "_CS"),
        feature = stringr::str_remove_all(.data$feature, "_NCS")
      ) %>% # Ensure all features have the same name.
      dplyr::group_by(.data$feature) %>%
      dplyr::summarise(
        total_amount = sum(.data$total_amount), # Sum the features together
        absolute_held = sum(.data$absolute_held)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(relative_held = .data$absolute_held / .data$total_amount) %>% # Calculate proportion
      dplyr::select(-.data$total_amount, -.data$absolute_held) # Remove extra columns
  }

  s1 <- s1 %>%
    dplyr::mutate(
      relative_held = .data$relative_held,
      incidental = FALSE
    ) %>%
    stats::na.omit()

  # Add targets to df
  s1 <- s1 %>%
    dplyr::left_join(pDat$targets$data[["targets"]], by = "feature") %>%
    dplyr::select(-"type")

  # Now join the selected and non-selected values
  if ((length(ns_cols) > 0)) { # Only if there are values in ns1
    df <- dplyr::bind_rows(s1, ns1)
  } else {
    df <- s1
  }

  return(df)
}




# Targets Bar Plot --------------------------------------------------------



#' Plot how well targets are met
#'
#' @param df A `df` containing the target information (resulting from the splnr_get_featureRep() function)
#' @param nr Number of rows of the legend
#' @param plotTitle A character value for the title of the plot. Can be empty ("").
#' @param category A named charcter vector of feature and category for grouping the plot output
#' @param showTarget `logical` Should the targets be shown on the bar plot
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @examples
#' pDat <- prioritizr::problem(dat_species_bin %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
#'   features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
#'   cost_column = "Cost"
#' ) %>%
#'   prioritizr::add_min_set_objective() %>%
#'   prioritizr::add_relative_targets(0.3) %>%
#'   prioritizr::add_binary_decisions() %>%
#'   prioritizr::add_default_solver(verbose = FALSE)
#'
#' soln <- pDat %>%
#'   prioritizr::solve.ConservationProblem()
#'
#'
#' # including incidental species coverage
#' df <- splnr_get_featureRep(
#'   soln = soln,
#'   pDat = pDat
#' )
#'
#' (splnr_plot_featureRep(df, category = dat_category))
#'
splnr_plot_featureRep <- function(df, category = NA,
                                  nr = 1, showTarget = NA,
                                  plotTitle = "") {
  if (is.data.frame(category)) {
    df <- df %>%
      dplyr::left_join(category, by = "feature") %>%
      dplyr::arrange(.data$category, .data$feature) %>%
      dplyr::mutate(feature = factor(.data$feature, levels = .data$feature))
  }

  if (max(df$relative_held < 1)) {
    df <- df %>%
      dplyr::mutate(
        relative_held = .data$relative_held * 100,
        target = .data$target * 100
      )
  }

  uniqueCat <- unique(df$category[!is.na(df$category)])

  colr <- tibble::tibble(
    Category = uniqueCat,
    Colour = viridis::viridis(length(uniqueCat))
  ) %>%
    tibble::deframe()

  gg_target <- ggplot2::ggplot() +
    ggplot2::geom_bar(data = df, stat = "identity", ggplot2::aes(x = .data$feature, y = .data$relative_held, fill = .data$category), na.rm = TRUE) +
    ggplot2::geom_bar(data = df %>% dplyr::filter(.data$incidental == TRUE), stat = "identity", ggplot2::aes(x = .data$feature, y = .data$relative_held), na.rm = TRUE, fill = "NA", colour = "black") +
    ggplot2::labs(title = plotTitle, x = "Feature", y = "Representation of features \nin total selected area (%)") +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(limits = c(0, ymax <- max(df$relative_held, na.rm = TRUE) + 10), expand = c(0, 0)) + # only works for min shortfall without incidental yet
    ggplot2::scale_fill_manual(
      values = colr,
      guide = ggplot2::guide_legend(nrow = nr)
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 16, colour = "black"),
      axis.text.y = ggplot2::element_text(size = 16, colour = "black"),
      axis.title.x = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 16),
      axis.title.y = ggplot2::element_text(size = 16),
      title = ggplot2::element_text(size = 16),
      legend.position = c(0.5, 0.95),
      legend.direction = "horizontal",
      legend.background = ggplot2::element_rect(fill = "NA")
    )

  if (!(is.na(showTarget))) {
    gg_target <- gg_target +
      ggplot2::geom_point(data = df, ggplot2::aes(x = .data$feature, y = .data$target), shape = 3, size = 10, na.rm = TRUE)
  }

  return(gg_target)
}



# Circular Bar Plot -------------------------------------------------------



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
#' # DISCLAIMER: THIS SOLUTION IS NOT ACTUALLY RUN WITH THESE TARGETS YET
#'
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
#' s1 <- dat_soln %>%
#'   tibble::as_tibble()
#'
#' p1 <- dat_problem
#'
#' df_rep_imp <- prioritizr::eval_feature_representation_summary(
#'   p1,
#'   s1[, "solution_1"]
#' ) %>%
#'   dplyr::select(feature, relative_held) %>%
#'   dplyr::mutate(relative_held = relative_held * 100)
#'
#' imp_layers <- c("Spp1", "Spp3")
#'
#' target <- data.frame(feature = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5")) %>%
#'   dplyr::mutate(class = dplyr::if_else(.data$feature %in% imp_layers,
#'     "important", "representative"
#'   )) %>%
#'   dplyr::mutate(target = dplyr::if_else(class == "important",
#'     50 / 100, 30 / 100
#'   ))
#'
#' df <- merge(df_rep_imp, target) %>%
#'   dplyr::select(-target) %>%
#'   na.omit() %>%
#'   dplyr::rename(value = relative_held) %>%
#'   dplyr::rename(group = class)
#'
#' colors <- c(
#'   "important" = "darkgreen",
#'   "representative" = "darkred"
#' )
#' legends <- c("Important", "Representative")
#'
#' (splnr_plot_circBplot(df,
#'   legend_list = legends,
#'   legend_color = colors,
#'   impTarget = 50, repTarget = 30
#' ))
splnr_plot_circBplot <- function(df, legend_color, legend_list,
                                 indicateTargets = TRUE, impTarget = NA,
                                 repTarget = NA, colTarget = "red") {
  # Adding rows to each group, creating space between the groups
  groups <- unique(df$group)
  NA_rows <- list()
  for (i in 1:length(groups)) {
    NA_rows[[i]] <- data.frame(feature = NA, value = 0, group = groups[i])
  }

  data <- df %>%
    dplyr::bind_rows(do.call(dplyr::bind_rows, NA_rows)) %>%
    dplyr::group_by(.data$group) %>%
    dplyr::arrange(.data$feature)

  # Set a number of 'empty bar' to add at the end of each group
  empty_bar <- 2
  to_add <- data.frame(matrix(NA, empty_bar * length(unique(data$group)), ncol(data)))
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
  angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bar # Subtracting 0.5 so the labels are not found in the extreme left or right
  # Calculate the alignment of labels: right or left
  # If I am on the left part of the plot, my labels have currently an angle < -90
  label_data$hjust <- ifelse(angle < -90, 1, 0)
  # Flip angle BY to make them readable
  label_data$angle <- ifelse(angle < -90, angle + 180, angle)

  # For the percentage lines
  grid_data <- data %>%
    dplyr::group_by(.data$group) %>%
    dplyr::summarize(start = min(.data$id), end = max(.data$id) - empty_bar) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(title = mean(c(.data$start, .data$end)))
  grid_data$end <- grid_data$end[c(nrow(grid_data), 1:nrow(grid_data) - 1)] + 1.5
  grid_data$start <- grid_data$end - 1
  grid_data <- grid_data[-1, ]

  # Make the plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = as.factor(.data$id), y = .data$value, fill = .data$group)) +

    # plotting the bars
    ggplot2::geom_bar(ggplot2::aes(x = as.factor(.data$id), y = .data$value, fill = .data$group),
      stat = "identity",
      position = "dodge"
    ) +

    # defining colors of the bars
    ggplot2::scale_fill_manual(
      name = "Features",
      values = legend_color,
      labels = legend_list
    ) +

    # Add text showing the value of each 100/75/50/25 lines
    ggplot2::geom_segment(
      data = grid_data,
      ggplot2::aes(x = .data$end, y = 25, xend = .data$start, yend = 25),
      colour = "grey50",
      alpha = 1,
      linewidth = 0.5,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_segment(
      data = grid_data,
      ggplot2::aes(x = .data$end, y = 50, xend = .data$start, yend = 50),
      colour = "grey50",
      alpha = 1,
      linewidth = 0.5,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_segment(
      data = grid_data,
      ggplot2::aes(x = .data$end, y = 75, xend = .data$start, yend = 75),
      colour = "grey50",
      alpha = 1,
      linewidth = 0.5,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_segment(
      data = grid_data,
      ggplot2::aes(x = .data$end, y = 100, xend = .data$start, yend = 100),
      colour = "grey50",
      alpha = 1,
      linewidth = 0.5,
      inherit.aes = FALSE
    ) +
    ggplot2::annotate("text",
      x = rep(max(data$id - 1), 4),
      y = c(25, 50, 75, 100),
      label = c(25, 50, 75, 100),
      color = "grey50",
      size = 4,
      angle = 0, #-5
      fontface = "bold",
      hjust = 0.5
    ) +

    # setting limitations of actual plot
    ggplot2::ylim(-130, 130) + #-140, 130
    ggplot2::theme_minimal() +
    ggplot2::coord_polar() +
    ggplot2::geom_text(
      data = label_data, ggplot2::aes(
        x = .data$id, y = .data$value + 10, label = .data$feature,
        hjust = .data$hjust
      ), color = "black",
      fontface = "bold", alpha = 0.6, size = 2.5, angle = label_data$angle,
      inherit.aes = FALSE
    ) +

    # # Defining colors of these lines
    # ggplot2::scale_color_manual(name = "Features",
    #                             values = palette) +

    ggplot2::theme(
      legend.position = "bottom",
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.margin = ggplot2::unit(rep(0.5, 4), "cm")
    )

  if (indicateTargets == TRUE) {
    if (is.na(impTarget) | is.na(repTarget)) {
      print("Please provide the targets you want to indicate.")
    }
    p <- p +
      ggplot2::geom_abline(slope = 0, intercept = impTarget, col = colTarget, lty = 2) +
      ggplot2::geom_abline(slope = 0, intercept = repTarget, col = colTarget, lty = 2)
  }
}
