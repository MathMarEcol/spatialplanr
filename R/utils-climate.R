##### Climate Priority Area Approach ####

#' Function to split the feature data into climate-smart (CS) and non-climate-smart (NCS) areas depending on the percentile chosen by the user.
#'
#' @param features feature sf object
#' @param metric climate metric `sf` object with 'metric' as the column name of the metric values per planning unit.
#' @param percentile cut-off threshold for determining whether an area is a climate priority area or not (e.g., lower 35th percentile of warming or upper 65th percentile of acidification). Note that the percentile here is the lower limit of the threshold.
#' @param direction If direction = 1, metric values are from low (least climate-smart) to high (most climate-smart). If direction = -1, metric values are from high (least climate-smart) to low (most climate-smart).
#'
#' @return A new sf dataframe that has cutoffs applied.
#' @noRd
#' @keywords internal
#'
#' @importFrom rlang .data
#'
#' @examples
#'
#' out_sf <- splnr_climate_priorityArea_preprocess(
#'   features = dat_species_bin,
#'   percentile = 5,
#'   metric = dat_clim,
#'   direction = 1
#' )
splnr_climate_priorityArea_preprocess <- function(features,
                                                  percentile,
                                                  metric,
                                                  direction) {
  spp <- features %>%
    sf::st_drop_geometry() %>%
    names()

  imptList <- list() # empty list to fill in with the important features
  for (i in 1:length(spp)) {
    df <- features %>%
      dplyr::select(!!rlang::sym(spp[i])) %>% # Select 1 species at a time
      sf::st_join(metric, join = sf::st_equals)

    if (any((apply(df, 2, is.na))[, 2])) {
      print("There are some NAs in the metric data. Please check.")
    }

    # Get the most climate-smart areas
    filteredDF <- df %>%
      dplyr::filter(!!rlang::sym(spp[i]) == 1) # Select rows that have biodiversity values (= 1)

    if (direction == 1) {
      prct <- (100 - percentile) / 100 # for 100 as the most climate-smart
      qntl <- stats::quantile(filteredDF$metric, prct)[[1]] # Get the percentile

      df <- df %>%
        dplyr::mutate(
          V1 = dplyr::if_else(metric >= qntl, true = 1, false = 0),
          V2 = dplyr::if_else(!!rlang::sym(spp[i]) == 1, true = 1, false = 0)
        )
    } else if (direction == -1) {
      prct <- percentile / 100
      qntl <- stats::quantile(filteredDF$metric, prct)[[1]] # Get the percentile

      df <- df %>%
        dplyr::mutate(
          V1 = dplyr::if_else(metric <= qntl, true = 1, false = 0),
          V2 = dplyr::if_else(!!rlang::sym(spp[i]) == 1, true = 1, false = 0)
        )
    } else {
      if (i == 1) {
        print("Please enter a valid direction: either 1 or -1.")
      }
    }

    if (i > 1){
      df <- df %>%
        sf::st_drop_geometry() # Drop here otherwise we get multiple version below
    }

    imptList[[i]] <- df %>%
      dplyr::mutate(!!rlang::sym(paste0(spp[i], "_CS")) := .data$V1 * .data$V2) %>% # CS = climate-smart areas
      dplyr::select(!!rlang::sym(paste0(spp[i], "_CS")))
  }

  imptList <- do.call(dplyr::bind_cols, imptList)

  repList <- list()

  for (i in 1:length(spp)) {

    df1 <- features %>%
      dplyr::select(!!rlang::sym(spp[i])) %>% # Select 1 species at a time
      sf::st_join(metric, join = sf::st_equals)

    df2 <- imptList %>%
      dplyr::select(!!rlang::sym(paste0(spp[i], "_CS")),)

    df3 <- sf::st_join(df1, df2, join = sf::st_equals)

    if (i > 1){
      df3 <- df3 %>%
        sf::st_drop_geometry() # Drop here otherwise we get multiple version below
    }

    repList[[i]] <- df3 %>%
      dplyr::mutate(!!rlang::sym(paste0(spp[i], "_NCS")) := dplyr::if_else(!!rlang::sym(paste0(spp[i], "_CS")) == 1,
                                                                                          true = 0,
                                                                                          false = .data[[spp[i]]]
    )) %>%
      dplyr::select(tidyselect::matches("_NCS|_CS"))
  }

  repList <- do.call(dplyr::bind_cols, repList) %>%
    dplyr::select(tidyselect::everything()) %>%
    sf::st_as_sf(sf_column_name = "geometry")

  return(repList)
}


#' Function to assign targets for climate-priority-area approach
#'
#' @param targets `data.frame`with list of features under "feature" column and their corresponding targets under "target" column
#' @param climateSmartDF `sf` object produced using the function splnr_ClimatePriorityArea_CSapproach()
#' @param refugiaTarget target assigned to climate-smart areas
#'
#' @return A new sf dataframe that has cutoffs applied.
#' @noRd
#' @keywords internal
#'
#' @importFrom rlang .data
#'
#' @examples
#' Features <- dat_species_bin
#'
#' targets <- Features %>%
#'   sf::st_drop_geometry() %>%
#'   colnames() %>%
#'   data.frame() %>%
#'   setNames(c("feature")) %>%
#'   dplyr::mutate(target = 0.3)
#'
#' metric_df <- dat_clim
#'
#' dat_species_binDF <- dat_species_bin %>%
#'   sf::st_drop_geometry()
#'
#' out_sf <- splnr_climate_priorityArea_preprocess(
#'   features = dat_species_bin,
#'   percentile = 5, metric = metric_df, direction = 1
#' )
#'
#' targets <- splnr_climate_priorityArea_assignTargets(
#'   targets = targets,
#'   climateSmartDF = out_sf,
#'   refugiaTarget = 1
#' )
splnr_climate_priorityArea_assignTargets <- function(targets,
                                                     climateSmartDF,
                                                     refugiaTarget = 1) {
  spp <- targets %>%
    dplyr::select("feature") %>%
    dplyr::pull()

  featDF <- climateSmartDF %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(.x, 0))) %>%
    dplyr::summarize(dplyr::across(tidyselect::everything(), sum)) %>%
    tidyr::pivot_longer(tidyselect::everything(), names_to = "feature", values_to = "planunit")

  finalList <- list() # empty list
  for (i in 1:length(spp)) {
    filteredTarget <- targets %>% # Get the set target per feature
      dplyr::filter(.data$feature == spp[i])

    trgt <- filteredTarget$target # Extracting the target set for each feature

    vars <- c(
      stringr::str_c(spp[i], "_CS"),
      stringr::str_c(spp[i], "_NCS")
    )

    suppressMessages({
      assignTarget <- featDF %>%
        dplyr::filter(.data$feature %in% vars) %>%
        dplyr::full_join(filteredTarget)
    })

    sumUnits <- sum(assignTarget$planunit, na.rm = TRUE) # getting the total of the feature

    assignTarget1 <- assignTarget %>%
      dplyr::mutate(planunit = dplyr::if_else(stringr::str_ends(.data$feature, paste0(spp[i])), true = sumUnits, false = .data$planunit)) %>%
      dplyr::mutate(proportion = .data$planunit / sumUnits)

    reltargetCS <- assignTarget1[assignTarget1$feature == paste0(spp[i], "_CS"), "proportion"] %>% dplyr::pull() # get the relative target for the climate-smart areas

    if (reltargetCS > assignTarget1[assignTarget1$feature == spp[i], "target"]) { # Do this check; is the percentile greater than the assigned target for that feature?
      targetCS <- (assignTarget1[assignTarget1$feature == spp[i], "target"] %>% as.numeric()) / (assignTarget1[assignTarget1$feature == paste0(spp[i], "_CS"), "proportion"] %>% as.numeric())

      targetNCS <- 0
    } else {
      targetCS <- refugiaTarget
      targetNCS <- ((assignTarget1[assignTarget1$feature == spp[i], "target"] %>% as.numeric()) - reltargetCS) / (assignTarget1[assignTarget1$feature == paste0(spp[i], "_NCS"), "proportion"] %>% as.numeric())
    }

    finalList[[i]] <- assignTarget1 %>%
      dplyr::mutate(target = dplyr::case_when(
        stringr::str_ends(.data$feature, "_CS") ~ targetCS,
        stringr::str_ends(.data$feature, "_NCS") ~ targetNCS
      )) %>%
      dplyr::filter(.data$feature != spp[i]) %>%
      dplyr::select("feature", "target")
  }

  finalDF <- do.call(dplyr::bind_rows, finalList)

  return(finalDF)
}

#' Function to run the climate-priority-area approach
#'
#' @param features feature `sf`object
#' @param metric climate metric `sf` object with 'metric' as the column name of the metric values per planning unit.
#' @param targets `data.frame`with list of features under "feature" column and their corresponding targets under "target" column
#' @param refugiaTarget target assigned to climate-smart areas
#' @param direction If direction = 1, metric values are from low (least climate-smart) to high (most climate-smart). If direction = -1, metric values are from high (least climate-smart) to low (most climate-smart).
#' @param percentile cut-off threshold for determining whether an area is a climate priority area or not (e.g., lower 35th percentile of warming or upper 65th percentile of acidification). Note that the percentile here is the lower limit of the threshold.
#'
#' @return A `list` with two components: 1. is the data frame passed to `prioritizr` when creating a conservation problem containing the binary information per planning unit per feature. 2. are the targets for the features in the conservation problem when the CPA approach is used.
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#'
#' targets <- dat_species_bin %>%
#'   sf::st_drop_geometry() %>%
#'   colnames() %>%
#'   data.frame() %>%
#'   setNames(c("feature")) %>%
#'   dplyr::mutate(target = 0.3)
#'
#' CPA_Approach <- splnr_climate_priorityAreaApproach(
#'   features = dat_species_bin,
#'   metric = dat_clim,
#'   targets = targets,
#'   direction = -1
#' )
#' out_sf <- CPA_Approach$Features
#' targets <- CPA_Approach$Targets
splnr_climate_priorityAreaApproach <- function(features,
                                               metric,
                                               targets,
                                               direction,
                                               percentile = 5,
                                               refugiaTarget = 1) {

  #TODO check that geometry of both sf objects are the same.

  assertthat::assert_that(inherits(features, "sf"),
                          inherits(metric, "sf"),
                          is.data.frame(targets),
                          "feature" %in% names(targets),
                          "target" %in% names(targets),
                          direction %in% c(-1, 1),
                          is.numeric(percentile),
                          percentile >= 0 && percentile <= 100,
                          is.numeric(refugiaTarget),
                          refugiaTarget >= 0 && refugiaTarget <= 1)

  CPAFeatures <- splnr_climate_priorityArea_preprocess(
    features = features,
    metric = metric,
    direction = direction,
    percentile = percentile
  )

  CPATargets <- splnr_climate_priorityArea_assignTargets(
    targets = targets,
    CPAFeatures,
    refugiaTarget = refugiaTarget
  )
  return(list(Features = CPAFeatures, Targets = CPATargets))
}

##### Feature Approach ####

#' Function to run the feature climate-smart approach
#'
#' This function creates a climate layer by selecting the most climate-smart areas in the entire planning region.
#'
#' @param features feature `sf`object
#' @param metric climate metric `sf` object with 'metric' as the column name of the metric values per planning unit.#'
#' @param direction If direction = 1, metric values are from low (least climate-smart) to high (most climate-smart). If direction = -1, metric values are from high (least climate-smart) to low (most climate-smart).
#' @param percentile cut-off threshold for determining whether an area is a climate priority area or not. Note that the percentile here is the lower limit of the threshold.
#'
#' @return A `list` with two components: 1. is the data frame passed to `prioritizr` when creating a conservation problem containing the binary information per planning unit per feature. 2. are the targets for the features in the conservation problem when the CPA approach is used.
#'
#' @noRd
#' @keywords internal
#' @importFrom rlang .data
#'
#' @examples
#'
#' featureTest <- splnr_climate_feature_preprocess(
#'   features = dat_species_bin,
#'   percentile = 5,
#'   metric = dat_clim,
#'   direction = 1
#' )
splnr_climate_feature_preprocess <- function(features,
                                             percentile,
                                             metric,
                                             direction) {
  if (any(apply(metric, 2, is.na)[, "metric"])) {
    print("There are some NAs in the metric data. Please check.")
  }
  prct <- percentile / 100
  qntl <- stats::quantile(metric$metric, prct)[[1]] # Get the percentile
  if (direction == 1) {
    print("Higher values mean more climate-smart areas.")
    df <- metric %>%
      dplyr::mutate(climate_layer = ifelse(.data$metric >= qntl, yes = 1, no = 0)) # Create the climate layer
  } else if (direction == -1) {
    print("Lower values mean more climate-smart areas.")
    df <- metric %>%
      dplyr::mutate(climate_layer = ifelse(.data$metric <= qntl, yes = 1, no = 0)) # Create the climate layer
  } else {
    if (direction != 1 & direction != -1) {
      print("Please enter a valid direction: either 1 or -1.")
      return(NULL)
    }
  }


  # Get the most climate-smart areas
  climateSmartDF <- df %>%
    dplyr::select("climate_layer")

  # Attach "climate_layer" to the features df and have this as the output
  features <- features %>%
    sf::st_join(climateSmartDF, join = sf::st_equals)

  return(features)
}

#' Function to assign targets for the feature approach
#'
#' @param targets `data.frame`with list of features under "feature" column and their corresponding targets under "target" column
#' @param climateSmartDF `sf` object produced using the function splnr_ClimatePriorityArea_CSapproach()
#' @param refugiaTarget target assigned to climate-smart areas
#'
#' @return A new sf dataframe that has cutoffs applied.
#' @noRd
#' @keywords internal
#'
#' @importFrom rlang .data
#'
#' @examples
#' Features <- dat_species_bin
#'
#' targets <- Features %>%
#'   sf::st_drop_geometry() %>%
#'   colnames() %>%
#'   data.frame() %>%
#'   setNames(c("feature")) %>%
#'   dplyr::mutate(target = 0.3)
#'
#' dat_species_binDF <- dat_species_bin %>%
#'   sf::st_drop_geometry()
#'
#' out_sf <- splnr_climate_feature_preprocess(
#'   features = dat_species_bin,
#'   percentile = 5,
#'   metric = dat_clim,
#'   direction = 1
#' )
#'
#' targets <- splnr_climate_feature_assignTargets(
#'   targets = targets,
#'   climateSmartDF = out_sf,
#'   refugiaTarget = 0.3
#' )
splnr_climate_feature_assignTargets <- function(climateSmartDF,
                                                refugiaTarget,
                                                targets) {

  # Calculate the target depending on the # of PUs deemed as "climate-smart"
  trgt <- refugiaTarget / (sum(climateSmartDF$climate_layer) / nrow(climateSmartDF))

  climate_layerDF <- tibble::tribble(
    ~feature, ~target,
    "climate_layer", trgt
  )

  finalDF <- targets %>%
    dplyr::bind_rows(climate_layerDF) # %>%
  # dplyr::mutate(targets = .data$targets / 100) # Convert target to proportions

  return(finalDF)
}

#' Function to run the feature approach
#'
#' @param features feature `sf`object
#' @param metric climate metric `sf` object with 'metric' as the column name of the metric values per planning unit.
#' @param targets `data.frame`with list of features under "feature" column and their corresponding targets under "target" column
#' @param refugiaTarget target assigned to climate-smart areas
#' @param direction If direction = 1, metric values are from low (least climate-smart) to high (most climate-smart). If direction = -1, metric values are from high (least climate-smart) to low (most climate-smart).
#' @param percentile cut-off threshold for determining whether an area is a climate priority area or not (e.g., lower 35th percentile of warming or upper 65th percentile of acidification). Note that the percentile here is the lower limit of the threshold.
#'
#' @return A `list` with two components: 1. is the data frame passed to `prioritizr` when creating a conservation problem containing the binary information per planning unit per feature. 2. are the targets for the features in the conservation problem when the CPA approach is used.
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' Features <- dat_species_bin
#'
#' targets <- Features %>%
#'   sf::st_drop_geometry() %>%
#'   colnames() %>%
#'   data.frame() %>%
#'   setNames(c("feature")) %>%
#'   dplyr::mutate(target = 0.3)
#'
#' Feature_Approach <- splnr_climate_featureApproach(
#'   features = dat_species_bin,
#'   metric = dat_clim,
#'   targets = targets,
#'   direction = 1
#' )
#' out_sf <- Feature_Approach$Features
#' targets <- Feature_Approach$Targets
splnr_climate_featureApproach <- function(features,
                                          metric,
                                          targets,
                                          direction,
                                          percentile = 35,
                                          refugiaTarget = 0.3) {

  #TODO Check that geometry is the same in all the asserts

  assertthat::assert_that(inherits(features, "sf"),
                          inherits(metric, "sf"),
                          is.data.frame(targets),
                          "feature" %in% names(targets),
                          "target" %in% names(targets),
                          direction %in% c(-1, 1),
                          is.numeric(percentile),
                          percentile >= 0 && percentile <= 100,
                          is.numeric(refugiaTarget),
                          refugiaTarget >= 0 && refugiaTarget <= 1)


  featureFeatures <- splnr_climate_feature_preprocess(
    features = features,
    metric = metric,
    direction = direction,
    percentile = percentile
  )

  featureTargets <- splnr_climate_feature_assignTargets(
    targets = targets,
    featureFeatures, refugiaTarget = refugiaTarget
  )
  return(list(Features = featureFeatures, Targets = featureTargets))
}

##### Percentile Approach ####
#' Preprocessing for the percentile climate-smart approach
#' This function filters the species' distributions to their climate-smart areas only.
#' @param features feature sf object
#' @param metric climate metric `sf` object with 'metric' as the column name of the metric values per planning unit.
#' @param percentile cut-off threshold for determining whether an area is a climate priority area or not (e.g., lower 35th percentile of warming or upper 65th percentile of acidification). Note that the percentile here is the lower limit of the threshold.
#' @param direction If direction = 1, metric values are from low (least climate-smart) to high (most climate-smart). If direction = -1, metric values are from high (least climate-smart) to low (most climate-smart).
#'
#' @return A new sf dataframe that has cutoffs applied.
#' @noRd
#' @keywords internal
#'
#' @importFrom rlang .data
#'
#' @examples
#'
#' out_sf <- splnr_climate_percentile_preprocess(
#'   features = dat_species_bin,
#'   metric = dat_clim,
#'   percentile = 5,
#'   direction = 1
#' )
splnr_climate_percentile_preprocess <- function(features,
                                                metric,
                                                percentile,
                                                direction) {
  if (any(apply(metric, 2, is.na)[, "metric"])) {
    print("There are some NAs in the metric data. Please check.")
  }

  spp <- features %>% # Get the list of features
    sf::st_drop_geometry() %>%
    names()

  percentileList <- list()
  for (i in 1:length(spp)) {

    df <- features %>%
      dplyr::select(!!rlang::sym(spp[i]),) %>% # Select 1 feature at a time
      sf::st_join(metric, join = sf::st_equals) %>%  # Join with the metric layer
      sf::st_drop_geometry() # Drop here otherwise we get multiple version below

    filteredDF <- df %>%
      dplyr::filter(!!rlang::sym(spp[i]) == 1) # Select only areas with presences

    prct <- percentile / 100 # Convert percentiles to proportions
    qntl <- stats::quantile(filteredDF$metric, prct)[[1]] # Get the percentile
    if (direction == 1) {
      if (i == 1) {
        print("Higher values mean more climate-smart areas.") # Sanity check
      }
      df1 <- df %>%
        dplyr::mutate(
          V1 = ifelse(metric >= qntl, yes = 1, no = 0), # Filter areas of the highest xth percentile within feat's distribution
          V2 = ifelse(!!rlang::sym(spp[i]) == 1, yes = 1, no = 0)
        ) # Filter areas with the feat present in it
    } else if (direction == -1) {
      if (i == 1) {
        print("Lower values mean more climate-smart areas.") # Sanity check
      }
      df1 <- df %>%
        dplyr::mutate(
          V1 = ifelse(metric <= qntl, yes = 1, no = 0), # Filter areas of the highest xth percentile within feat's distribution
          V2 = ifelse(!!rlang::sym(spp[i]) == 1, yes = 1, no = 0)
        ) # Filter areas with the feat present in it
    } else {
      if (i == 1) {
        print("Please enter a valid direction: either 1 or -1.") # Sanity check
      }
    }

    percentileList[[i]] <- df1 %>%
      dplyr::mutate(!!rlang::sym(paste0(spp[i], "_filtered")) := .data$V1 * .data$V2) %>% # V1*V2 will be 1 if area is within the xth percentile and if a feat is present in it
      dplyr::select(!!rlang::sym(paste0(spp[i], "_filtered")))
  }

  resultDF <- do.call(dplyr::bind_cols, percentileList) %>% # Create sf object as output
    dplyr::rename_all(~ stringr::str_sub(.x, end = -10)) %>%
    dplyr::bind_cols(features %>% dplyr::select("geometry")) %>%
    dplyr::select(tidyselect::everything()) %>%
    sf::st_as_sf(sf_column_name = "geometry")

  return(resultDF)
}

#' Function to assign targets for the percentile approach
#'
#' @param features feature sf object
#' @param targets `data.frame`with list of features under "feature" column and their corresponding targets under "target" column
#' @param climateSmartDF `sf` object produced using the function `splnr_climate_percentile_preprocess()`
#'
#' @return A new sf dataframe that has cutoffs applied.
#' @noRd
#' @keywords internal
#'
#' @importFrom rlang .data
#'
#' @examples
#' Features <- dat_species_bin
#'
#' targets <- Features %>%
#'   sf::st_drop_geometry() %>%
#'   colnames() %>%
#'   data.frame() %>%
#'   setNames(c("feature")) %>%
#'   dplyr::mutate(target = 0.3)
#'
#' dat_species_binDF <- dat_species_bin %>%
#'   sf::st_drop_geometry()
#'
#' out_sf <- splnr_climate_percentile_preprocess(
#'   features = dat_species_bin,
#'   metric = dat_clim,
#'   percentile = 35,
#'   direction = 1
#' )
#'
#' targets <- splnr_climate_percentile_assignTargets(
#'   features = dat_species_bin,
#'   targets = targets,
#'   climateSmartDF = out_sf
#' )
splnr_climate_percentile_assignTargets <- function(features,
                                                   climateSmartDF,
                                                   targets) {
  spp <- features %>% # Get the list of features
    sf::st_drop_geometry() %>%
    names()

  suppressMessages({
    df <- features %>%
      sf::st_drop_geometry() %>%
      dplyr::mutate_all(~ ifelse(is.na(.), 0, .)) %>%
      tibble::as_tibble() %>%
      dplyr::summarize(dplyr::across(dplyr::everything(), sum)) %>% # Get the # of planning units where feature is present
      tidyr::pivot_longer(tidyselect::everything(), names_to = "feature", values_to = "original") %>%
      dplyr::left_join(targets) # %>%
    # dplyr::mutate(feature = paste0(.data$feature, "_filtered")) # Change names

    df1 <- climateSmartDF %>%
      sf::st_drop_geometry() %>%
      dplyr::mutate_all(~ ifelse(is.na(.), 0, .)) %>%
      tibble::as_tibble() %>%
      dplyr::summarize(dplyr::across(dplyr::everything(), sum)) %>% # Get the # of planning units selected using the climate-smart approach
      tidyr::pivot_longer(tidyselect::everything(), names_to = "feature", values_to = "filtered")

    df <- df %>%
      dplyr::left_join(df1) %>%
      dplyr::mutate(proportion = .data$filtered / .data$original) %>% # Calculating proportion of climate-smart areas over areas where feat is present
      dplyr::mutate(target = .data$target / .data$proportion) %>% # Calculate target based on the set target per feature and the proportion
      dplyr::select("feature", "target") %>%
      # dplyr::mutate(targets = .data$targets / 100) %>% # Convert target to proportions
      dplyr::mutate(target = ifelse(.data$target > 1, 1, .data$target)) # Make sure that 100% is the largest target possible
  })

  return(df)
}

#' Function to run the percentile approach
#'
#' @param features feature `sf`object
#' @param metric climate metric `sf` object with 'metric' as the column name of the metric values per planning unit.
#' @param targets `data.frame`with list of features under "feature" column and their corresponding targets under "target" column
#' @param direction If direction = 1, metric values are from low (least climate-smart) to high (most climate-smart). If direction = -1, metric values are from high (least climate-smart) to low (most climate-smart).
#' @param percentile cut-off threshold for determining whether an area is a climate priority area or not (e.g., lower 35th percentile of warming or upper 65th percentile of acidification). Note that the percentile here is the lower limit of the threshold.
#'
#' @return A `list` with two components: 1. is the data frame passed to `prioritizr` when creating a conservation problem containing the binary information per planning unit per feature. 2. are the targets for the features in the conservation problem when the CPA approach is used.
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#'
#' targets <- dat_species_bin %>%
#'   sf::st_drop_geometry() %>%
#'   colnames() %>%
#'   data.frame() %>%
#'   setNames(c("feature")) %>%
#'   dplyr::mutate(target = 0.3)
#'
#' Percentile_Approach <- splnr_climate_percentileApproach(
#'   features = dat_species_bin,
#'   metric = dat_clim,
#'   targets = targets,
#'   direction = 1
#' )
#' out_sf <- Percentile_Approach$Features
#' targets <- Percentile_Approach$Targets
splnr_climate_percentileApproach <- function(features,
                                             metric,
                                             targets,
                                             direction,
                                             percentile = 35) {

  assertthat::assert_that(inherits(features, "sf"),
                          inherits(metric, "sf"),
                          is.data.frame(targets),
                          "feature" %in% names(targets),
                          "target" %in% names(targets),
                          direction %in% c(-1, 1),
                          is.numeric(percentile),
                          percentile >= 0 && percentile <= 100)


  percentileFeatures <- splnr_climate_percentile_preprocess(
    features = features,
    metric = metric,
    direction = direction,
    percentile = percentile
  )

  percentileTargets <- splnr_climate_percentile_assignTargets(
    features = features,
    targets = targets,
    percentileFeatures
  )
  return(list(Features = percentileFeatures, Targets = percentileTargets))
}


##### Penalty Approach #####
