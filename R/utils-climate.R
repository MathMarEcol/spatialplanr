##### Climate Priority Area Approach ####

#' Function to split the feature data into climate-smart (CS) and non-climate-smart (NCS) areas depending on the percentile chosen by the user.
#'
#' @param featuresDF feature sf object which should have a column for cellID
#' @param percentile cut-off threshold for determining whether an area is a climate priority area or not (e.g., lower 35th percentile of warming or upper 65th percentile of acidification). Note that the percentile here is the lower limit of the threshold.
#' @param metricDF climate metric data.frame with 'metric' as the column name of the metric values per planning unit. This should also have a column for the cellID
#' @param direction If direction = 1, metric values are from low (least climate-smart) to high (most climate-smart). If direction = -1, metric values are from high (least climate-smart) to low (most climate-smart).
#'
#' @return A new sf dataframe that has cutoffs applied.
#' @noRd
#' @keywords internal
#'
#' @importFrom rlang .data
#'
#' @examples
#' metric_df <- dat_clim
#'
#' dat_species_binDF <- dat_species_bin %>%
#'   sf::st_drop_geometry()
#'
#' out_sf <- splnr_climate_priorityArea_preprocess(
#'   featuresDF = dat_species_bin,
#'   percentile = 5, metricDF = metric_df, direction = 1
#' )
splnr_climate_priorityArea_preprocess <- function(featuresDF,
                                                  percentile,
                                                  metricDF,
                                                  direction) {
  spp <- featuresDF %>%
    dplyr::select(-"geometry", -"cellID") %>%
    sf::st_drop_geometry() %>%
    names()

  metric <- metricDF %>%
    sf::st_drop_geometry()

  imptList <- list() # empty list to fill in with the important features
  for (i in 1:length(spp)) {
    df <- featuresDF %>%
      tibble::as_tibble() %>%
      dplyr::select(!!rlang::sym(spp[i]), "cellID") %>% # Select 1 species at a time
      dplyr::left_join(metricDF, by = "cellID") %>%
      dplyr::select(-"cellID")

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

    imptList[[i]] <- df %>%
      dplyr::mutate(!!rlang::sym(paste0(spp[i], "_CS")) := .data$V1 * .data$V2) %>% # CS = climate-smart areas
      dplyr::select(!!rlang::sym(paste0(spp[i], "_CS")))
  }

  imptList <- do.call(dplyr::bind_cols, imptList) %>%
    dplyr::mutate(cellID = dplyr::row_number())

  repList <- list()

  for (i in 1:length(spp)) {
    df1 <- featuresDF %>%
      tibble::as_tibble() %>%
      dplyr::select(!!rlang::sym(spp[i]), .data$cellID) %>% # Select 1 species at a time
      dplyr::left_join(metricDF, by = "cellID")

    df2 <- imptList %>%
      dplyr::select(!!rlang::sym(paste0(spp[i], "_CS")), "cellID")

    repList[[i]] <- dplyr::left_join(df1, df2, by = "cellID") %>%
      dplyr::mutate(!!rlang::sym(paste0(spp[i], "_NCS")) := dplyr::if_else(!!rlang::sym(paste0(spp[i], "_CS")) == 1,
        true = 0,
        false = .data[[spp[i]]]
      )) %>%
      dplyr::select(tidyselect::matches("_NCS|_CS"))
  }

  repList <- do.call(dplyr::bind_cols, repList) %>%
    dplyr::bind_cols(featuresDF %>%
      dplyr::select("cellID", "geometry")) %>%
    dplyr::select("cellID", tidyselect::everything()) %>%
    sf::st_as_sf(sf_column_name = "geometry")

  return(repList)
}


#' Function to assign targets for climate-priority-area approach
#'
#' @param targetsDF `data.frame`with list of features under "feature" column and their corresponding targets under "target" column
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
#' Features <- dat_species_bin %>%
#'   dplyr::select(-"cellID")
#'
#' target <- Features %>%
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
#'   featuresDF = dat_species_bin,
#'   percentile = 5, metricDF = metric_df, direction = 1
#' )
#'
#' targets <- splnr_climate_priorityArea_assignTargets(
#'   targetsDF = target,
#'   climateSmartDF = out_sf,
#'   refugiaTarget = 1
#' )
splnr_climate_priorityArea_assignTargets <- function(targetsDF,
                                                     climateSmartDF,
                                                     refugiaTarget = 1) {
  spp <- targetsDF %>%
    dplyr::select("feature") %>%
    dplyr::pull()

  featDF <- climateSmartDF %>%
    sf::st_drop_geometry() %>%
    dplyr::select(-"cellID") %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(.x, 0))) %>%
    dplyr::summarize(dplyr::across(tidyselect::everything(), sum)) %>%
    tidyr::pivot_longer(tidyselect::everything(), names_to = "feature", values_to = "planunit")

  finalList <- list() # empty list
  for (i in 1:length(spp)) {
    filteredTarget <- targetsDF %>% # Get the set target per feature
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
#' @param featuresDF feature `sf`object which should have a column for cellID
#' @param targetsDF `data.frame`with list of features under "feature" column and their corresponding targets under "target" column
#' @param metricDF climate metric data.frame with 'metric' as the column name of the metric values per planning unit. This should also have a column for the cellID
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
#' Features <- dat_species_bin %>%
#'   dplyr::select(-"cellID")
#'
#' target <- Features %>%
#'   sf::st_drop_geometry() %>%
#'   colnames() %>%
#'   data.frame() %>%
#'   setNames(c("feature")) %>%
#'   dplyr::mutate(target = 0.3)
#'
#' metric_df <- dat_clim
#'
#'
#' CPA_Approach <- splnr_climate_priorityAreaApproach(
#'   featuresDF = dat_species_bin,
#'   metricDF = metric_df, targetsDF = target, direction = -1
#' )
#' out_sf <- CPA_Approach$Features
#' targets <- CPA_Approach$Targets
splnr_climate_priorityAreaApproach <- function(featuresDF,
                                               metricDF,
                                               targetsDF,
                                               direction,
                                               percentile = 5,
                                               refugiaTarget = 1) {
  CPAFeatures <- splnr_climate_priorityArea_preprocess(
    featuresDF = featuresDF, metricDF = metricDF,
    direction = direction, percentile = percentile
  )

  CPATargets <- splnr_climate_priorityArea_assignTargets(
    targetsDF = targetsDF,
    CPAFeatures, refugiaTarget = refugiaTarget
  )
  return(list(Features = CPAFeatures, Targets = CPATargets))
}

##### Feature Approach ####

#' Function to run the feature climate-smart approach
#'
#' This function creates a climate layer by selecting the most climate-smart areas in the entire planning region.
#'
#' @param featuresDF feature `sf`object which should have a column for cellID
#' @param metricDF climate metric data.frame with 'metric' as the column name of the metric values per planning unit. This should also have a column for the cellID
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
#' metric_df <- dat_clim
#'
#' featureTest <- splnr_climate_feature_preprocess(
#'   featuresDF = dat_species_bin,
#'   percentile = 5, metricDF = metric_df, direction = 1
#' )
splnr_climate_feature_preprocess <- function(featuresDF,
                                             percentile,
                                             metricDF,
                                             direction) {
  if (any(apply(metricDF, 2, is.na)[, "metric"])) {
    print("There are some NAs in the metric data. Please check.")
  }
  prct <- percentile / 100
  qntl <- stats::quantile(metricDF$metric, prct)[[1]] # Get the percentile
  if (direction == 1) {
    print("Higher values mean more climate-smart areas.")
    df <- metricDF %>%
      dplyr::mutate(climate_layer = ifelse(.data$metric >= qntl, yes = 1, no = 0)) # Create the climate layer
  } else if (direction == -1) {
    print("Lower values mean more climate-smart areas.")
    df <- metricDF %>%
      dplyr::mutate(climate_layer = ifelse(.data$metric <= qntl, yes = 1, no = 0)) # Create the climate layer
  } else {
    if (direction != 1 & direction != -1) {
      print("Please enter a valid direction: either 1 or -1.")
      return(NULL)
    }
  }

  # Get the most climate-smart areas
  climateSmartDF <- df %>%
    dplyr::select("cellID", "climate_layer") %>%
    tibble::as_tibble()

  # Attach "climate_layer" to the features df and have this as the output
  featuresDF <- featuresDF %>%
    sf::st_drop_geometry() %>%
    tibble::as_tibble() %>%
    dplyr::left_join(climateSmartDF, by = "cellID") %>%
    sf::st_as_sf(geometry = featuresDF$geometry)

  return(featuresDF)
}

#' Function to assign targets for the feature approach
#'
#' @param targetsDF `data.frame`with list of features under "feature" column and their corresponding targets under "target" column
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
#' Features <- dat_species_bin %>%
#'   dplyr::select(-"cellID")
#'
#' target <- Features %>%
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
#' out_sf <- splnr_climate_feature_preprocess(
#'   featuresDF = dat_species_bin,
#'   percentile = 5, metricDF = metric_df, direction = 1
#' )
#'
#' targets <- splnr_climate_feature_assignTargets(
#'   targetsDF = target,
#'   climateSmartDF = out_sf,
#'   refugiaTarget = 0.3
#' )
splnr_climate_feature_assignTargets <- function(climateSmartDF,
                                                refugiaTarget,
                                                targetsDF) {
  # Calculate the target depending on the # of PUs deemed as "climate-smart"
  trgt <- refugiaTarget / (sum(climateSmartDF$climate_layer) / nrow(climateSmartDF))

  climate_layerDF <- tibble::tribble(
    ~feature, ~target,
    "climate_layer", trgt
  )

  finalDF <- targetsDF %>%
    dplyr::bind_rows(climate_layerDF) # %>%
  # dplyr::mutate(targets = .data$targets / 100) # Convert target to proportions

  return(finalDF)
}

#' Function to run the feature approach
#'
#' @param featuresDF feature `sf`object which should have a column for cellID
#' @param targetsDF `data.frame`with list of features under "feature" column and their corresponding targets under "target" column
#' @param metricDF climate metric data.frame with 'metric' as the column name of the metric values per planning unit. This should also have a column for the cellID
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
#' Features <- dat_species_bin %>%
#'   dplyr::select(-"cellID")
#'
#' target <- Features %>%
#'   sf::st_drop_geometry() %>%
#'   colnames() %>%
#'   data.frame() %>%
#'   setNames(c("feature")) %>%
#'   dplyr::mutate(target = 0.3)
#'
#' metric_df <- dat_clim
#'
#'
#' Feature_Approach <- splnr_climate_featureApproach(
#'   featuresDF = dat_species_bin,
#'   metricDF = metric_df, targetsDF = target, direction = 1
#' )
#' out_sf <- Feature_Approach$Features
#' targets <- Feature_Approach$Targets
splnr_climate_featureApproach <- function(featuresDF,
                                          metricDF,
                                          targetsDF,
                                          direction,
                                          percentile = 35,
                                          refugiaTarget = 0.3) {
  featureFeatures <- splnr_climate_feature_preprocess(
    featuresDF = featuresDF, metricDF = metricDF,
    direction = direction, percentile = percentile
  )

  featureTargets <- splnr_climate_feature_assignTargets(
    targetsDF = targetsDF,
    featureFeatures, refugiaTarget = refugiaTarget
  )
  return(list(Features = featureFeatures, Targets = featureTargets))
}

##### Percentile Approach ####
#' Preprocessing for the percentile climate-smart approach
#' This function filters the species' distributions to their climate-smart areas only.
#' @param featuresDF feature sf object which should have a column for cellID
#' @param percentile cut-off threshold for determining whether an area is a climate priority area or not (e.g., lower 35th percentile of warming or upper 65th percentile of acidification). Note that the percentile here is the lower limit of the threshold.
#' @param metricDF climate metric data.frame with 'metric' as the column name of the metric values per planning unit. This should also have a column for the cellID
#' @param direction If direction = 1, metric values are from low (least climate-smart) to high (most climate-smart). If direction = -1, metric values are from high (least climate-smart) to low (most climate-smart).
#'
#' @return A new sf dataframe that has cutoffs applied.
#' @noRd
#' @keywords internal
#'
#' @importFrom rlang .data
#'
#' @examples
#' metric_df <- dat_clim
#'
#' dat_species_binDF <- dat_species_bin %>%
#'   sf::st_drop_geometry()
#'
#' out_sf <- splnr_climate_percentile_preprocess(
#'   featuresDF = dat_species_bin,
#'   percentile = 5, metricDF = metric_df, direction = 1
#' )
splnr_climate_percentile_preprocess <- function(featuresDF,
                                                percentile,
                                                metricDF,
                                                direction) {
  if (any(apply(metricDF, 2, is.na)[, "metric"])) {
    print("There are some NAs in the metric data. Please check.")
  }

  spp <- featuresDF %>% # Get the list of features
    tibble::as_tibble() %>%
    dplyr::select(-"geometry", -"cellID") %>%
    names()

  metric <- metricDF %>% # Make sure metric df is a data.frame
    dplyr::mutate(cellID = dplyr::row_number()) %>%
    sf::st_drop_geometry() %>%
    tibble::as_tibble()

  percentileList <- list()
  for (i in 1:length(spp)) {
    df <- featuresDF %>%
      tibble::as_tibble() %>%
      dplyr::select(!!rlang::sym(spp[i]), "cellID") %>% # Select 1 feature at a time
      dplyr::left_join(metric, by = "cellID") %>% # Join with the metric layer
      dplyr::select(-"cellID")

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
    dplyr::bind_cols(featuresDF %>% dplyr::select("cellID", "geometry")) %>%
    dplyr::select("cellID", tidyselect::everything()) %>%
    sf::st_as_sf(sf_column_name = "geometry")

  return(resultDF)
}

#' Function to assign targets for the percentile approach
#'
#' @param featuresDF feature sf object which should have a column for cellID
#' @param targetsDF `data.frame`with list of features under "feature" column and their corresponding targets under "target" column
#' @param climateSmartDF `sf` object produced using the function `splnr_climate_percentile_preprocess()`
#'
#' @return A new sf dataframe that has cutoffs applied.
#' @noRd
#' @keywords internal
#'
#' @importFrom rlang .data
#'
#' @examples
#' Features <- dat_species_bin %>%
#'   dplyr::select(-"cellID")
#'
#' target <- Features %>%
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
#' out_sf <- splnr_climate_percentile_preprocess(
#'   featuresDF = dat_species_bin,
#'   percentile = 35, metricDF = metric_df, direction = 1
#' )
#'
#' targets <- splnr_climate_percentile_assignTargets(
#'   featuresDF = dat_species_bin,
#'   targetsDF = target,
#'   climateSmartDF = out_sf
#' )
splnr_climate_percentile_assignTargets <- function(featuresDF,
                                                   climateSmartDF,
                                                   targetsDF) {
  spp <- featuresDF %>% # Get the list of features
    tibble::as_tibble() %>%
    dplyr::select(-"cellID", -"geometry") %>%
    names()

  suppressMessages({
    df <- featuresDF %>%
      sf::st_drop_geometry() %>%
      dplyr::mutate_all(~ ifelse(is.na(.), 0, .)) %>%
      tibble::as_tibble() %>%
      dplyr::select(-"cellID") %>%
      dplyr::summarize(dplyr::across(dplyr::everything(), sum)) %>% # Get the # of planning units where feature is present
      tidyr::pivot_longer(tidyselect::everything(), names_to = "feature", values_to = "original") %>%
      dplyr::left_join(targetsDF) # %>%
    # dplyr::mutate(feature = paste0(.data$feature, "_filtered")) # Change names

    df1 <- climateSmartDF %>%
      sf::st_drop_geometry() %>%
      dplyr::mutate_all(~ ifelse(is.na(.), 0, .)) %>%
      tibble::as_tibble() %>%
      dplyr::select(-"cellID") %>%
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
#' @param featuresDF feature `sf`object which should have a column for cellID
#' @param targetsDF `data.frame`with list of features under "feature" column and their corresponding targets under "target" column
#' @param metricDF climate metric data.frame with 'metric' as the column name of the metric values per planning unit. This should also have a column for the cellID
#' @param direction If direction = 1, metric values are from low (least climate-smart) to high (most climate-smart). If direction = -1, metric values are from high (least climate-smart) to low (most climate-smart).
#' @param percentile cut-off threshold for determining whether an area is a climate priority area or not (e.g., lower 35th percentile of warming or upper 65th percentile of acidification). Note that the percentile here is the lower limit of the threshold.
#'
#' @return A `list` with two components: 1. is the data frame passed to `prioritizr` when creating a conservation problem containing the binary information per planning unit per feature. 2. are the targets for the features in the conservation problem when the CPA approach is used.
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' Features <- dat_species_bin %>%
#'   dplyr::select(-"cellID")
#'
#' target <- Features %>%
#'   sf::st_drop_geometry() %>%
#'   colnames() %>%
#'   data.frame() %>%
#'   setNames(c("feature")) %>%
#'   dplyr::mutate(target = 0.3)
#'
#' metric_df <- dat_clim
#'
#'
#' Percentile_Approach <- splnr_climate_percentileApproach(
#'   featuresDF = dat_species_bin,
#'   metricDF = metric_df, targetsDF = target, direction = 1
#' )
#' out_sf <- Percentile_Approach$Features
#' targets <- Percentile_Approach$Targets
splnr_climate_percentileApproach <- function(featuresDF,
                                             metricDF,
                                             targetsDF,
                                             direction,
                                             percentile = 35) {
  percentileFeatures <- splnr_climate_percentile_preprocess(
    featuresDF = featuresDF, metricDF = metricDF,
    direction = direction, percentile = percentile
  )

  percentileTargets <- splnr_climate_percentile_assignTargets(
    featuresDF = featuresDF,
    targetsDF = targetsDF,
    percentileFeatures
  )
  return(list(Features = percentileFeatures, Targets = percentileTargets))
}
##### Penalty Approach #####
