utils::globalVariables(".")

#' Function to split the feature data into climate-smart (CS) and non-climate-smart (NCS) areas depending on the percentile chosen by the user.
#'
#' @param featuresDF feature sf object which should have a column for cellID
#' @param percentile cut-off threshold for determining whether an area is a climate priority area or not (e.g., lower 35th percentile of warming or upper 65th percentile of acidification). Note that the percentile here is the lower limit of the threshold.
#' @param metricDF climate metric data.frame with 'metric' as the column name of the metric values per planning unit. This should also have a column for the cellID
#' @param direction If direction = 1, metric values are from low (least climate-smart) to high (most climate-smart). If direction = -1, metric values are from high (least climate-smart) to low (most climate-smart).
#'
#' @return A new sf dataframe that has cutoffs applied.
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' metric_df <- dat_clim
#'
#' dat_species_binDF <- dat_species_bin %>%
#' sf::st_drop_geometry()
#'
#' out_sf <- splnr_ClimatePriorityArea_CSapproach (featuresDF = dat_species_bin,
#'         percentile = 5, metricDF = metric_df, direction = 1)
splnr_ClimatePriorityArea_CSapproach <- function(featuresDF,
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
  for(i in 1:length(spp)) {

    df <- featuresDF %>%
      tibble::as_tibble() %>%
      dplyr::select(!!rlang::sym(spp[i]), "cellID") %>% # Select 1 species at a time
      dplyr::left_join(., metricDF, by = "cellID") %>%
      dplyr::select(-"cellID")

    if(any((apply(df, 2, is.na))[,2])) {
      print("There are some NAs in the metric data. Please check.")
    }

    # Get the most climate-smart areas
    filteredDF <- df %>%
      dplyr::filter(!!rlang::sym(spp[i]) == 1) # Select rows that have biodiversity values (= 1)

    if(direction == 1) {
      prct <- (100-percentile)/100 # for 100 as the most climate-smart
      qntl <- stats::quantile(filteredDF$metric, prct)[[1]] # Get the percentile

      df <- df %>%
        dplyr::mutate(V1 = dplyr::if_else(metric >= qntl, true = 1, false = 0),
                      V2 = dplyr::if_else(!!rlang::sym(spp[i]) == 1, true = 1, false = 0))
    } else if(direction == -1) {
      prct <- percentile/100
      qntl <- stats::quantile(filteredDF$metric, prct)[[1]] # Get the percentile

      df <- df %>%
        dplyr::mutate(V1 = dplyr::if_else(metric <= qntl, true = 1, false = 0),
                      V2 = dplyr::if_else(!!rlang::sym(spp[i]) == 1, true = 1, false = 0))
    } else {
      if(i == 1) {
        print("Please enter a valid direction: either 1 or -1.")
      }
    }

    imptList[[i]] <- df %>%
      dplyr::mutate(!!rlang::sym(paste0(spp[i], "_CS")) := .data$V1 * .data$V2) %>%  # CS = climate-smart areas
      dplyr::select(!!rlang::sym(paste0(spp[i], "_CS")))

  }

  imptList <- imptList %>%
    do.call(dplyr::bind_cols, .) %>%
    dplyr::mutate(cellID = dplyr::row_number())

  repList <- list()
  for(i in 1:length(spp)) {

    df1 <- featuresDF %>%
      tibble::as_tibble() %>%
      dplyr::select(!!rlang::sym(spp[i]), .data$cellID) %>% # Select 1 species at a time
      dplyr::left_join(., metricDF, by = "cellID")

    df2 <- imptList %>%
      dplyr::select(!!rlang::sym(paste0(spp[i], "_CS")), "cellID")

    repList[[i]] <- dplyr::left_join(df1, df2, by = "cellID") %>%
      dplyr::mutate(!!rlang::sym(paste0(spp[i], "_NCS")) := dplyr::if_else(!!rlang::sym(paste0(spp[i], "_CS")) == 1,
                                                                           true = 0,
                                                                           false = .data [[ spp[i] ]])) %>%
      dplyr::select(tidyselect::matches('_NCS|_CS'))
  }

  repList <- repList %>%
    do.call(dplyr::bind_cols, .) %>%
    dplyr::bind_cols(., featuresDF %>%
                       dplyr::select("cellID", "geometry")) %>%
    dplyr::select("cellID", tidyselect::everything()) %>%
    sf::st_as_sf(sf_column_name = "geometry")

  return(repList)

}


#' Function to assign targets for climate-priority-area approach
#'
#' @param featuresDF feature `sf`object which should have a column for cellID
#' @param targetsDF `data.frame`with list of features under "feature" column and their corresponding targets under "target" column
#' @param climateSmartDF `sf` object produced using the function splnr_ClimatePriorityArea_CSapproach()
#' @param refugiaTarget target assigned to climate-smart areas
#'
#' @return A new sf dataframe that has cutoffs applied.
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
#' dat_species_binDF <- dat_species_bin %>%
#' sf::st_drop_geometry()
#'
#' out_sf <- splnr_ClimatePriorityArea_CSapproach(featuresDF = dat_species_bin,
#'         percentile = 5, metricDF = metric_df, direction = 1)
#'
#' target <- splnr_CPA_CSapproach_assignTargets(featuresDF = Features,
#'                              targetsDF = target,
#'                              climateSmartDF = out_sf,
#'                             refugiaTarget = 1)
splnr_CPA_CSapproach_assignTargets <- function(featuresDF,
                                               targetsDF,
                                               climateSmartDF,
                                               refugiaTarget = 1) {

  spp <- targetsDF %>%
    dplyr::select("feature") %>%
    dplyr::pull()

  featDF <- climateSmartDF %>%
    sf::st_drop_geometry() %>%
    dplyr::select(-"cellID") %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~tidyr::replace_na(.x, 0))) %>%
    dplyr::summarize(dplyr::across(tidyselect::everything(), sum)) %>%
    tidyr::pivot_longer(tidyselect::everything(), names_to = "feature", values_to = "planunit")

  finalList <- list() # empty list
  for(i in 1:length(spp)) {

    filteredTarget <- targetsDF %>% # Get the set target per feature
      dplyr::filter(.data$feature == spp[i])

    trgt <- filteredTarget$target # Extracting the target set for each feature

    vars <- c(stringr::str_c(spp[i], "_CS"),
              stringr::str_c(spp[i], "_NCS"))

    suppressMessages({
      assignTarget <- featDF %>%
        dplyr::filter(.data$feature %in% vars) %>%
        dplyr::full_join(., filteredTarget)
    })

    sumUnits <- sum(assignTarget$planunit, na.rm = TRUE) # getting the total of the feature

    assignTarget1 <- assignTarget %>%
      dplyr::mutate(planunit = dplyr::if_else(stringr::str_ends(.data$feature, paste0(spp[i])), true = sumUnits, false = .data$planunit)) %>%
      dplyr::mutate(proportion = .data$planunit/sumUnits)

    reltargetCS <- assignTarget1[assignTarget1$feature == paste0(spp[i], "_CS"), "proportion"] %>% dplyr::pull()  # get the relative target for the climate-smart areas

    if(reltargetCS > assignTarget1[assignTarget1$feature == spp[i], "target"]) { # Do this check; is the percentile greater than the assigned target for that feature?
      targetCS <- (assignTarget1[assignTarget1$feature == spp[i], "target"] %>% as.numeric())/(assignTarget1[assignTarget1$feature == paste0(spp[i], "_CS"), "proportion"] %>% as.numeric())

      targetNCS <- 0
    } else {
      targetCS <- refugiaTarget
      targetNCS <- ((assignTarget1[assignTarget1$feature == spp[i], "target"] %>% as.numeric()) - reltargetCS)/(assignTarget1[assignTarget1$feature == paste0(spp[i], "_NCS"), "proportion"] %>% as.numeric())

    }

    finalList[[i]] <- assignTarget1 %>%
      dplyr::mutate(target = dplyr::case_when(stringr::str_ends(.data$feature, "_CS") ~ targetCS,
                                              stringr::str_ends(.data$feature, "_NCS") ~ targetNCS)) %>%
      dplyr::filter(.data$feature != spp[i]) %>%
      dplyr::select("feature", "target")
  }

  finalDF <- do.call(dplyr::bind_rows, finalList)

  return(finalDF)
}
