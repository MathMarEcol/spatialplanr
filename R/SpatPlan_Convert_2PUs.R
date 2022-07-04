
# 1) Input can be string (file) or data.
# a) If a file, we check the extension and load that way and then go to step b.
# b) If data, we check the format and process that way (skipping Step a)

# 2) Are the data continuous?
# a) YES - Change the data to 0-1 then interpolate to PUs to get a spatial mean (not binary).
# i) If the data are raster we do exactextractr::exact_extract - or use terra::extract
# ii) If the data are polygon we do.....
# b) NO - Get the % overlap of the data to the PUs and return value
# i) If the data are raster we do exactextractr::exact_extract with a specialised function
# ii) If the data are polygon we do.....


#' Function to interpolate data onto Planning Units
#'
#' The dataset needs to be raster or vector format.
#' If the input contains continuous data, the output is an area-averaged mean for each planning unit.
#' If the input is binary, the output is the proportion of the planning unit covered.
#'
#' @param dat Dataset or filename of dataset
#' @param PlanUnits `sf` object of Planning Units
#'
#' @return `sf` object containing the Planning Units and the feature.
#' @export
#'
#' @examples
#' @importFrom rlang .data
#' @importFrom rlang :=
SpatPlan_Convert_2PUs <- function(dat, PlanUnits){

  ## First deal with whether the input is a file or a dataset

  ## LOAD FILES IF NEEDED
  if(class(dat)[1] == "character"){ # If a file, we need to load the data

    ext <- fs::path_ext(dat)
    nm <- fs::path_ext_remove(basename(dat)) # Strip out the junk and get the name
    if (ext %in% c("tif", "tiff", "grd", "gri")) {
      print("Data is in raster format")
      dat <- terra::rast(dat)
    } else if (ext %in% c("shp")) {
      print("Data is in vector format")
      dat <- sf::read_sf(dat)
    }
  }

  ## PROCESS DATA
  if (stringr::str_detect(class(dat)[1], "SpatRaster") == TRUE){ # Is it a raster or a vector dataset

    if (stringr::str_detect(names(dat), "layer")){ # Replace "layer" with better name
      names(dat) <- fs::path_ext_remove(basename(terra::sources(dat)))
    }

    nm <- names(dat)

    # Transform the data as required
    if (terra::crs(dat) != sf::st_crs(PlanUnits)){
      dat <- dat %>%
        terra::project(sf::st_crs(PlanUnits)[[1]])# Transform to correct CRS # project should default to NN if categorical, and bilinear if continuous
    }

    if(terra::is.factor(dat)){
      meth <- "mode"
    } else{
      meth <- "mean"
    }

    out <- PlanUnits %>%
      dplyr::mutate(!!nm := exactextractr::exact_extract(dat, PlanUnits, meth, progress = FALSE)) # Use mean for continuous. Use mode for categorical

    return(out)

  } else if(stringr::str_detect(class(dat)[1], "sf") == TRUE){ # Is it a raster or a vector dataset #TODO - This doesn't make it a polygon.

    nm = stringr::str_subset(colnames(dat), "geometry", negate = TRUE) # Don't return "geometry"

    # Transform the data as required
    if (sf::st_crs(dat) != sf::st_crs(PlanUnits)){
      dat <- dat %>%
        sf::st_transform(sf::st_crs(PlanUnits)) %>% # Transform to correct CRS
        sf::st_make_valid() # Make valid if needed
    }

    inter <- sf::st_intersection(PlanUnits, dat) %>%
      dplyr::mutate(!!nm := as.numeric(sf::st_area(.data$geometry))) %>% # Return area in km2
      # dplyr::mutate(!!nm := as.numeric(sf::st_area(.))) %>% # Return area in km2
      dplyr::select(tidyselect::all_of(nm), .data$cellID) %>%
      sf::st_drop_geometry() %>%
      dplyr::group_by(.data$cellID) %>%
      dplyr::summarise(!!nm := sum(!!rlang::sym(nm), na.rm = TRUE))

    out <- PlanUnits %>%
      dplyr::left_join(inter, by = "cellID") %>%
      dplyr::mutate(!!nm := tidyr::replace_na(!!rlang::sym(nm), 0),
                    !!nm := !!rlang::sym(nm) / as.numeric(sf::st_area(PlanUnits)))

    return(out)
  }

}
