#' Assign Longhurst Provinces to PUs
#'
#' A function that assigns the Longhurst province and description to each planning unit.
#'
#' @param PlanUnits Planning Units as an `sf` object
#' @param Direc The directory where the MME data is being stored. If not specified, the default location is assumed.
#'
#' @return An `sf` dataframe with additional longhurst columns (`ProvCode`, `ProvDescr`)
#' @export
#'
#' @examples
SpatPlan_Match_Longhurst <- function(PlanUnits,
                                     Direc = file.path("~", "SpatPlan_Data")){


  if (!file.exists(Direc)) {
    stop(paste("The Data folder does not exist at ",Direc,". Please download from the RDM and then try again. See https://github.com/MathMarEcol/spatialplanr for details."))
  }

  longh <- sf::st_read(file.path(Direc,"LonghurstProvinces","Longhurst_world_v4_2010.shp")) %>%
    sf::st_transform(sf::st_crs(PlanUnits))

  nr <- sf::st_nearest_feature(PlanUnits, longh)

  LPs <- PlanUnits %>%
    dplyr::mutate(ProvCode = longh$ProvCode[nr],
                  ProvDescr = longh$ProvDescr[nr])

  return(LPs)
}
