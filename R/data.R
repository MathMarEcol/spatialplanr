#' Planning Units
#'
#' An sf dataframe of planning units for spatial prioritisation
#'
#' @format ## `dat_PUs`
#' A data frame with XXXX rows and XXX columns:
#' \describe{
#'   \item{cellID}{ID number for each row (or cell) of the dataset}
#'   \item{geometry}{sf geometry column}
#'   ...
#' }
#' @source "Made up data"
"dat_PUs"


# -------------------------------------------------------------------------

#' Boundary of planning units
#'
#' An sf dataframe of planning units for spatial prioritisation
#'
#' @format ## `dat_bndry`
#' A data frame with XXXX rows and XXX columns:
#' \describe{
#'   \item{cellID}{ID number for each row (or cell) of the dataset}
#'   \item{geometry}{sf geometry column}
#'   ...
#' }
#' @source "Made up data"
"dat_bndry"

# -------------------------------------------------------------------------


#' Regionalisation
#'
#' An sf dataframe of planning units for spatial prioritisation
#'
#' @format ## `dat_region`
#' A data frame with XXXX rows and XXX columns:
#' \describe{
#'   \item{cellID}{ID number for each row (or cell) of the dataset}
#'   \item{geometry}{sf geometry column}
#'   ...
#' }
#' @source "Made up data"
"dat_region"


# -------------------------------------------------------------------------

#' prioritizr Problem
#'
#' An conservation problem for spatial prioritisation
#'
#' @format ## `dat_problem`
#' A class that stores the data, mathematical formula and other information needed to run the prioritisation:
#' \describe{
#'   \item{data}{list containing information on the cost and features}
#'   \item{objective}{object specifying objective function used in conservation problem}
#'   ...
#' }
#' @source "Made up data"
"dat_problem"

# -------------------------------------------------------------------------

#' prioritizr Solution
#'
#' An sf dataframe of planning units for spatial prioritisation
#'
#' @format ## `dat_soln`
#' A data frame with XXXX rows and XXX columns:
#' \describe{
#'   \item{cellID}{ID number for each row (or cell) of the dataset}
#'   \item{geometry}{sf geometry column}
#'   ...
#' }
#' @source "Made up data"
"dat_soln"


# -------------------------------------------------------------------------

#' prioritizr Solution2
#'
#' An sf dataframe of planning units for spatial prioritisation
#'
#' @format ## `dat_soln2`
#' A data frame with XXXX rows and XXX columns:
#' \describe{
#'   \item{cellID}{ID number for each row (or cell) of the dataset}
#'   \item{geometry}{sf geometry column}
#'   ...
#' }
#' @source "Made up data"
"dat_soln2"


# -------------------------------------------------------------------------

#' Binary Species Data
#'
#' An sf dataframe of planning units for spatial prioritisation
#'
#' @format ## `dat_species_bin`
#' A data frame with XXXX rows and XXX columns:
#' \describe{
#'   \item{cellID}{ID number for each row (or cell) of the dataset}
#'   \item{geometry}{sf geometry column}
#'   ...
#' }
#' @source "Made up data"
"dat_species_bin"


# -------------------------------------------------------------------------

#' Binary Species Data 2
#'
#' An sf dataframe of planning units for spatial prioritisation
#'
#' @format ## `dat_species_bin2`
#' A data frame with XXXX rows and XXX columns:
#' \describe{
#'   \item{cellID}{ID number for each row (or cell) of the dataset}
#'   \item{geometry}{sf geometry column}
#'   ...
#' }
#' @source "Made up data"
"dat_species_bin2"


# -------------------------------------------------------------------------

#' Probability Species Data
#'
#' An sf dataframe of planning units for spatial prioritisation
#'
#' @format ## `dat_species_prob`
#' A data frame with XXXX rows and XXX columns:
#' \describe{
#'   \item{cellID}{ID number for each row (or cell) of the dataset}
#'   \item{geometry}{sf geometry column}
#'   ...
#' }
#' @source "Made up data"
"dat_species_prob"


# -------------------------------------------------------------------------

#' MPA Data
#'
#' An sf dataframe of planning units for spatial prioritisation
#'
#' @format ## `dat_mpas`
#' A data frame with XXXX rows and XXX columns:
#' \describe{
#'   \item{cellID}{ID number for each row (or cell) of the dataset}
#'   \item{geometry}{sf geometry column}
#'   \item{wdpa}{binary MPA information (1: MPA)}
#'   ...
#' }
#' @source "Made up data"
"dat_mpas"

#' Boundary of planning units
#'
#' An sf dataframe of planning units for spatial prioritisation
#'
#' @format ## `dat_bndry`
#' A data frame with XXXX rows and XXX columns:
#' \describe{
#'   \item{cellID}{ID number for each row (or cell) of the dataset}
#'   \item{geometry}{sf geometry column}
#'   ...
#' }
#' @source "Made up data"
"dat_bndry"

# -------------------------------------------------------------------------


#' Category vector
#'
#' A tibble containing specific grouping of features
#'
#' @format ## `Category_vec`
#' A data frame with XXXX rows and XXX columns:
#' \describe{
#'   \item{feature}{all features available for spatial prioritisation}
#'   \item{category}{the categories of all features}
#'   ...
#' }
#' @source "Made up data"
"Category_vec"


# -------------------------------------------------------------------------


#' Category vector
#'
#' A tibble containing specific grouping of features
#'
#' @format ## `Category_vec2`
#' A data frame with XXXX rows and XXX columns:
#' \describe{
#'   \item{feature}{all features available for spatial prioritisation}
#'   \item{category}{the categories of all features}
#'   ...
#' }
#' @source "Made up data"
"Category_vec2"


# -------------------------------------------------------------------------


#' Aquamaps species example data
#'
#' sf file containing the suitable habitat for a few marine species of the coral sea
#'
#' @format ## `spDataFiltered`
#' A data frame with 397 rows and 17 columns:
#' \describe{
#'   \item{Chelonia_mydas}{suitable habitat for Chelonia mydas}
#'   \item{cellID}{cell ID of planning units}
#'   ...
#' }
#' @source Aquamaps.org
"spDataFiltered"


# -------------------------------------------------------------------------


#' Current MPAs of the Coral Sea
#'
#' sf file containing the current marine protected areas of the coral sea
#'
#' @format ## `MPAsCoralSea`
#' A data frame with 397 rows and 17 columns:
#' \describe{
#'   \item{wdpa}{binary vector denoting presence or absence of a current marine protected area}
#'   \item{cellID}{cell ID of planning units}
#'   ...
#' }
#' @source https://www.protectedplanet.net/en/thematic-areas/wdpa?tab=WDPA
"MPAsCoralSea"


# -------------------------------------------------------------------------


#' IUCN and Inverse Area based Targets for Aquamaps example species
#'
#' tbl_df file containing IUCN and IA targets of the selected species of the coral sea
#'
#' @format ## `IUCN_IA_Targets`
#' A data frame with 15 rows and 4 columns:
#' \describe{
#'   \item{Species}{Species names as found in the IUCN database}
#'   \item{Area km2}{total suitable habitat of a species}
#'   \item{Target}{Individual species-specific target}
#'   \item{IUCN_Category}{IUCN category of the species}
#'   ...
#' }
#' @source https://www.iucnredlist.org/
"IUCN_IA_Targets"


# -------------------------------------------------------------------------
