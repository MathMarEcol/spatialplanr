## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  cache = FALSE,
  message = FALSE,
  eval = TRUE,
  fig.width = 9
)

## ----klippy, eval=TRUE, echo=FALSE, include=TRUE------------------------------
klippy::klippy(position = c("top", "right"))

## ----setup--------------------------------------------------------------------
library(spatialplanr)

## -----------------------------------------------------------------------------
Region <- "Coral Sea" # "Australia"
Type <- "Oceans" # "EEZ"

## ----eval=FALSE---------------------------------------------------------------
#  Region <- c(xmin = 150, xmax = 160, ymin = -40, ymax = -30)

## -----------------------------------------------------------------------------
Shape <- "Hexagon" # "Shape of PUs
PU_size <- 10000 # km2

## -----------------------------------------------------------------------------
cCRS <-
  "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs"

## -----------------------------------------------------------------------------
Bndry <- splnr_get_boundary(Limits = Region, Type = Type, cCRS = cCRS)

## -----------------------------------------------------------------------------
landmass <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
) %>%
  sf::st_transform(cCRS)

## -----------------------------------------------------------------------------
PUs <- splnr_get_planningUnits(Bndry = Bndry, InnerB = landmass, CellArea = PU_size, Shape = Shape)

## -----------------------------------------------------------------------------
(ggPU <- splnr_plot_PUs(PUs) +
  ggplot2::theme_bw()) # Plot Planning Units

## -----------------------------------------------------------------------------
(ggPU <- splnr_plot_PUs(PUs) +
  splnr_gg_add(
    Bndry = Bndry, overlay = landmass,
    cropOverlay = PUs, ggtheme = "Default"
  ))

## -----------------------------------------------------------------------------
splnr_theme <- list(
  ggplot2::theme_bw(),
  ggplot2::theme(
    legend.position = "right",
    legend.direction = "vertical",
    text = ggplot2::element_text(size = 9, colour = "black"),
    axis.text = ggplot2::element_text(size = 9, colour = "black"),
    plot.title = ggplot2::element_text(size = 9),
    axis.title = ggplot2::element_blank()
  )
)

(ggPU <- splnr_plot_PUs(PUs) +
  splnr_gg_add(
    Bndry = Bndry, overlay = landmass,
    cropOverlay = PUs, ggtheme = splnr_theme
  ))

## -----------------------------------------------------------------------------
Dict <- tibble::tribble(
  ~nameCommon, ~nameVariable, ~category, 
  "Green sea turtle", "Chelonia_mydas", "Reptiles", 
  "Loggerhead sea turtle", "Caretta_caretta", "Reptiles", 
  "Hawksbill sea turtle", "Eretmochelys_imbricata", "Reptiles", 
  "Olive ridley sea turtle", "Lepidochelys_olivacea", "Reptiles", 
  "Saltwater crocodile", "Crocodylus_porosus", "Reptiles", 
  "Humpback whale", "Megaptera_novaeangliae", "Mammals", 
  "Common Minke whale", "Balaenoptera_acutorostrata",
  "Mammals", 
  "Dugong", "Dugong_dugon", "Mammals", 
  "Grey nurse shark", "Carcharias_taurus", "Sharks and rays", 
  "Tiger shark", "Galeocerdo_cuvier", "Sharks and rays", 
  "Great hammerhead shark", "Sphyrna_mokarran",
  "Sharks and rays", 
  "Giant oceanic manta ray", "Mobula_birostris", "Sharks and rays", 
  "Reef manta ray", "Mobula_alfredi", "Sharks and rays", 
  "Whitetip reef shark", "Triaenodon_obesus", "Sharks and rays",
  "Red-footed booby", "Sula_sula", "Birds"
)

## -----------------------------------------------------------------------------
datEx_species_bin <- spDataFiltered %>%
  splnr_apply_cutoffs(Cutoffs = 0.5)

## -----------------------------------------------------------------------------
(ggFeature1 <- splnr_plot(
  datEx_species_bin,
  "Chelonia_mydas",
  plot_title = "Chelonia mydas",
  legend_title = "",
  legend_labels = c("Absence", "Presence")
) +
  splnr_gg_add(
    PUs = PUs, Bndry = Bndry, overlay = landmass,
    cropOverlay = PUs, ggtheme = splnr_theme
  ))

## -----------------------------------------------------------------------------
(ggFeature <- splnr_plot(
  datEx_species_bin,
  "Megaptera_novaeangliae",
  plot_title = "Megaptera novaeangliae",
  legend_title = "",
  legend_labels = c("Absence", "Presence")
) +
  splnr_gg_add(
    PUs = PUs, Bndry = Bndry, overlay = landmass,
    cropOverlay = PUs, ggtheme = splnr_theme
  ))

## -----------------------------------------------------------------------------
(ggFeatNo <- splnr_plot(datEx_species_bin, showFeatureSum = TRUE, plot_title = "", legend_title = "Number of features") +
  splnr_gg_add(
    PUs = PUs, Bndry = Bndry, overlay = landmass,
    cropOverlay = PUs, ggtheme = splnr_theme
  ))

## -----------------------------------------------------------------------------
feature_names <- splnr_featureNames(datEx_species_bin)

## -----------------------------------------------------------------------------
out_sf <- datEx_species_bin

## -----------------------------------------------------------------------------
out_sf$Cost_None <- 1
(ggCost <- splnr_plot(out_sf, col_names = "Cost_None", legend_title = "Cost", legend_labels = "1") +
  splnr_gg_add(
    PUs = PUs, Bndry = Bndry, overlay = landmass,
    cropOverlay = PUs, ggtheme = splnr_theme
  ))

## ----eval=FALSE---------------------------------------------------------------
#  gfw_data <- splnr_get_gfw('Australia', "2022-01-01", "2022-12-31", "yearly", cCRS = cCRS, compress = TRUE) %>%
#    sf::st_interpolate_aw(PUs, extensive = TRUE)
#  
#  out_sf$Apparent.Fishing.Hours <- 0 # Add column to PUs
#  out_sf$Apparent.Fishing.Hours[as.numeric(rownames(PUs))] <- gfw_data$Apparent.Fishing.Hours # Put corresponding data in PUs
#  
#  
#  (ggCost <- splnr_plot(out_sf, col_names = "Apparent.Fishing.Hours") +
#    splnr_gg_add(
#      PUs = PUs, Bndry = Bndry, overlay = landmass,
#      cropOverlay = PUs, ggtheme = splnr_theme
#      ))

## ----eval=FALSE, echo=FALSE---------------------------------------------------
#  bathymetry <- oceandatr::get_bathymetry(planning_grid = PUs, keep = FALSE, classify_bathymetry = FALSE)
#  geomorphology <- oceandatr::get_geomorphology(planning_grid = PUs)
#  knolls <- oceandatr::get_knolls(planning_grid = PUs)
#  seamounts <- oceandatr::get_seamounts_buffered(planning_grid = PUs, buffer = 30000)
#  coral_habitat <- oceandatr::get_coral_habitat(planning_grid = PUs)
#  enviro_regions <- oceandatr::get_enviro_regions(planning_grid = PUs, max_num_clusters = 5)

## ----eval=FALSE, echo=FALSE---------------------------------------------------
#  splnr_plot(df = bathymetry, col_names = "bathymetry", plot_title = "") +
#    splnr_gg_add(
#      PUs = PUs, Bndry = Bndry, overlay = landmass,
#      cropOverlay = PUs, ggtheme = splnr_theme
#    )

## -----------------------------------------------------------------------------
catTarg <- c("Reptiles" = 0.3, "Mammals" = 0.1, "Sharks and rays" = 0.1, "Birds" = 0.1)

target <- Dict %>%
  splnr_targets_byCategory(catTarg, catName = "category")

## -----------------------------------------------------------------------------
datEx_problem <- prioritizr::problem(out_sf, feature_names, "Cost_None") %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_relative_targets(target$target) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(verbose = FALSE)

## -----------------------------------------------------------------------------
datEx_soln <- datEx_problem %>%
  prioritizr::solve.ConservationProblem()

## -----------------------------------------------------------------------------
(ggSoln <- splnr_plot(datEx_soln, col_names = "solution_1", legend_title = "Solution", legend_labels = c("0","1")) +
  splnr_gg_add(
    PUs = PUs, Bndry = Bndry, overlay = landmass,
    cropOverlay = PUs, ggtheme = splnr_theme
  ))

## -----------------------------------------------------------------------------
(ggCostOverlay <- splnr_plot_costOverlay(
  soln = datEx_soln,
  Cost = NA,
  Cost_name = "Cost_None"
) +
  splnr_gg_add(
    PUs = PUs, Bndry = Bndry, overlay = landmass,
    cropOverlay = PUs, ggtheme = splnr_theme
  ))

## ----fig.height=7-------------------------------------------------------------
dfTarget <- splnr_get_featureRep(datEx_soln, datEx_problem,
  climsmart = FALSE, solnCol = "solution_1"
)


(ggTarget <- splnr_plot_featureRep(dfTarget, category = Dict, renameFeatures = TRUE,
                                   namesToReplace = Dict, categoryFeatureCol = "nameCommon",
  nr = 1, showTarget = TRUE,
))

## ----eval=FALSE---------------------------------------------------------------
#  # dfTargetCirc <- dfTarget %>%
#  # dplyr::select("feature", "value", "group") %>%
#  # na.omit()
#  
#  # colors <- c(
#  #   "important" = "darkgreen",
#  #   "representative" = "darkred"
#  # )
#  # legends <- c("Important", "Representative")
#  
#  # (ggCircTarg <- splnr_plot_circBplot(df = dfTargetCirc, legend_list = legends, legend_color = colors, impTarget = 30, repTarget = 10))

## -----------------------------------------------------------------------------
ggFerrier <- splnr_plot_importanceScore(
  soln = datEx_soln,
  pDat = datEx_problem,
  method = "Ferrier",
  decimals = 4,
  legendTitle = "Importance Score \n(Ferrier Score)"
) +
  splnr_gg_add(
    PUs = PUs, Bndry = Bndry, overlay = landmass,
    cropOverlay = PUs, ggtheme = splnr_theme
  )

ggRWR <- splnr_plot_importanceScore(
  soln = datEx_soln,
  pDat = datEx_problem,
  method = "RWR",
  decimals = 2,
  legendTitle = "Importance Score \n(Rarity Weighted Richness)"
) +
  splnr_gg_add(
    PUs = PUs, Bndry = Bndry, overlay = landmass,
    cropOverlay = PUs, ggtheme = splnr_theme
  )

(ggScores <- patchwork::wrap_plots(ggFerrier + ggRWR))

## ----eval=FALSE---------------------------------------------------------------
#  # The user can download their own MPAs using this code,
#  # but below we use data already within the package.
#  LockedIn <- splnr_get_MPAs(PUs, "Australia") %>%
#    splnr_apply_cutoffs(0.5) %>%
#    dplyr::mutate(wdpa = as.logical(wdpa))

## -----------------------------------------------------------------------------
(ggMPA <- splnr_plot(MPAsCoralSea, "wdpa") +
  splnr_gg_add(
    PUs = PUs, Bndry = Bndry, overlay = landmass,
    cropOverlay = PUs, ggtheme = splnr_theme
  ))

## -----------------------------------------------------------------------------

minTarget <- 0.1
maxTarget <- 0.2

# Create inverse area target
IA_Targets <- splnr_targets_byInverseArea(
  datEx_species_bin,
  minTarget,
  maxTarget
)

## ----eval = FALSE-------------------------------------------------------------
#  
#  IUCN_IA_Targets <- IA_Targets %>%
#    splnr_get_IUCNRedList(species_col = "Species") %>% # Add RL data to the df
#    splnr_targets_byIUCN(IUCN_target = 0.3)

## -----------------------------------------------------------------------------
datEx_problem2 <- prioritizr::problem(out_sf, feature_names, "Cost_None") %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_relative_targets(IA_Targets$target) %>%
  prioritizr::add_locked_in_constraints(as.logical(MPAsCoralSea$wdpa)) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(verbose = FALSE)

# Add a solution object
datEx_soln2 <- datEx_problem2 %>%
  prioritizr::solve.ConservationProblem()

(ggSoln2 <- splnr_plot_solution(datEx_soln2) +
  splnr_gg_add(
    PUs = PUs, Bndry = Bndry, overlay = landmass,
    cropOverlay = PUs, ggtheme = splnr_theme
  ))

## -----------------------------------------------------------------------------
(splnr_plot_solution(datEx_soln2) +
  splnr_gg_add(
    PUs = PUs, Bndry = Bndry, lockedInAreas = MPAsCoralSea,
    colInterest = MPAsCoralSea$wdpa, Type = "Full",
    colorLI = "lightgrey", alphaLI = 0.2, overlay = landmass,
    cropOverlay = PUs, ggtheme = splnr_theme
  ))

## -----------------------------------------------------------------------------
(splnr_plot_solution(datEx_soln2) +
  splnr_gg_add(
    lockedInAreas = MPAsCoralSea, colInterest = MPAsCoralSea$wdpa,
    Type = "Contours", overlay = landmass,
    cropOverlay = PUs, ggtheme = splnr_theme
  ))

## -----------------------------------------------------------------------------
(ggComp <- splnr_plot_comparison(datEx_soln, datEx_soln2) +
  splnr_gg_add(
    PUs = PUs, Bndry = Bndry, overlay = landmass,
    cropOverlay = PUs, ggtheme = splnr_theme
  ))

## -----------------------------------------------------------------------------
CorrMat <- splnr_get_kappaCorrData(list(datEx_soln, datEx_soln2),
  name_sol = c("soln1", "soln2")
)

(ggCorr <- splnr_plot_corrMat(CorrMat,
  AxisLabels = c("Solution 1", "Solution 2")
))

## -----------------------------------------------------------------------------
datEx_soln_portfolio <- datEx_problem %>%
  prioritizr::add_cuts_portfolio(5) %>% # create a portfolio of solutions
  prioritizr::solve.ConservationProblem()

selFreq <- datEx_soln_portfolio %>% # calculate selection frequency
  sf::st_drop_geometry() %>%
  dplyr::mutate(selFreq = as.factor(rowSums(
    dplyr::select(., dplyr::starts_with("solution_"))
  ))) %>%
  sf::st_as_sf(geometry = datEx_soln_portfolio$geometry) %>%
  dplyr::select(selFreq)

(ggselFreq <- splnr_plot_selectionFreq(selFreq) +
  splnr_gg_add(
    PUs = PUs, Bndry = Bndry, overlay = landmass,
    cropOverlay = PUs, ggtheme = splnr_theme
  ))

