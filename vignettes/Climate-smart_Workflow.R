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
print("klippy")
klippy::klippy(position = c("top", "right"))

## ----setup--------------------------------------------------------------------
print("library")
library(spatialplanr)

## -----------------------------------------------------------------------------
print("parameters")
Region <- "Coral Sea" # "Australia"
Type <- "Oceans" # "EEZ"
Shape <- "Hexagon" # "Shape of PUs
PU_size <- 10000 # km2

cCRS <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs" # Mollweide

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

## -----------------------------------------------------------------------------
Bndry <- splnr_get_boundary(Limits = Region, Type = Type, cCRS = cCRS)

landmass <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  sf::st_transform(cCRS)

## -----------------------------------------------------------------------------
PUs <- splnr_get_planningUnits(Bndry, landmass, PU_size, Shape)

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
  dplyr::as_tibble() %>%
  dplyr::mutate(dplyr::across(
    -dplyr::any_of(c("cellID", "geometry")), # Don't apply to geometry/cellID
    ~ dplyr::case_when(
      . >= 0.5 ~ 1,
      . < 0.5 ~ 0,
      is.na(.data) ~ 0
    )
  )) %>%
  sf::st_as_sf()

col_name <- spDataFiltered %>%
  sf::st_drop_geometry() %>%
  dplyr::select(-"cellID") %>%
  colnames()

## -----------------------------------------------------------------------------
metric_df <- CoralSeaVelocity %>%
  dplyr::rename(metric = voccMag_transformed)

## ----fig.width = 9------------------------------------------------------------
(ggclim <- splnr_plot_climData(metric_df, "metric") +
  splnr_gg_add(
    Bndry = Bndry, overlay = landmass,
    cropOverlay = PUs, ggtheme = splnr_theme
  ))

## ----fig.width = 9------------------------------------------------------------
set.seed(5)

metric_df <- CoralSeaVelocity %>%
  dplyr::rename(metric = voccMag_transformed) %>%
  dplyr::mutate(
    metricOG = metric,
    metric = ifelse(metric > 0.99, runif(., 0.85, 1.0), metric)
  )

(ggclim <- splnr_plot_climData(metric_df, "metric") +
  splnr_gg_add(
    Bndry = Bndry, overlay = landmass,
    cropOverlay = PUs, ggtheme = splnr_theme
  ))

## -----------------------------------------------------------------------------
target <- datEx_species_bin %>%
  sf::st_drop_geometry() %>%
  dplyr::select(-"cellID") %>%
  colnames() %>%
  data.frame() %>%
  setNames(c("feature")) %>%
  dplyr::mutate(target = 0.3)


CPA_Approach <- splnr_climate_priorityAreaApproach(
  featuresDF = datEx_species_bin,
  metricDF = metric_df, targetsDF = target, direction = -1, refugiaTarget = 1
)

out_sf <- CPA_Approach$Features %>%
  dplyr::left_join(
    datEx_species_bin %>%
      sf::st_drop_geometry() %>%
      dplyr::select(
        "cellID",
        tidyselect::starts_with("Cost_")
      ),
    by = "cellID"
  ) %>%
  dplyr::left_join(metric_df %>%
    sf::st_drop_geometry(), by = "cellID")

targets <- CPA_Approach$Targets

## -----------------------------------------------------------------------------
out_sf$Cost_None <- rep(1, 397)

usedFeatures <- out_sf %>%
  sf::st_drop_geometry() %>%
  dplyr::select(
    -tidyselect::starts_with("Cost_"),
    -"cellID",
    -tidyselect::starts_with("metric")
  ) %>%
  names()

## -----------------------------------------------------------------------------
p1 <- prioritizr::problem(out_sf, usedFeatures, "Cost_None") %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_relative_targets(targets$target) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(verbose = FALSE)

dat_solnClim <- prioritizr::solve.ConservationProblem(p1)

## ----fig.width = 9------------------------------------------------------------
(ggSoln <- splnr_plot_solution(dat_solnClim) +
  splnr_gg_add(
    Bndry = Bndry, overlay = landmass,
    cropOverlay = PUs, ggtheme = splnr_theme
  ))

## ----fig.width = 9------------------------------------------------------------
(ggClimDens <- splnr_plot_climKernelDensity(
  soln = list(dat_solnClim),
  names = c("Input 1"), type = "Normal",
  legendTitle = "Climate velocity (add unit)",
  xAxisLab = "Climate velocity"
))

## -----------------------------------------------------------------------------
target <- datEx_species_bin %>%
  sf::st_drop_geometry() %>%
  dplyr::select(-"cellID") %>%
  colnames() %>%
  data.frame() %>%
  setNames(c("feature")) %>%
  dplyr::mutate(target = 30)

Percentile_Approach <- splnr_climate_percentileApproach(
  featuresDF = datEx_species_bin,
  metricDF = metric_df, targetsDF = target, direction = -1, percentile = 35
)

out_sf <- Percentile_Approach$Features %>%
  dplyr::left_join(
    datEx_species_bin %>%
      sf::st_drop_geometry() %>%
      dplyr::select(
        "cellID",
        tidyselect::starts_with("Cost_")
      ),
    by = "cellID"
  ) %>%
  dplyr::left_join(metric_df %>%
    sf::st_drop_geometry(), by = "cellID")

targets <- Percentile_Approach$Targets

## ----warning=FALSE------------------------------------------------------------
out_sf$Cost_None <- rep(1, 397)

usedFeatures <- out_sf %>%
  sf::st_drop_geometry() %>%
  dplyr::select(
    -tidyselect::starts_with("Cost_"),
    -"cellID",
    -tidyselect::starts_with("metric")
  ) %>%
  names()

p2 <- prioritizr::problem(out_sf, usedFeatures, "Cost_None") %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_relative_targets(targets$target) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(verbose = FALSE)

dat_solnClimPercentile <- prioritizr::solve.ConservationProblem(p2,
  force = TRUE
)

## ----fig.width = 9------------------------------------------------------------
(ggSoln <- splnr_plot_solution(dat_solnClimPercentile) +
  splnr_gg_add(
    Bndry = Bndry, overlay = landmass,
    cropOverlay = PUs, ggtheme = splnr_theme
  ))

## ----fig.width = 9------------------------------------------------------------
(ggClimDens <- splnr_plot_climKernelDensity(
  soln = list(dat_solnClimPercentile),
  names = c("Input 1"), type = "Normal",
  legendTitle = "Climate velocity (add unit)",
  xAxisLab = "Climate velocity"
))

## -----------------------------------------------------------------------------
target <- datEx_species_bin %>%
  sf::st_drop_geometry() %>%
  dplyr::select(-"cellID") %>%
  colnames() %>%
  data.frame() %>%
  setNames(c("feature")) %>%
  dplyr::mutate(target = 0.3)

Feature_Approach <- splnr_climate_featureApproach(
  featuresDF = datEx_species_bin,
  metricDF = metric_df, targetsDF = target, direction = 1
)

out_sf <- Feature_Approach$Features %>%
  dplyr::left_join(
    datEx_species_bin %>%
      sf::st_drop_geometry() %>%
      dplyr::select(
        "cellID",
        tidyselect::starts_with("Cost_")
      ),
    by = "cellID"
  ) %>%
  dplyr::left_join(metric_df %>%
    sf::st_drop_geometry(), by = "cellID")

targets <- Feature_Approach$Targets

## -----------------------------------------------------------------------------
out_sf$Cost_None <- rep(1, 397)

usedFeatures <- out_sf %>%
  sf::st_drop_geometry() %>%
  dplyr::select(
    -tidyselect::starts_with("Cost_"),
    -"cellID",
    -tidyselect::starts_with("metric")
  ) %>%
  names()

p3 <- prioritizr::problem(out_sf, usedFeatures, "Cost_None") %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_relative_targets(targets$target) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(verbose = FALSE)

dat_solnClimFeature <- prioritizr::solve.ConservationProblem(p3)

## ----fig.width = 9------------------------------------------------------------
(ggSoln <- splnr_plot_solution(dat_solnClimFeature) +
  splnr_gg_add(
    Bndry = Bndry, overlay = landmass,
    cropOverlay = PUs, ggtheme = splnr_theme
  ))

## ----fig.width = 9------------------------------------------------------------
(ggClimDens <- splnr_plot_climKernelDensity(
  soln = list(dat_solnClimFeature),
  names = c("Input 1"), type = "Normal",
  legendTitle = "Climate velocity (add unit)",
  xAxisLab = "Climate velocity"
))

