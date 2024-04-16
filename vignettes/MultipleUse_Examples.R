## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  cache = FALSE,
  message = FALSE,
  eval = TRUE
)

## ----klippy, eval=TRUE, echo=FALSE, include=TRUE------------------------------
klippy::klippy(position = c("top", "right"))

## ----setup--------------------------------------------------------------------
library(spatialplanr)
set.seed(100)

## -----------------------------------------------------------------------------
Region <- "Coral Sea" # "Australia"
Type <- "Oceans" # "EEZ"

## -----------------------------------------------------------------------------
Shape <- "Hexagon" # "Shape of PUs
PU_size <- 10000 # km2

## -----------------------------------------------------------------------------
cCRS <-
  "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs"

## -----------------------------------------------------------------------------
Bndry <- splnr_get_boundary(Limits = Region, Type = Type, cCRS = cCRS)

landmass <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
) %>%
  sf::st_transform(cCRS)

## -----------------------------------------------------------------------------
PUs <- splnr_get_planningUnits(Bndry, landmass, PU_size, Shape)

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

col_name <- spDataFiltered %>%
  sf::st_drop_geometry() %>%
  dplyr::select(-"cellID") %>%
  colnames()

## -----------------------------------------------------------------------------
target <- rep(0.3, nrow(Dict))

p1 <- prioritizr::problem(
  datEx_species_bin %>% dplyr::mutate(Cost1 = rep(1, 397)),
  col_name,
  "Cost1"
) %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_relative_targets(target) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(verbose = FALSE)

## ----fig.width=9--------------------------------------------------------------
s1 <- p1 %>%
  prioritizr::solve.ConservationProblem()

(ggSoln <- splnr_plot_solution(s1) +
  splnr_gg_add(PUs = PUs, Bndry = Bndry, overlay = landmass, cropOverlay = PUs, ggtheme = splnr_theme))

## -----------------------------------------------------------------------------
s1T <- s1 %>%
  dplyr::select(tidyselect::starts_with(c("solution"))) %>%
  sf::st_drop_geometry() %>%
  tibble::as_tibble()

r1 <- prioritizr::eval_feature_representation_summary(p1, s1T)
print(r1)

## -----------------------------------------------------------------------------
target2 <- matrix(NA, ncol = 2, nrow = nrow(Dict))
target2[, 1] <- 0.2
target2[, 2] <- 0.05

## -----------------------------------------------------------------------------
z2 <- prioritizr::zones("zone 1" = col_name, "zone 2" = col_name)

## -----------------------------------------------------------------------------
p2 <- prioritizr::problem(
  datEx_species_bin %>% dplyr::mutate(
    Cost1 = rep(1, 397), # when giving sf input, we need as many cost columns as we have zones
    Cost2 = runif(n = dim(.)[[1]])
  ),
  z2,
  cost_column = c("Cost1", "Cost2")
) %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_relative_targets(target2) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(verbose = FALSE)

s2 <- p2 %>%
  prioritizr::solve.ConservationProblem()

## ----fig.width = 9------------------------------------------------------------
(gg_s2 <- splnr_plot_solution(
  s2,
  zones = TRUE,
  colorVals = c("#c6dbef", "#3182bd", "black"),
  legendLabels = c("Not selected", "Zone 1", "Zone 2")
) +
  splnr_gg_add(
    PUs = PUs, Bndry = Bndry, overlay = landmass,
    cropOverlay = PUs, ggtheme = splnr_theme
  ))

## -----------------------------------------------------------------------------
targets2b <- Dict %>%
  dplyr::mutate(
    targetZ1 = dplyr::if_else(category == "Reptiles", 30 / 100, 0),
    targetZ2 = dplyr::if_else(category != "Reptiles", 10 / 100, 0)
  ) %>%
  dplyr::select("targetZ1", "targetZ2") %>%
  as.matrix()

## -----------------------------------------------------------------------------
# NOTE: when using sf input, we need as many cost columns as we have zones
p2b <- prioritizr::problem(
  datEx_species_bin %>% dplyr::mutate(
    Cost1 = rep(1, 397),
    Cost2 = runif(n = dim(.)[[1]])
  ),
  z2,
  cost_column = c("Cost1", "Cost2")
) %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_relative_targets(targets2b) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(verbose = FALSE)

s2b <- p2b %>%
  prioritizr::solve.ConservationProblem()

## -----------------------------------------------------------------------------
r2b <- s2b %>%
  dplyr::select(tidyselect::starts_with(c("solution"))) %>%
  sf::st_drop_geometry() %>%
  tibble::as_tibble() %>%
  prioritizr::eval_feature_representation_summary(p2b, .)
print(r2b, n = 45)

## -----------------------------------------------------------------------------
Dict[[1]][6]
Dict[[1]][7]

## ----fig.width = 9------------------------------------------------------------
(gg_s2b <- splnr_plot_solution(
  s2b,
  zones = TRUE,
  colorVals = c("#c6dbef", "#3182bd", "black"),
  legendLabels = c("Not selected", "Zone 1", "Zone 2")
) +
  splnr_gg_add(
    PUs = PUs, Bndry = Bndry, overlay = landmass,
    cropOverlay = PUs, ggtheme = splnr_theme
  ))

## -----------------------------------------------------------------------------
zm1 <- diag(2)
print(zm1)

## -----------------------------------------------------------------------------
p3 <- prioritizr::problem(
  datEx_species_bin %>% dplyr::mutate(
    Cost1 = rep(1, 397), # when giving sf input, we need as many cost columns as we have zones
    Cost2 = runif(n = dim(.)[[1]])
  ),
  z2,
  cost_column = c("Cost1", "Cost2")
) %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_boundary_penalties(0.5, zone = zm1) %>%
  prioritizr::add_relative_targets(target2) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(time_limit = 10, verbose = FALSE)

s3 <- p3 %>%
  prioritizr::solve.ConservationProblem()

## ----fig.width=9--------------------------------------------------------------
(gg_s3 <- splnr_plot_solution(
  s3,
  zones = TRUE,
  colorVals = c("#c6dbef", "#3182bd", "black"),
  legendLabels = c("Not selected", "Zone 1", "Zone 2")
) +
  splnr_gg_add(
    PUs = PUs, Bndry = Bndry, overlay = landmass,
    cropOverlay = PUs, ggtheme = splnr_theme
  ))

## ----fig.width=9--------------------------------------------------------------
zm2 <- zm1
zm2[2, 2] <- 0

# NOTE: When using sf input, we need as many cost columns as we have zones
p4 <- prioritizr::problem(
  datEx_species_bin %>% dplyr::mutate(
    Cost1 = rep(1, 397),
    Cost2 = runif(n = dim(.)[[1]])
  ),
  z2,
  cost_column = c("Cost1", "Cost2")
) %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_boundary_penalties(0.5, zone = zm2) %>%
  prioritizr::add_relative_targets(target2) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(time_limit = 10, verbose = FALSE)

s4 <- p4 %>%
  prioritizr::solve.ConservationProblem()

(gg_s4 <- splnr_plot_solution(
  s4,
  zones = TRUE,
  colorVals = c("#c6dbef", "#3182bd", "black"),
  legendLabels = c("Not selected", "Zone 1", "Zone 2")
) +
  splnr_gg_add(
    PUs = PUs, Bndry = Bndry, overlay = landmass,
    cropOverlay = PUs, ggtheme = splnr_theme
  ))

## -----------------------------------------------------------------------------
zm3 <- matrix(1, ncol = 2, nrow = 2)
print(zm3)

## ----fig.width=9--------------------------------------------------------------
p5 <- prioritizr::problem(
  datEx_species_bin %>% dplyr::mutate(
    Cost1 = rep(1, 397), # when giving sf input, we need as many cost columns as we have zones
    Cost2 = runif(n = dim(.)[[1]])
  ),
  z2,
  cost_column = c("Cost1", "Cost2")
) %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_boundary_penalties(0.5, zone = zm3) %>%
  prioritizr::add_relative_targets(target2) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(time_limit = 10, verbose = FALSE)

s5 <- p5 %>%
  prioritizr::solve.ConservationProblem()

(gg_s5 <- splnr_plot_solution(
  s5,
  zones = TRUE,
  colorVals = c("#c6dbef", "#3182bd", "black"),
  legendLabels = c("Not selected", "Zone 1", "Zone 2")
) +
  splnr_gg_add(
    PUs = PUs, Bndry = Bndry, overlay = landmass,
    cropOverlay = PUs, ggtheme = splnr_theme
  ))

## -----------------------------------------------------------------------------
mpas <- MPAsCoralSea %>%
  dplyr::mutate(zone = "zone 1") %>%
  dplyr::rename(
    pu = cellID,
    status = wdpa
  ) %>%
  sf::st_drop_geometry() %>%
  tibble::tibble() %>%
  dplyr::filter(status == 1)

## -----------------------------------------------------------------------------
# NOTE: When using sf input, we need as many cost columns as we have zones
p6 <- prioritizr::problem(
  datEx_species_bin %>% dplyr::mutate(
    Cost1 = rep(1, 397),
    Cost2 = runif(n = dim(.)[[1]])
  ),
  z2,
  cost_column = c("Cost1", "Cost2")
) %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_manual_locked_constraints(mpas) %>%
  prioritizr::add_relative_targets(target2) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(time_limit = 10, verbose = FALSE)

s6 <- p6 %>%
  prioritizr::solve.ConservationProblem()

## ----fig.width = 9------------------------------------------------------------
(gg_s6 <- splnr_plot_solution(
  s6,
  zones = TRUE,
  colorVals = c("#c6dbef", "#3182bd", "black"),
  legendLabels = c("Not selected", "Zone 1", "Zone 2")
) +
  splnr_gg_add(
    PUs = PUs, Bndry = Bndry,
    lockedInAreas = MPAsCoralSea, colInterest = MPAsCoralSea$wdpa,
    Type = "Full", colorLI = "red", alphaLI = 0.2, overlay = landmass,
    cropOverlay = PUs, ggtheme = splnr_theme, labelL = "Current MPAs"
  ))

## -----------------------------------------------------------------------------
# NOTE: When using sf input, we need as many cost columns as we have zones
p7 <- prioritizr::problem(
  datEx_species_bin %>% dplyr::mutate(
    Cost1 = rep(1, 397),
    Cost2 = runif(n = dim(.)[[1]])
  ),
  z2,
  cost_column = c("Cost1", "Cost2")
) %>%
  prioritizr::add_min_shortfall_objective(c(8, 0.3)) %>%
  prioritizr::add_relative_targets(target2) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(time_limit = 10, verbose = FALSE)

## ----fig.width = 9------------------------------------------------------------
s7 <- p7 %>%
  prioritizr::solve.ConservationProblem()

(gg_s7 <- splnr_plot_solution(
  s7,
  zones = TRUE,
  colorVals = c("#c6dbef", "#3182bd", "black"),
  legendLabels = c("Not selected", "Zone 1", "Zone 2")
) +
  splnr_gg_add(
    PUs = PUs, Bndry = Bndry, overlay = landmass,
    cropOverlay = PUs, ggtheme = splnr_theme
  ))

## -----------------------------------------------------------------------------
target8 <- rep(0.3, nrow(Dict))

set.seed <- 10 # Add seed so the problem below solves each time

costRandom <- datEx_species_bin %>%
  dplyr::mutate(CostR = runif(n = dim(.)[[1]])) %>%
  dplyr::select("CostR")

## -----------------------------------------------------------------------------
p8 <- prioritizr::problem(
  datEx_species_bin %>% dplyr::mutate(Cost1 = rep(1, 397)),
  col_name,
  "Cost1"
) %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_relative_targets(target8) %>%
  prioritizr::add_linear_constraints(sum(costRandom$CostR) * 0.1,
    sense = "<=", costRandom$CostR
  ) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(verbose = FALSE)

s8 <- p8 %>%
  prioritizr::solve.ConservationProblem()

## ----fig.width = 9------------------------------------------------------------
(gg_s8 <- splnr_plot_solution(s8) +
  splnr_gg_add(PUs = PUs, Bndry = Bndry, overlay = landmass, cropOverlay = PUs, ggtheme = splnr_theme))

## -----------------------------------------------------------------------------
targetAcross <- dplyr::tibble(
  feature = col_name,
  zone = list(c("zone1", "zone2"))[rep(1, length(col_name))],
  target = rep(0.3, length(col_name)),
  type = rep("relative", length(col_name))
)

## -----------------------------------------------------------------------------
datSpecZone1 <- datEx_species_bin %>%
  sf::st_drop_geometry() %>%
  dplyr::rename_at(dplyr::vars(-cellID), ~ paste0(.x, "_zone1"))

featuresZone1 <- datSpecZone1 %>%
  dplyr::select(-"cellID") %>%
  colnames()

datSpecZone2 <- datEx_species_bin %>%
  sf::st_drop_geometry() %>%
  dplyr::rename_at(dplyr::vars(-cellID), ~ paste0(.x, "_zone2"))

featuresZone2 <- datSpecZone2 %>%
  dplyr::select(-"cellID") %>%
  colnames()

## -----------------------------------------------------------------------------
z10 <- prioritizr::zones(
  featuresZone1,
  featuresZone2,
  zone_names = c("zone1", "zone2"),
  feature_names = col_name
)

out_sf <- datSpecZone1 %>%
  dplyr::left_join(datSpecZone2, by = "cellID") %>%
  dplyr::mutate(geometry = datEx_species_bin$geometry) %>%
  sf::st_as_sf() %>%
  dplyr::mutate(
    Cost1 = runif(n = dim(.)[[1]]) * 100,
    Cost2 = runif(n = dim(.)[[1]]) * 300
  )

## -----------------------------------------------------------------------------
p10 <- prioritizr::problem(out_sf,
  z10,
  cost_column = c("Cost1", "Cost2")
) %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_manual_targets(targetAcross) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(verbose = FALSE)

s10 <- p10 %>%
  prioritizr::solve.ConservationProblem()

## -----------------------------------------------------------------------------
s10F <- s10 %>%
  dplyr::select(tidyselect::starts_with(c("solution"))) %>%
  sf::st_drop_geometry() %>%
  tibble::as_tibble()

r10 <- prioritizr::eval_feature_representation_summary(p10, s10F)
print(r10)

## ----fig.width=9--------------------------------------------------------------
(gg_s10 <- splnr_plot_solution(s10,
  zones = TRUE,
  colorVals = c("#c6dbef", "#3182bd", "black"),
  legendLabels = c("Not selected", "Zone 1", "Zone 2")
) +
  splnr_gg_add(
    PUs = PUs, Bndry = Bndry, overlay = landmass,
    cropOverlay = PUs, ggtheme = splnr_theme
  ))

## -----------------------------------------------------------------------------
CostArea <- out_sf %>%
  dplyr::mutate(
    areaCostZone1 = rep(1, 397),
    areaCostZone2 = rep(0, 397)
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::select("areaCostZone1", "areaCostZone2") %>%
  as.matrix()

## -----------------------------------------------------------------------------
p11 <- prioritizr::problem(out_sf,
  z10,
  cost_column = c("Cost1", "Cost2")
) %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_linear_constraints(sum(CostArea[, 1]) * 0.3,
    sense = "<=", CostArea
  ) %>%
  prioritizr::add_manual_targets(targetAcross) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(verbose = FALSE)

s11 <- p11 %>%
  prioritizr::solve.ConservationProblem()

## -----------------------------------------------------------------------------
s11F <- s11 %>%
  dplyr::select(tidyselect::starts_with(c("solution"))) %>%
  sf::st_drop_geometry() %>%
  tibble::as_tibble()

r11 <- prioritizr::eval_feature_representation_summary(p11, s11F)
print(r11)

## ----fig.width=9--------------------------------------------------------------
(gg_s11 <- splnr_plot_solution(
  s11,
  zones = TRUE,
  colorVals = c("#c6dbef", "#3182bd", "black"),
  legendLabels = c("Not selected", "Zone 1", "Zone 2")
) +
  splnr_gg_add(
    PUs = PUs, Bndry = Bndry, overlay = landmass,
    cropOverlay = PUs, ggtheme = splnr_theme
  ))

## -----------------------------------------------------------------------------
targetAcrossSome <- Dict %>%
  dplyr::mutate(
    target = dplyr::case_when(
      category == "Reptiles" ~ 30 / 100,
      category == "Mammals" ~ 10 / 100,
      category == "Sharks and rays" ~ 5 / 100,
      TRUE ~ 0
    ),
    zone = dplyr::case_when(
      category == "Reptiles" ~ list(c("zone1", "zone2")),
      category == "Mammals" ~ list("zone2"),
      category == "Sharks and rays" ~ list("zone3"),
      TRUE ~ list(c("zone1", "zone2", "zone3"))
    ),
    type = rep("relative", length(col_name))
  ) %>%
  dplyr::rename(feature = "nameVariable") %>%
  dplyr::select(-"category", -"nameCommon")

## -----------------------------------------------------------------------------
datSpecZone1 <- datEx_species_bin %>%
  sf::st_drop_geometry() %>%
  dplyr::rename_at(dplyr::vars(-cellID), ~ paste0(.x, "_zone1"))

featuresZone1 <- datSpecZone1 %>%
  dplyr::select(-"cellID") %>%
  colnames()

datSpecZone2 <- datEx_species_bin %>%
  sf::st_drop_geometry() %>%
  dplyr::rename_at(dplyr::vars(-cellID), ~ paste0(.x, "_zone2"))

featuresZone2 <- datSpecZone2 %>%
  dplyr::select(-"cellID") %>%
  colnames()

datSpecZone3 <- datEx_species_bin %>%
  sf::st_drop_geometry() %>%
  dplyr::rename_at(dplyr::vars(-cellID), ~ paste0(.x, "_zone3"))

featuresZone3 <- datSpecZone3 %>%
  dplyr::select(-"cellID") %>%
  colnames()

z12 <- prioritizr::zones(
  featuresZone1,
  featuresZone2,
  featuresZone3,
  zone_names = c("zone1", "zone2", "zone3"),
  feature_names = col_name
)

## -----------------------------------------------------------------------------
# NOTE: When using sf input, we need as many cost columns as we have zones
out_sf <- datSpecZone1 %>%
  dplyr::left_join(datSpecZone2, by = "cellID") %>%
  dplyr::left_join(datSpecZone3, by = "cellID") %>%
  dplyr::mutate(geometry = datEx_species_bin$geometry) %>%
  sf::st_as_sf() %>%
  dplyr::mutate(
    Cost1 = runif(n = dim(.)[[1]]) * 100,
    Cost2 = runif(n = dim(.)[[1]]) * 300,
    Cost3 = runif(n = dim(.)[[1]]) * 200
  )

## -----------------------------------------------------------------------------
p12 <- prioritizr::problem(out_sf,
  z12,
  cost_column = c("Cost1", "Cost2", "Cost3")
) %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_manual_targets(targetAcrossSome) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(verbose = FALSE)

s12 <- p12 %>%
  prioritizr::solve.ConservationProblem()

## -----------------------------------------------------------------------------
s12F <- s12 %>%
  dplyr::select(tidyselect::starts_with(c("solution"))) %>%
  sf::st_drop_geometry() %>%
  tibble::as_tibble()

r12 <- prioritizr::eval_feature_representation_summary(p12, s12F)
print(r12)

## ----fig.width=9--------------------------------------------------------------
(gg_s12 <- splnr_plot_solution(
  s12,
  zones = TRUE,
  colorVals = c("#c6dbef", "#3182bd", "navyblue", "black"),
  legendLabels = c("Not selected", "Zone 1", "Zone 2", "Zone3")
) +
  splnr_gg_add(
    PUs = PUs, Bndry = Bndry, overlay = landmass,
    cropOverlay = PUs, ggtheme = splnr_theme
  ))

## ----echo = FALSE, message = FALSE, warning = FALSE, out.width='90%', fig.cap = "Types of multiple use in MSP according to [Schupp et al. 2019](https://doi.org/10.3389/fmars.2019.00165)"----
knitr::include_graphics(file.path("Schupp2019TypesMultipleUse.png"))

