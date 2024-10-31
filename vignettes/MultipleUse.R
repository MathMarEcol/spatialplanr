## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>",
warning = FALSE,
cache = FALSE,
message = FALSE,
eval = TRUE
)

## ----setup--------------------------------------------------------------------
library(spatialplanr)
set.seed(100)

## -----------------------------------------------------------------------------
Region <- "Coral Sea" # "Australia"
Type <- "Oceans" # "EEZ"

## -----------------------------------------------------------------------------
PU_size <- 107460 # m

## -----------------------------------------------------------------------------
cCRS <- "ESRI:54009"

## -----------------------------------------------------------------------------
Bndry <- splnr_get_boundary(Limits = Region, Type = Type, cCRS = cCRS)

landmass <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
) %>%
  sf::st_transform(cCRS)

## -----------------------------------------------------------------------------
PUs <- spatialgridr::get_grid(boundary = Bndry,
                              crs = cCRS,
                              output = "sf_hex",
                              resolution = PU_size)


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

