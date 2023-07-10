## code to prepare `DATASET` dataset goes here

library(magrittr)
set.seed(1222)

# I need a dataset I can use in the examples to demonstrate
# Create a dummy dataset for the moment. Could be better to use a real
# dataset from AquaMaps for the east coast of Australia.

# Create a dataset boundary
dat_bndry <- dplyr::tibble(x = 100, y = seq(-50, 0, by = 1)) %>%
  dplyr::bind_rows(dplyr::tibble(x = seq(100, 160, by = 1), y = 0)) %>%
  dplyr::bind_rows(dplyr::tibble(x = 160, y = seq(0, -50, by = -1))) %>%
  dplyr::bind_rows(dplyr::tibble(x = seq(160, 100, by = -1), y = -50)) %>%
  as.matrix() %>%
  list() %>%
  sf::st_polygon() %>%
  sf::st_sfc(crs = "EPSG:4326") %>%
  sf::st_sf() %>%
  sf::st_make_valid()

# Use boundary to create grid
dat_PUs <- sf::st_make_grid(dat_bndry, cellsize = 2) %>%
  sf::st_sf() %>%
  dplyr::mutate(cellID = dplyr::row_number()) # Add a cell ID reference

# Create some regionalisations
dat_region <- dat_PUs %>%
  sf::st_sf() %>%
  dplyr::mutate(Region = "Region1")
dat_region$Region[30:65] = "Region2"
dat_region$Region[66:80] = "Region3"


# Add some species probabilities
dat_species_prob <- dat_PUs %>%
  sf::st_sf() %>%
  dplyr::mutate(Spp1 = runif(n = dim(.)[[1]]),
                Spp2 = runif(n = dim(.)[[1]]),
                Spp3 = runif(n = dim(.)[[1]]),
                Spp4 = runif(n = dim(.)[[1]]),
                Spp5 = runif(n = dim(.)[[1]]))


# Convert the probabilities to binary data
col_name <- dat_species_prob %>%
  sf::st_drop_geometry() %>%
  colnames()

dat_species_bin <- dat_species_prob %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(dplyr::across(-dplyr::any_of(c("cellID", "geometry")), # Apply to all columns except geometry and cellID
                              ~ dplyr::case_when(. >= 0.5 ~ 1,
                                                 . < 0.5 ~ 0,
                                                 is.na(.data) ~ 0))) %>%
  sf::st_as_sf()

# Add a solution object
dat_soln <- prioritizr::problem(dat_species_bin %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
                                features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
                                cost_column = "Cost") %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_relative_targets(0.3) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(verbose = FALSE) %>%
  prioritizr::solve.ConservationProblem()


# Add a 2nd solution object
dat_soln2 <- prioritizr::problem(dat_species_bin %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
                                features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
                                cost_column = "Cost") %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_relative_targets(0.5) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(verbose = FALSE) %>%
  prioritizr::solve.ConservationProblem()



# Save the data
usethis::use_data(dat_bndry, dat_PUs, dat_region, dat_species_prob, dat_species_bin, dat_soln, dat_soln2, overwrite = TRUE)