testthat::test_that("Correct function output", {
  expect_s3_class(dat_species_prob %>%
                    splnr_replace_NAs("Spp2"), "sf")
})


testthat::test_that("Correct function output", {
  expect_s3_class(dat_species_prob %>%
                    dplyr::mutate(Spp2 = dplyr::if_else(Spp2 < 0.01, NA, Spp2)) %>%
                    splnr_replace_NAs("Spp2"), "sf")
})




testthat::test_that("Correct function output", {
  expect_s3_class(splnr_create_polygon(x = dplyr::tibble(x = seq(-50, 50, by = 1), y = 120) %>%
                                         dplyr::bind_rows(dplyr::tibble(x = 50, y = seq(120, 180, by = 1))) %>%
                                         dplyr::bind_rows(dplyr::tibble(x = seq(50, -50, by = -1), y = 180)) %>%
                                         dplyr::bind_rows(dplyr::tibble(x = -50, y = seq(150, 120, by = -1)))), "sf"
  )
})



testthat::test_that("Correct function output", {
  expect_s3_class(
    splnr_match_names(dat_region %>% dplyr::select(-cellID),
                      c("Region1" = "SE Aust", "Region2" = "Tas", "Region3" = "NE Aust")), "sf"
  )
})


testthat::test_that("Correct function output", {
  expect_s3_class(
    dat_species_prob %>%
      dplyr::mutate(Spp1 = Spp1 * 100) %>%
      splnr_scale_01(col_name = "Spp1"), "sf"
  )
})


testthat::test_that("Correct function output", {
  expect_vector(
    dat_species_prob %>%
      splnr_featureNames(exclude = c("cellID"))
  )
})

testthat::test_that("Correct function output", {
  expect_s3_class(
    dat_species_prob %>%
      dplyr::mutate(Spp1 = Spp1 * 100) %>%
      splnr_scale_01(col_name = "Spp1"), "sf"
  )
})

testthat::test_that("Correct function output", {
  expect_s3_class(
    dat_species_prob %>%
      dplyr::mutate(Spp1 = Spp1 * 10) %>%
      splnr_scale_01(col_name = "Spp1"), "sf"
  )
})

testthat::test_that("Correct function output", {
  expect_s3_class(
    dat_species_prob %>%
      dplyr::mutate(Spp1 = Spp1 * 1000) %>%
      splnr_scale_01(col_name = "Spp1"), "sf"
  )
})

testthat::test_that("Correct function output", {
  expect_vector(dat_species_prob %>%
                  splnr_featureNames(exclude = c("cellID"))
  )
})



testthat::test_that("Correct function output", {
  expect_s3_class(
    rnaturalearth::ne_coastline(returnclass = "sf") %>%
      splnr_convert_toPacific(), "sf"
  )
})


testthat::test_that("Correct function output", {
  expect_s3_class(
    dat_species_prob %>%
      splnr_arrangeFeatures(), "sf"
  )
})





testthat::test_that("Correct function output", {

  pDat1 <- prioritizr::problem(dat_species_bin %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
                               features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
                               cost_column = "Cost"
  ) %>%
    prioritizr::add_min_set_objective() %>%
    prioritizr::add_relative_targets(0.3) %>%
    prioritizr::add_binary_decisions() %>%
    prioritizr::add_default_solver(verbose = FALSE)


  soln1 <- pDat1 %>%
    prioritizr::solve.ConservationProblem()

  soln2 <- prioritizr::problem(dat_species_bin %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
                               features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
                               cost_column = "Cost"
  ) %>%
    prioritizr::add_min_set_objective() %>%
    prioritizr::add_relative_targets(0.32) %>%
    prioritizr::add_binary_decisions() %>%
    prioritizr::add_default_solver(verbose = FALSE) %>%
    prioritizr::solve.ConservationProblem()

  expect_s3_class(
    splnr_get_selFreq(solnMany = list(soln1, soln2), type = "list"), "sf"
  )
})
