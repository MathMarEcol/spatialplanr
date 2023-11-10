testthat::test_that("Correct function output", {
  expect_s3_class(dat_species_prob %>%
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

#
# testthat::test_that("Correct function output", {
#   expect_s3_class(
#   )
# })
