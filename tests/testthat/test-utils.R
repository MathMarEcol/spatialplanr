testthat::test_that("Correct function output", {
  expect_s3_class(dat_species_prob %>%
    splnr_replace_NAs("Spp2"), "sf")
})
