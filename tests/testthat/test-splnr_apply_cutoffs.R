
testthat::test_that("Correct function output", {
  expect_s3_class(
    splnr_apply_cutoffs(dat_species_prob, Cutoffs = 0.5), "sf"
  )
})

testthat::test_that("Correct function output", {
  expect_s3_class(
    splnr_apply_cutoffs(dat_species_prob, Cutoffs = 0.5, inverse = TRUE), "sf"
  )
})


testthat::test_that("Correct function output", {
  expect_s3_class(
    splnr_apply_cutoffs(dat_species_prob,
                        Cutoffs = c("Spp1" = 0.5, "Spp2" = 0.4, "Spp3" = 0.6, "Spp4" = 0.5, "Spp5" = 0.5)), "sf"
  )
})

