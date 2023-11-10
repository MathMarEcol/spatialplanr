pDat <- prioritizr::problem(dat_species_bin %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
                            features = c("Spp1", "Spp2", "Spp3"),
                            cost_column = "Cost"
) %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_relative_targets(0.3) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(verbose = FALSE)

soln <- pDat %>%
  prioritizr::solve.ConservationProblem()


testthat::test_that("Correct function output", {

  expect_s3_class(
    splnr_get_featureRep(
      soln = soln,
      pDat = pDat
    ), "tbl_df"

  )
})


testthat::test_that("Correct function output", {
  expect_s3_class(
    splnr_plot_featureRep(splnr_get_featureRep(
      soln = soln,
      pDat = pDat), category = dat_category)
    , "gg"
  )
})
