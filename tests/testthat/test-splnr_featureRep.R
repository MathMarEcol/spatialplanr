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




testthat::test_that("Correct function output", {
  s1 <- soln %>%
    tibble::as_tibble()

  df_rep_imp <- prioritizr::eval_feature_representation_summary(
    pDat,
    s1[, "solution_1"]
  ) %>%
    dplyr::select(feature, relative_held) %>%
    dplyr::mutate(relative_held = relative_held * 100)

  imp_layers <- c("Spp1", "Spp3")

  target <- data.frame(feature = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5")) %>%
    dplyr::mutate(class = dplyr::if_else(.data$feature %in% imp_layers,
                                         "important", "representative"
    )) %>%
    dplyr::mutate(target = dplyr::if_else(class == "important",
                                          50 / 100, 30 / 100
    ))

  df <- merge(df_rep_imp, target) %>%
    dplyr::select(-target) %>%
    na.omit() %>%
    dplyr::rename(value = relative_held) %>%
    dplyr::rename(group = class)

  colors <- c(
    "important" = "darkgreen",
    "representative" = "darkred"
  )
  legends <- c("Important", "Representative")

  expect_s3_class(
    (splnr_plot_circBplot(df,
                          legend_list = legends,
                          legend_color = colors,
                          impTarget = 50, repTarget = 30))
    , "gg"
  )
})

