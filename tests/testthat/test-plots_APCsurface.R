
test_that("plot_APCheatmap", {
  
  testthat::skip_if_not_installed("mgcv")
  
  data(drug_deaths)
  
  # plot hexamap of observed data
  gg1 <- plot_APCheatmap(dat = drug_deaths, y_var = "mortality_rate")
  
  gg2 <- plot_APCheatmap(dat = drug_deaths, y_var = "mortality_rate",
                         bin_heatmap = FALSE,
                         apc_range = list("cohort" = 1980:2010))
  
  gg3 <- plot_APCheatmap(dat = drug_deaths, y_var = "mortality_rate",
                         markLines_list = list("age"    = c(20,70),
                                               "period" = c(1990,2010),
                                               "cohort" = c(1985,1993)),
                         apc_range = list("cohort" = 1980:2010))
  
  expect_s3_class(gg1, class = c("gg","ggplot"))
  expect_s3_class(gg2, class = c("gg","ggplot"))
  expect_s3_class(gg3, class = c("gg","ggplot"))
  
  
  # plot heatmap of smoothed structure
  model <- gam(mortality_rate ~ te(period, age), data = drug_deaths)
  drug_deaths$mortality_rate <- drug_deaths$mortality_rate + 1
  model_logLink <- bam(mortality_rate ~ te(period, age),
                       family = Gamma(link = "log"), data = drug_deaths)
  
  gg1 <- plot_APCheatmap(dat = drug_deaths, model = model)
  gg2 <- plot_APCheatmap(dat = drug_deaths, model = model_logLink)
  
  expect_s3_class(gg1, class = c("gg","ggplot"))
  expect_s3_class(gg2, class = c("gg","ggplot"))
})



test_that("plot_APChexamap", {
  
  testthat::skip_if_not_installed("mgcv")
  
  data(travel)
  data(drug_deaths)
  
  # helper functions
  expect_identical(round(compute_xCoordinate(period_vec = c(1980,1999)), 2),
                   c(1714.73,1731.18))
  expect_identical(compute_yCoordinate(period_vec = c(1990, 1999), age_vec = c(20,50)),
                   c(-975.0, -949.5))
  
  # plot hexamap of observed data
  expect_null(plot_APChexamap(dat = drug_deaths, y_var = "mortality_rate"))
  
  expect_null(plot_APChexamap(dat = drug_deaths, y_var = "mortality_rate",
                              y_var_logScale = TRUE, color_range = c(1,50),
                              apc_range = list("cohort" = 1980:2010)))
  
  expect_null(plot_APChexamap(dat = travel, y_var = "mainTrip_distance",
                              y_var_logScale = TRUE))
  
  # error when 0 values are logarithmized
  expect_error(plot_APChexamap(dat = drug_deaths, y_var = "mortality_rate",
                               y_var_logScale = TRUE, color_range = c(0,50)))
  
  
  # plot hexamap of smoothed structure
  model <- gam(mortality_rate ~ te(period, age), data = drug_deaths)
  
  expect_null(plot_APChexamap(dat = drug_deaths, model = model))
})
