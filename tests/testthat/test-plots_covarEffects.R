
test_that("plot_linearEffects", {
  
  testthat::skip_if_not_installed("mgcv")
  library(mgcv)
  
  data(travel)
  
  model <- gam(mainTrip_distance ~ te(period, age) + household_size +
                 residence_region, data = travel)
  model_logLink <- bam(mainTrip_distance ~ te(period, age) +
                         s(household_income) + household_size +
                         residence_region,
                       family = Gamma(link = "log"), data = travel)
  
  gg1 <- plot_linearEffects(model)
  gg2 <- plot_linearEffects(model_logLink)
  
  expect_s3_class(gg1, class = c("gg","ggplot"))
  expect_s3_class(gg2, class = c("gg","ggplot"))
})


test_that("plot_1Dsmooth", {
  
  testthat::skip_if_not_installed("mgcv")
  library(mgcv)
  
  data(travel)
  
  model <- bam(mainTrip_distance ~ te(period, age) + s(household_income) +
                 household_size + residence_region, data = travel)
  model_logLink <- gam(mainTrip_distance ~ te(period, age) +
                         s(household_income) + household_size +
                         residence_region,
                       family = Gamma(link = "log"), data = travel)
  
  # plot_1Dsmooth
  gg1 <- plot_1Dsmooth(model, select = 2, alpha = 0.1)
  expect_warning({
    gg2 <- plot_1Dsmooth(model_logLink, select = 2, alpha = 0.1)
  })
  
  expect_s3_class(gg1, class = c("gg","ggplot"))
  expect_s3_class(gg2, class = c("gg","ggplot"))
  
  expect_error(plot_1Dsmooth(model, select = 20))
  
  # get_plotGAMobject
  x <- APCtools:::get_plotGAMobject(model)
  
  expect_identical(names(x[[1]])[1:3], c("x","y","scale"))
})