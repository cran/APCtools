## ---- echo=FALSE--------------------------------------------------------------
# global settings
knitr::opts_chunk$set(fig.width = 10)

## ----packages, message = FALSE------------------------------------------------
library(APCtools)
library(dplyr)    # general data handling
library(mgcv)     # estimation of generalized additive regression models (GAMs)
library(ggplot2)  # data visualization
library(ggpubr)   # arranging multiple ggplots in a grid with ggarrange()

# set the global theme of all plots
theme_set(theme_minimal())

## ----data preparation---------------------------------------------------------
data(travel)

## ---- message=FALSE, warning=FALSE, fig.height=2.2----------------------------
gg1 <- plot_density(dat = travel, y_var = "mainTrip_distance", log_scale = TRUE)
gg2 <- plot_density(dat = travel, y_var = "mainTrip_distance", log_scale = TRUE,
                    plot_type = "boxplot")
gg3 <- plot_density(dat = travel, y_var = "household_size")

ggpubr::ggarrange(gg1, gg2, gg3, nrow = 1)

## ---- message = FALSE, fig.height=4-------------------------------------------
plot_variable(dat = travel, y_var = "mainTrip_distance",
              apc_dimension = "period", plot_type = "line", ylim = c(0,1000))

plot_variable(dat = travel, y_var = "household_size", apc_dimension = "period")

## ---- fig.width=8, message=FALSE----------------------------------------------
age_groups    <- list(c(80,89),c(70,79),c(60,69),c(50,59),
                      c(40,49),c(30,39),c(20,29))
period_groups <- list(c(1970,1979),c(1980,1989),c(1990,1999),
                      c(2000,2009),c(2010,2019))

plot_densityMatrix(dat              = travel,
                   y_var            = "mainTrip_distance",
                   age_groups       = age_groups,
                   period_groups    = period_groups,
                   log_scale        = TRUE)

## ---- fig.height=6.5, message=FALSE-------------------------------------------
plot_densityMatrix(dat                 = travel,
                   y_var               = "mainTrip_distance",
                   age_groups          = age_groups,
                   period_groups       = period_groups,
                   highlight_diagonals = list("born 1950 - 1959" = 8,
                                              "born 1970 - 1979" = 10),
                   log_scale           = TRUE)

## ---- fig.height=6.5, message=FALSE-------------------------------------------
dist_cat_breaks <- c(1,500,1000,2000,6000,100000)
dist_cat_labels <- c("< 500 km","500 - 1,000 km", "1,000 - 2,000 km", "2,000 - 6,000 km", "> 6,000 km")

plot_densityMatrix(dat              = travel,
                   y_var            = "mainTrip_distance",
                   age_groups       = age_groups,
                   period_groups    = period_groups,
                   log_scale        = TRUE,
                   y_var_cat_breaks = dist_cat_breaks,
                   y_var_cat_labels = dist_cat_labels,
                   highlight_diagonals = list("born 1950 - 1959" = 8,
                                              "born 1970 - 1979" = 10),
                   legend_title     = "Distance category")

## ---- fig.height=6.5, message=FALSE-------------------------------------------
plot_densityMatrix(dat                 = travel,
                   y_var               = "household_size",
                   age_groups          = age_groups,
                   period_groups       = period_groups,
                   highlight_diagonals = list("born 1950 - 1959" = 8,
                                              "born 1970 - 1979" = 10))

## ---- fig.height=6.5, fig.width=8---------------------------------------------
plot_APCheatmap(dat            = travel,
                y_var          = "mainTrip_distance",
                y_var_logScale = TRUE,
                bin_heatmap    = FALSE,
                markLines_list = list(cohort = c(1900,1920,1939,1946,
                                                 1966,1982,1994)))

## ---- fig.height=6.5, fig.width=8---------------------------------------------
plot_APCheatmap(dat            = travel,
                y_var          = "mainTrip_distance",
                y_var_logScale = TRUE,
                markLines_list = list(cohort = c(1900,1920,1939,1946,
                                                 1966,1982,1994)))

## ---- fig.height=6.5----------------------------------------------------------
plot_APChexamap(dat            = travel,
                y_var          = "mainTrip_distance",
                y_var_logScale = TRUE)

## -----------------------------------------------------------------------------
# GAM without covariates
model_pure <- gam(mainTrip_distance ~ te(age, period, bs = "ps", k = c(8,8)),
                  data = travel)

# GAM including covariates
model_cov  <- gam(mainTrip_distance ~ te(age, period, bs = "ps", k = c(8,8)) +
                    residence_region + household_size + s(household_income),
                  data = travel)

# create a named list of the two models, useful for some functions
model_list <- list("pure model"      = model_pure,
                   "covariate model" = model_cov)

## ---- fig.height=3, fig.width=8-----------------------------------------------
plot_APCheatmap(dat   = travel,
                model = model_pure)

## ---- fig.height=6.5----------------------------------------------------------
plot_APChexamap(dat   = travel,
                model = model_pure)

## -----------------------------------------------------------------------------
plot_marginalAPCeffects(model    = model_pure,
                        dat      = travel,
                        variable = "age")

## -----------------------------------------------------------------------------
plot_jointMarginalAPCeffects(model_list  = model_list,
                             dat         = travel,
                             vlines_list = list("cohort" = c(1900,1920,1939,
                                                             1946,1966,1982,
                                                             1994)))

## -----------------------------------------------------------------------------
plot_partialAPCeffects(model    = model_pure,
                       dat      = travel,
                       variable = "period")

## -----------------------------------------------------------------------------
create_APCsummary(model_list = model_list,
                  dat        = travel)

## -----------------------------------------------------------------------------
APCtools::plot_linearEffects(model_cov)
APCtools::plot_1Dsmooth(model_cov, select = 2)

## -----------------------------------------------------------------------------
summary_list <- create_modelSummary(model_list)
summary_list[[1]]
summary_list[[2]]

