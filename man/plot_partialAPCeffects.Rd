% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_partialAPCeffects.R
\name{plot_partialAPCeffects}
\alias{plot_partialAPCeffects}
\title{Partial APC plots based on an estimated GAM model}
\usage{
plot_partialAPCeffects(
  model,
  dat,
  variable = "age",
  hide_partialEffects = FALSE,
  vlines_vec = NULL,
  return_plotData = FALSE
)
}
\arguments{
\item{model}{Optional regression model estimated with \code{\link[mgcv]{gam}}
or \code{\link[mgcv]{bam}} to estimate a smoothed APC surface. Only used if
\code{y_var} is not
specified.}

\item{dat}{Dataset with columns \code{period} and \code{age}. If \code{y_var}
is specified, the dataset must contain the respective column. If \code{model}
is specified, the dataset must have been used for model estimation with
\code{gam} or \code{bam}.}

\item{variable}{One of \code{c("age","period","cohort")}, specifying the
temporal dimension for which the partial effect plots should be created.}

\item{hide_partialEffects}{If TRUE, only the marginal effect will be plotted.
Defaults to FALSE.}

\item{vlines_vec}{Optional numeric vector of values on the x-axis where
vertical lines should be drawn. Can be used to highlight the borders of
specific age groups, time intervals or cohorts.}

\item{return_plotData}{If TRUE, a list of the datasets prepared for plotting
is returned instead of the ggplot object. The list contains one dataset each
for the overall effect (= evaluations of the APC surface to plot the partial
effects) and for each marginal APC effect (no matter the specified value of
the argument \code{variable}). Defaults to FALSE.}
}
\value{
ggplot object (if \code{hide_partialEffects} is TRUE) or a plot grid
created with \code{\link[ggpubr]{ggarrange}} (if FALSE).
}
\description{
Create the partial APC plots based on an APC model estimated as a semiparametric
additive regression model with \code{\link[mgcv]{gam}} or \code{\link[mgcv]{bam}}.
}
\details{
If the model was estimated with a log or logit link, the function
automatically performs an exponential transformation of the effect.
}
\examples{
library(APCtools)
library(mgcv)

data(travel)
model <- gam(mainTrip_distance ~ te(age, period), data = travel)

plot_partialAPCeffects(model, dat = travel, variable = "age")

# mark specific cohorts
plot_partialAPCeffects(model, dat = travel, variable = "cohort",
                       vlines_vec = c(1966.5,1982.5,1994.5))

}
\references{
Weigert, M., Bauer, A., Gernert, J., Karl, M., Nalmpatian, A.,
Küchenhoff, H., and Schmude, J. (2021). Semiparametric APC analysis of
destination choice patterns: Using generalized additive models to quantify
the impact of age, period, and cohort on travel distances.
\emph{Tourism Economics}. doi:10.1177/1354816620987198.
}
\author{
Alexander Bauer \email{alexander.bauer@stat.uni-muenchen.de},
Maximilian Weigert \email{maximilian.weigert@stat.uni-muenchen.de}
}
