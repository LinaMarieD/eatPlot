% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_points.R
\name{plot_points}
\alias{plot_points}
\title{Plot Points.}
\usage{
plot_points(
  data_plot_points,
  point_values = "est_noTrend_noComp",
  point_sig = "sig_noTrend_noComp",
  y_range,
  plot_settings = plotsettings_lineplot()
)
}
\arguments{
\item{data_plot_points}{Data frame with the point estimates for every year. Prepared by ...}

\item{point_values}{Character string of the column name in \code{plot_dat[["plot_points"]]} containing the y-values for the plotted points. Defaults to \code{est_noTrend_noComp}.}

\item{point_sig}{Character string of the column name containing significance values for \code{point_values}. Defaults to \code{"sig_noTrend_Comp_crossDiff_wholeGroup"}.}

\item{y_range}{Numeric vector with two elements, indicating the min and max for the y axis.}

\item{plot_settings}{Named list constructed with \code{plotsettings_lineplot()}. Defaults to a list with all settings set to \code{0}. There are several predefined lists with optimized settings for different plots. See \code{plotsettings_lineplot()} for an overview.}
}
\value{
ggplot2 object
}
\description{
Plot Points.
}
\examples{
# tbd
}
