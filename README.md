
<!-- README.md is generated from README.Rmd. Please edit that file -->

# eatPlot

<!-- badges: start -->

[![R-CMD-check](https://github.com/nickhaf/eatPlot/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nickhaf/eatPlot/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/nickhaf/eatPlot/branch/main/graph/badge.svg)](https://app.codecov.io/gh/nickhaf/eatPlot?branch=main)
<!-- badges: end -->

The goal of eatPlot is to easily plot results stemming from the eatRep
package.

## Installation

You can install the development version of eatPlot from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nickhaf/eatPlot")
```

## Basic workflow

### Data preperation

`eatPlot` makes it simple to prepare `eatRep` output for plotting.
Depending on whether you want to plot cross-sectional data or trenddata,
you can use either:

``` r
library(eatPlot)
barplot_data <- prep_no_trend(
  dat = adjusted_means,
  grouping_var = "adjust",
  columns = "adjust",
  competence = "GL",
  sig_niveau = 0.05
)
```

or:

``` r
lineplot_data <- prep_trend(dat = trend_books, 
                            grouping_var = "KBuecher_imp3", 
                            competence = "GL", 
                            sig_niveau = 0.05)
```

You can also remove specific states, if you don’t want to plot them:

``` r
lineplot_data <- filter_rows(lineplot_data, state = "wholeGroup", remove = TRUE)
```

### Plotting

The prepared data can then fed into one of the plotting functions. For
example, if you want to plot a barplot with an ajacent table, first plot
both plots indidually, and then merge them together:

``` r
p_table <- plot_table(barplot_data[["plot_table"]])
p_bar <- plot_bar(barplot_data)

plot_table_bar(p_table, p_bar)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

Or, if you want a lineplot:

``` r
p_line <- plot_lineplot(lineplot_data, axis_x_label_centralize = 0.15)
p_line
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

The plots are optimally adjusted for A4-pages, and can be saved as pdf
as follows:

``` r
save_plot(p_line, filename = "../lineplot.pdf")
#> png 
#>   2
```
