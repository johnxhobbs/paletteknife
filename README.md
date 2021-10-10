
# paletteknife

<!-- badges: start -->
  [![CRAN status](https://www.r-pkg.org/badges/version/paletteknife)](https://CRAN.R-project.org/package=paletteknife)
  [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
  <!-- badges: end -->

The goal of paletteknife is to make assigning colour scales, adding legends, and
adjusting axes a more effortless task when using R base graphics. 

The principal functions included are:

 - `autocol()` - creates a colour vector from a continuous or categorical vector
 - `autolegend()` - draws a legend with the last `autocol()` scales
 - `autoaxis()` - adds more axes labels and gridlines
 - `autopal()` - creates a colour palette between limits
 
Extensive examples have been included, see `?autocol` and `?autolegend` for starters.
Functions themselves are relatively simple but serve as shortcuts so that adding a 
colour scale and legend can be done in-line and interactively, contributing to a
smoother worflow.

For deployment, the underlying base functions can be extracted and embedded if
minimal dependencies are required. With that aim, `viridisLite` and `RColorBrewer`
palettes are included so that these tools are as portable as possible.

Suggestions and contributions are very welcome for further base graphics tasks
which can be helpfully streamlined. See maintainer contact details and repository. 

## Installation

You can install the released version of paletteknife from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("paletteknife")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(paletteknife)

pals_display(pals = pals.rcolorbrewer)

aq_dates = seq(as.Date('1973-05-01'), by=1, length.out=nrow(airquality))
with(airquality, plot(aq_dates, Temp, pch=16, cex=Solar.R/100, col=autocol(Ozone, set='Reds'),
                      xaxt='n', xlab='', main='New York Air Quality'))

autoaxis(side=1, major='week', major_grid=TRUE, minor='day', las=2, format='%d %b')
autoaxis(side=2, minor=1, minor_grid=TRUE)

autolegend(ncol=5, title='Size: Solar, Colour: Ozone')  # Click on plot

```

