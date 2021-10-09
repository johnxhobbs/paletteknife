
# paletteknife

The goal of paletteknife is to make assigning colour scales, adding legends, and
adjusting axes a more effortless task when using R base graphics. 

The principal functions included are:

 - `autocol()` - creates a colour vector from a continuous or categorical vector
 - `autolegend()` - draws a legend with the last `autocol()` scales
 - `autoaxis()` - adds more axes labels and gridlines
 - `autopal()` - creates a colour palette between limits
 
Extensive examples have been included, see `?autocol` and `?autolegend` for starters.
Functions themselves are relatively simple but serve as shortcuts for making better
plots. Suggestions and contributions are very welcome for other base graphics tasks
which can be helpfully decorated, see maintainer contact details.

## Installation

You can install the released version of paletteknife from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("paletteknife")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(paletteknife)

aq_dates = seq(as.Date('1973-05-01'), by=1, length.out=nrow(airquality))
with(airquality, plot(aq_dates, Temp, pch=16, cex=Solar.R/100, col=autocol(Ozone, set='Reds'),
                      xaxt='n', xlab='', main='New York Air Quality'))

autoaxis(side=1, major='week', major_grid=TRUE, minor='day', las=2, format='%d %b')
autoaxis(side=2, minor=1, minor_grid=TRUE)

autolegend(ncol=5, title='Size: Solar, Colour: Ozone')  # Click on plot

```

