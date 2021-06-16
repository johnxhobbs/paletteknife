#
# 
#

#' Auto-Assign Colour Palette
#'
#' Create a ready-to-go vector of colours and pretty legend
#'
#' @details
#' Helper function for using colours in R's default `plot` and `legend`. Colours
#' from built-in palettes are automatically scaled to return a vector of colours
#' with legend information included as attributes.
#'
#' A discrete palette is used for factor and character inputs whilst a
#' continuous palette is used for integer and numeric.
#'
#' Colour sets built-in so far are:
#'
#' *Continuous:*
#'
#' All of these continuous palettes have been forked from the `viridisLite` package maintained by Simon Garnier.
#'
#'  - `viridis`
#'
#'    Perceptually uniform which allows very fair comparison between colours.
#'    Created by Simon Garnier and released under the MIT license.
#'    Default for `set = 'muted'`.
#'
#'  - `inferno`
#'
#'    A red version of `viridis` which maps the lowest value to black, good for heatmaps.
#'
#'  - `turbo`
#'
#'    An improved matlab-like 'jet' palette, useful if you want high contrast at the expense of proportionality.
#'    Created by Anton Mikhailov, Copyright 2019 Google and released under Apache-2.0 license.
#'    Default for `set = 'bright'`.
#'
#'
#' *Discrete:*
#'
#'  - `pastel`
#'
#'    For 12 or fewer colours, ColorBrewer Set3 is used. For more than 12,
#'    Zeileis, Hornik and Murrell (2009): *Escaping RGBland: Selecting Colors for Statistical Graphics*.
#'
#'  - `bright`
#'
#'    For 12 or fewer colours, ColorBrewer Paired is used. For more than 12,
#'    Sasha Trubetskoy  (2017): *List of 20 Simple, Distinct Colors*.
#'
#' *Divergent:*
#'   TODO
#'
#' Custom limits can be specified, in two ways:
#'   - percentile limits `c('5%','90%')`: this is essential for clamping down on outliers which compress the colour scale
#'   - absolute limits `c(0,10)`: useful if multiple plots using the same range are required for cross-comparison
#'   - default behaviour `limits = NA`: adjust to exactly fit input range
#'
#' If the colour scale does not provide enough distinction, it might be necessary to
#' transform the input vector, for example using `log(abs(x))`.
#'
#' @examples
#' autopal(LETTERS[20:30], set = 'muted', na_colour = 'grey')
#'
#' autopal(as.factor(c(1:10,NA,2:9,NA,3:5)), set = 'bright')
#'
#' colpal = autopal(log(mtcars$qsec), 'viridis', n = 6, alpha = 0.8)
#'   plot(mtcars$hp, mtcars$wt, col = colpal, cex = mtcars$cyl, pch = 16)
#'   autolegend(colpal, 'topleft', title = 'log(wt)', ncol = 2)
#'
#' # Here we want a summary plot ordered by level, so need to create a colour vector to match
#' mixedbag = as.factor(sample(letters,1000,replace=T))
#'   plot(x = mixedbag, y = rnorm(1000), col = autopal(levels(mixedbag)))
#'   autolegend(autopal(levels(mixedbag)), 'bottom', ncol = 9, bty = 'n')
#'
#' @param x Vector to be mapped to colours
#' @param set Colour set to use - 'bright' or 'muted' for categorical, or specify directly ('turbo', 'viridis', 'inferno')
#' @param alpha Transparency as a single value or as another vector (recycled to fill)
#' @param limits Colour scale limits as absolute range `c(0,10)`, or as percentile to remove outliers `c('0%','99.9%')`, or NA = all
#' @param na_colour Colour to represent NA, defaults to NA (do not plot)
#' @param n Continuous legend target size
#' @return `colpal` Vector of colour hex strings for plotting, and can be used with `autolegend` which uses its attributes for legend-plotting
#' @export
autopal = function(x, set = 'bright', alpha = 1.0, limits = NA, na_colour = NA, n = 8){
  # Sanitise the input arguments
  # TODO

  # Choose whether continuous or categorical datatype based on class(x)
  pal_type = switch (class(x),
    'factor' = 'categorical',
    'character' = 'categorical',
    'integer' = 'continuous',
    'numeric' = 'continuous',
    stop('Must be one of: factor, character, integer, numeric')
  )

  # Remove NA values here and reinsert at the end
  i = !is.na(x)
  x = x[i]

  if(pal_type=='categorical'){
    # Currently 4 good palettes: default to ColorBrewer for small sets, and Sasha / Zeileis
    pal_n = length(unique(x))
    if(pal_n <= 12 & set=='bright') choice = colorbrewer_paired
    if(pal_n <= 12 & set=='muted')  choice = colorbrewer_pastel
    if(pal_n > 12 & set=='bright')  choice = sasha
    if(pal_n > 12 & set=='muted')   choice = zeileis

    col_level = as.integer(as.factor(x)) %% nrow(choice)
    col_level[col_level==0] = nrow(choice)

    colpal = rgb(choice[col_level,1],choice[col_level,2],choice[col_level,3], alpha = alpha)

    legend_labels = 1:length(unique(x)) %% nrow(choice)
    legend_labels[legend_labels==0] = nrow(choice)
    legend_fill = rgb(choice[legend_labels,1],choice[legend_labels,2],choice[legend_labels,3], alpha = alpha)
    legend_labels = as.character(unique(as.factor(x)))
  }

  if(pal_type=='continuous'){
    if(set=='bright') choice = turbo   # bit arbitrary
    if(set=='muted') choice = viridis  # bit arbitrary
    if(set=='turbo') choice = turbo
    if(set=='viridis') choice = viridis
    if(set=='inferno') choice = inferno

    # Trim data as appropriate - clamp outliers to ends
    if(!is.na(limits[1])){
      if(is.character(limits)){
        limits = as.numeric(gsub('[^0-9\\.]','',limits))
        limits = quantile(x = x, probs = limits/100)
      }
      x = pmax(pmin(x,limits[2]),limits[1])
    }
    if(is.na(limits[1])){
      limits = range(x)
    }

    # All colour_sets are 256 long, so make sure we've got the full range
    x_scaled = 1 + 255 * (x - limits[1]) / (limits[2] - limits[1])

    colpal = rgb(choice[x_scaled,1],choice[x_scaled,2],choice[x_scaled,3], alpha = alpha)

    # Attach legend-details as attribute
    legend_labels = pretty(limits, n = n)
    legend_labels[1] = signif(limits[1], digits = 3)
    legend_labels[length(legend_labels)] = signif(limits[2], digits = 3)

    legend_fill = approx(seq(limits[1],limits[2],length.out=256), 1:256, xout=legend_labels )$y
    legend_fill[1] = 1
    legend_fill[length(legend_fill)] = 256
    legend_fill = rgb(choice[legend_fill,1],choice[legend_fill,2],choice[legend_fill,3])
  }

  # Reinsert any NA values
  res_pal = character(length(x))
  res_pal[i] = colpal
  res_pal[!i] = na_colour

  # Attach legend info as an attribute
  attr(res_pal, 'legend_labels') = legend_labels
  attr(res_pal, 'legend_fill') = legend_fill
  #names(colpal) = x

  return(res_pal)
}

#' @export
#' @rdname autopal
autolegend = function(colpal, ...){
  legend(..., legend = attr(colpal,'legend_labels'), fill = attr(colpal,'legend_fill'))
}
