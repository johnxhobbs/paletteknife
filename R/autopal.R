
#' Map Colours From Value
#'
#' Create a vector of colours and associated legend for easier base plots
#'
#' Helper function for using colours in R's default `plot` and `legend`. Colours
#' from built-in palettes are automatically scaled to return a vector of colours
#' and a hidden `.autocol_legend` which contains the correct legend mapping for
#' `autolegend()`.
#'
#' A discrete palette is used for factor and character inputs whilst a
#' continuous palette is used for integer and numeric.
#'
#' Colour sets built-in so far are:
#'
#' - All of the continuous palette forked from the `viridisLite` package maintained by Simon Garnier.
#'
#'   `cividis` `inferno` `magma` `mako` `plasma` `rocket`
#'   `turbo` `viridis`
#'
#' - All of the palettes included in RColorBrewer
#'   - Categorical (size 8 or 9, `Paired` and `Set3` 12)
#'
#'   `Accent` `Set1` `Set2` `Set3` `Paired` `Pastel1` `Pastel2` `Dark2`
#'
#'   - Continuous (in order of 'heat')
#'
#'   `Greys` `Blues` `BuGn` `BuPu` `Greens` `GnBu` `PuBu` `Purples` `PuBuGn`
#'   `YlGnBu` `YlGn`
#'   `YlOrBr` `YlOrRd` `Oranges` `OrRd` `Reds` `RdPu` `PuRd`
#'
#'   - Divergent (first three go via yellow to form a rainbow palette)
#'
#'   `Spectral` `RdYlBu` `RdYlGn`
#'   `BrBG` `RdBu` `RdGy` `PiYG` `PRGn` `PuOr`
#'
#' - Sasha Trubetskoy  (2017): *List of 20 Simple, Distinct Colors*
#'
#'   `sasha`
#'
#' Custom limits can be specified using `c(0,10)`. This is useful if multiple
#' plots using the same range are required for cross-comparison. Default
#' behaviour (`limits = NA`) sets the range to exactly fit.
#'
#' The skew of the colourscale can be adjusted with `bias`, for example if `x`
#' has an exponential distribution, a bias value > 1 will bring out contrast at
#' the low end.
#'
#' @examples
#' # Try scales which include NA in both colour and alpha channel
#' # Note, a crude fix to reverse the palette order, and changing the autolegend labels
#' with(airquality, plot(Temp, col=autocol(x=-Solar.R, set='Spectral', alpha = Ozone, na_colour = 'black'), pch=16, cex = sqrt(Wind) ))
#'   .autocol_legend[[1]] = -.autocol_legend[[1]]
#'   autolegend(bty = 'n', horiz = T)
#'
#' plot(iris$Sepal.Length, iris$Petal.Length, col = autocol(iris$Petal.Width, 'PuBuGn', alpha = 0.8, bias = 1.5, legend_len = 12), cex = 3, pch = 16 )
#'   autolegend('topleft', title = 'Petal.Width', ncol = 3)
#'
#' # Here we want a summary plot ordered by level, so need to create a colour vector to match
#' # 'Alphabet' is a built-in colour set, see "palette.pals()"
#' mixedbag = as.factor(sample(letters,1000,replace=T))
#'   plot(x = mixedbag, y = rnorm(1000), col = autocol(levels(mixedbag), set='Alphabet'))
#'   autolegend('bottom', ncol = 9)
#'
#' # Maintain the order of strings
#' barplot(1:8, col=autocol(LETTERS[8:1]))
#'   autolegend('topleft')
#'
#' # All colour scales must be string / number, so will require 'unclassing'
#' x=as.Date('2000-01-01')+0:100
#'   plot(x, pch=16, col=autocol(as.numeric(x)) )
#'   attr(.autocol_legend[[1]], 'class') <- class(x)
#'   autolegend()
#'
#' # Logical vectors can be plotted, but also trivial with default palette()
#' plot(runif(4), col=autocol(c(T,F,F,T), set = c('red','black')), pch=16, cex=5)
#' plot(runif(4), col=2-c(T,F,F,T), pch=16, cex=5)
#'
#' # Use the limits to clip or augment the colour-scale
#' layout(matrix(1:2))
#'   plot(runif(10), col=autocol(1:10, limits = c(0,20)), pch=16, main='Low data')
#'   plot(runif(10), col=autocol(c(100,20:12), limits = c(0,20)), pch=16, main='High data')
#'   text(1,0.5,'This point has a \n value of 100 but  \n clipped to max \n colour = 20', pos=4, xpd = NA)
#'   autolegend(horiz=TRUE)
#'   layout(1)
#'
#'
#' @param x Vector to be mapped to colours
#' @param set Colour set to use - see ?autocol for full list. A default `sasha` or `viridis` is chosen if empty.
#' @param alpha Transparency as a single value or as another vector (recycled to fill).
#'              If it is a vector, all values are scaled from 0:max(alpha) meaning transparent:opaque.
#'              Single values must be in range 0-1. If `NA` no alpha channel is added.
#' @param limits Colour scale limits as absolute range `c(0,10)` or `NA` = full range
#' @param na_colour Colour to represent NA-values, default `NA` returns a colour of `NA` (thus not plotted)
#' @param bias Skew to apply to colour-ramp (>1 increases resolution at low end, <1 at the high end)
#' @param legend_len Continuous legend target size
#' @export
autocol = function(x, set = '', alpha = NA, limits = NA, na_colour = NA, bias = 1, legend_len = 6){
  # Sanitise the input arguments
  # TODO

  # Choose whether continuous or categorical datatype based on class(x)
  pal_type = switch (class(x),
    'factor' = 'categorical',
    'character' = 'categorical',
    'logical' = 'categorical',
    'integer' = 'continuous',
    'numeric' = 'continuous',
    stop('Must be one of: factor, character, logical, integer, numeric')
  )
  # Get ready to replace these again at the end
  x_na = is.na(x)

  if(pal_type=='categorical'){

    set_palette = get_set(set, default = 'sasha')

    if(class(x)=='character') x = factor(x, levels=unique(x))
    col_level = as.integer(as.factor(x)) %% length(set_palette)
    col_level[col_level==0] = length(set_palette)

    res_pal = set_palette[col_level]

    legend_labels = 1:length(unique(x)) %% length(set_palette)
    legend_labels[legend_labels==0] = length(set_palette)
    legend_fill = set_palette[legend_labels]
    legend_labels = as.character(unique(as.factor(x)))
    .autocol_legend <<- list(legend_labels,legend_fill)
  }

  if(pal_type=='continuous'){
    chosen_colour_ramp = colorRamp(get_set(set, default = 'viridis'), space = 'Lab', bias = bias)
    # Correct limits
    if(is.na(limits[1]))
      limits = range(x, na.rm = T)

    create_autolegend_data(limits = limits, chosen_colour_ramp = chosen_colour_ramp, legend_len = legend_len)

    x_scaled = (x - limits[1]) / (limits[2] - limits[1])
    x_scaled = pmin(1,pmax(0, x_scaled))
    # rgb() cannot pass na values, so find and replace these NA for now
    x_scaled[x_na] = 0
    res_pal = rgb(chosen_colour_ramp(x_scaled), maxColorValue = 255)
    res_pal[x_na] = NA
  }

  # Deal with any NA colours
  res_pal[x_na] = na_colour

  # Deal with the alpha channel - this is the same for categorical and discrete
  # The values are mapped 0 (transparent) to 255 (solid), such that either 1.0
  # is solid, or whatever the maximum value is.
  # col2rgb() allows the res_pal so far to have colour names --> hex codes
  if(!is.na(alpha[1])){
    max_alpha = if(length(alpha)==1) 1 else max(alpha,na.rm=T)
    alpha = pmax(0, alpha, na.rm = T) # Negative alpha and NA are both turned invisible
    res_pal = rgb(t(col2rgb( res_pal )), alpha=255*alpha/max_alpha, maxColorValue = 255 )
    }
  return(res_pal)
}

#' Add Auto-Generated Legend
#'
#' Add a legend for the last autocol() set generated
#'
#' If no location (such as 'bottom') is given, then it calls the locator() crosshairs
#' so the position of the legend can be picked interactively.
#'
#' @examples
#' autolegend('topright', ncol = 2, title = 'Legend')
#' autolegend(horiz = T, bty = 'n') # Try clicking just under the plot title
#'
#' @param ... Arguments passed directly to `legend` - legend text and fill are taken automatically from hidden `.autocol_legend`
#' @export
autolegend = function(...){
  if(!exists('.autocol_legend')) stop('Must call autocol(...) first to create .autocol_legend data')

  legend(..., locator(n=1), legend = .autocol_legend[[1]], fill = .autocol_legend[[2]], xpd = NA)
}

#' Auto-Palette
#'
#' Return a palette vector from one of the built-in sets
#'
#' This can be used where a palette is provided rather than a mapped colour
#' vector, for example image(). The limits can be specified for the `autolegend`
#' generated every time a new palette is made.
#' Custom colour limits can be set using `breaks` or `levels` (see examples) if
#' the same colour range is needed across several plots.
#' See ?autocol for list of all available colour sets.
#'
#' @examples
#' image(volcano, col = autopal('RdYlGn', n=100, limits=c(50,200), bias = 1.5), breaks=seq(50,200,length.out=101) )
#'   autolegend()
#' # Or using the slightly smarter filled.contour
#' filled.contour(volcano, col = autopal('RdYlGn', n=20, limits=c(100,150)), levels = seq(50,200,length.out=21) )
#'
#' @param set Colour set to use - see ?autocol for full list. A default `sasha` or `viridis` is chosen if empty.
#' @param limits Colour scale limits to pass to legend eg `c(0,10)` - if left as `NA` no autolegend will be generated
#' @param bias Skew to apply to colour-ramp (>1 increases resolution at low end, <1 at the high end)
#' @param legend_len Continuous legend target size
#' @export
autopal = function(set, n = 30, limits = NA, bias = 1, legend_len = 6){
  chosen_colour_ramp = colorRamp(get_set(set), space = 'Lab', bias = bias)
  if(!is.na(limits[1]))
    create_autolegend_data(limits = limits, chosen_colour_ramp = chosen_colour_ramp, legend_len = legend_len)
  palcols = rgb(chosen_colour_ramp(c(0,seq(0,1,length.out=n-2),1)), maxColorValue = 255)
  return(palcols)
}

create_autolegend_data = function(limits, chosen_colour_ramp, legend_len = 6){
  # Make legend data - get pretty intervals and then cap ends to suitable decimal places
  legend_labels = pretty(limits, n = legend_len)
  longest_label = max(nchar(as.character(legend_labels)))
  legend_labels[1] = signif(limits[1], digits = longest_label)
  legend_labels[length(legend_labels)] = signif(limits[2], digits = longest_label)
  legend_scaled = (legend_labels-min(legend_labels))/diff(range(legend_labels))
  legend_fill = rgb(chosen_colour_ramp(legend_scaled), maxColorValue = 255)

  # Push legend levels into the global environment for plotting later
  # This is a hidden variable
  .autocol_legend <<- list(legend_labels,legend_fill)
}
