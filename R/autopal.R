
#' Map Colours From Value
#'
#' Create a vector of colours and associated legend for easier base plots
#'
#' @details
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
#' - All of the palettes incldued in RColorBrewer
#'
#'   `Accent` `Blues` `BrBG` `BuGn` `BuPu` `Dark2` `GnBu`
#'   `Greens` `Greys` `Oranges` `OrRd` `Paired` `Pastel1`
#'   `Pastel2` `PiYG` `PRGn` `PuBu` `PuBuGn` `PuOr` `PuRd`
#'   `Purples` `RdBu` `RdGy` `RdPu` `RdYlBu` `RdYlGn`
#'   `Reds` `Set1` `Set2` `Set3` `Spectral` `YlGn` `YlGnBu`
#'   `YlOrBr` `YlOrRd`
#'
#' - Sasha Trubetskoy  (2017): *List of 20 Simple, Distinct Colors*
#'
#'   `sasha`
#'
#' Custom limits can be specified, in two ways:
#'   - percentile limits `c('5%','90%')`: this is essential for clamping down on outliers which compress the colour scale
#'   - absolute limits `c(0,10)`: useful if multiple plots using the same range are required for cross-comparison
#'   - default behaviour `limits = NA`: adjust to exactly fit input range
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
#' mixedbag = as.factor(sample(letters,1000,replace=T))
#'   plot(x = mixedbag, y = rnorm(1000), col = autocol(levels(mixedbag)))
#'   autolegend('bottom', ncol = 9)
#'
#' @param x Vector to be mapped to colours
#' @param set Colour set to use - see ?autocol for full list. A default `sasha` or `viridis` is chosen if empty.
#' @param alpha Transparency as a single value or as another vector (recycled to fill) - if it is a vector, all values are scaled from 0:max(alpha) -> transparent:opaque. Single values must be in range 0-1. If NA no alpha hex is added.
#' @param limits Colour scale limits as absolute range `c(0,10)`, or as percentile to remove outliers `c('0%','99.9%')`, or NA = all
#' @param na_colour Colour to represent NA, default NA returns a colour of NA (invisible)
#' @param legend_len Continuous legend target size
#' @param bias Skew to apply to colour-ramp (2 increases resolution at low end, 0.5 at the high end)
#' @export
autocol = function(x, set = '', alpha = NA, limits = NA, na_colour = NA, bias = 1, legend_len = 6){
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
  # Get ready to replace these again at the end
  x_na = is.na(x)

  if(pal_type=='categorical'){

    if(set==''){
      if(length(unique(x)) <= 12)
        set = 'Paired'
      else
        set = 'sasha'
    }
    set_palette = get(set)
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
    if(set=='') set = 'viridis'
    chosen_colour_ramp = colorRamp(get(set), space = 'Lab', bias = bias)
    limits = correct_limits(x = x, limits = limits)
    create_autolegend_data(limits = limits, chosen_colour_ramp = chosen_colour_ramp, legend_len = legend_len)

    x_scaled = (x - limits[1]) / (limits[2] - limits[1])
    x_scaled = pmin(1,pmax(0, x_scaled))
    # rgb() cannot pass na values, so find and replace
    x_scaled[x_na] = 0
    res_pal = rgb(chosen_colour_ramp(x_scaled), maxColorValue = 255)
    res_pal[x_na] = NA
  }

  # Deal with any NA colours
  res_pal[x_na] = na_colour

  # Deal with the alpha channel - this is the same for categorical and discrete
  # The values are mapped 0 (transparent) to 255 (solid), such that either 1.0
  # is solid, or whatever the maximum value is.
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
#' autolegend('topright', ncol = 2)
#' autolegend(horiz = T, bty = 'n') # Try clicking just under the plot title
#' @export
autolegend = function(...){
  if(!exists('.autocol_legend')) stop('Must call autocol(...) first to create .autocol_legend data')

  legend(..., locator(n=1), legend = .autocol_legend[[1]], fill = .autocol_legend[[2]], xpd = NA)
}

#' Create Capped Colour Palette For Images
#'
#' Returns a colour palette and necessary breaks for image
#'
#' See ?autocol for more detail on everything except examples. The mechanism is slightly
#' different here because the full palette and corresponding breaks are given.
#' The main help here (rather than just using zlim) is to cap outliers, rather
#' than omitting them.
#'
#' @examples
#' z = matrix(runif(100), nrow=10)
#' imcol = autoimcol(z = z, set = 'viridis', limits = c(0.1,0.9) )
#' image(z, col = imcol, breaks = .auto_imbreaks)
#' autolegend()
autoimcol = function(z, set, limits = NA, n = 30, legend_len = 6, bias = 1){
  chosen_colour_ramp = colorRamp(get(set), space = 'Lab', bias = bias)
  limits = correct_limits(x = z, limits = limits)
  create_autolegend_data(limits = limits, chosen_colour_ramp = chosen_colour_ramp, legend_len = legend_len)
  .auto_imbreaks <<- c(min(z,na.rm=T), seq(limits[1],limits[2],length.out=n-1), max(z,na.rm=T))
  # Note, these colours apply to the middle of each interval, given that the first interval is all the capped values, duplicate first (and last) colour
  imcolours = rgb(chosen_colour_ramp(c(0,seq(0,1,length.out=n-2),1)), maxColorValue = 255)
  return(imcolours)
}

#' z = matrix(sort(rnorm(1e4)), nrow=1e2)
#' image(z, col = autopal('RdYlGn', n=100, limits=c(-3,5), bias = 2), breaks=seq(-3,5,length.out=101) )
#' autolegend()
autopal = function(set, n = 30, limits = NA, bias = 1, legend_len = 6){
  chosen_colour_ramp = colorRamp(get(set), space = 'Lab', bias = bias)
  if(!is.na(limits[1]))
    create_autolegend_data(limits = limits, chosen_colour_ramp = chosen_colour_ramp, legend_len = legend_len)
  palcols = rgb(chosen_colour_ramp(c(0,seq(0,1,length.out=n-2),1)), maxColorValue = 255)
  return(palcols)
}

correct_limits = function(x, limits){
  if(is.na(limits[1])){
    limits = range(x, na.rm = T)
  }
  else{
    if(is.character(limits)){
      limits = as.numeric(gsub('[^0-9\\.]','',limits))
      limits = quantile(x = x, probs = limits/100)
    }
  }
  return(limits)
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
