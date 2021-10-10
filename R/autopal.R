
#' Map Colours From Value
#'
#' Create a vector of colours and associated legend for easier base plots
#'
#' Helper function for using colours in R's default `plot` and `legend`. Colours
#' from built-in palettes are automatically scaled to return a vector of colours
#' and a hidden `.autolegend_persistent` which contains the correct legend mapping for
#' `autolegend()`.
#'
#' A discrete palette is used for factor and character inputs whilst a
#' continuous palette is used for integer and numeric.
#'
#' Colour sets built-in so far are held in lists starting `pals.` and can be
#' visualized most easily with `pals_display()`. The `set` argument can be
#' any of the colour set names listed here (such as 'magma'), or from `palette.pals()`,
#' or finally as a custom-defined vector, such as `set = rainbow(5)`.
#'
#' The current lists of palettes included with paletteknife all being with `pal.`
#'
#' - **`pals.viridis`**
#'
#'    All of the continuous palette forked from the `viridisLite` package maintained by Simon Garnier.
#'     - Contains: `cividis` `inferno` `magma` `mako` `plasma` `rocket` `turbo` `viridis`
#'
#' - **`pals.rcolorbrewer`**
#'
#'   All of the palettes included in RColorBrewer
#'   - Categorical:
#'   `Accent` `Set1` `Set2` `Set3` `Paired` `Pastel1` `Pastel2` `Dark2`
#'
#'   - Continuous:
#'   `Greys` `Blues` `BuGn` `BuPu` `Greens` `GnBu` `PuBu` `Purples` `PuBuGn`
#'   `YlGnBu` `YlGn`
#'   `YlOrBr` `YlOrRd` `Oranges` `OrRd` `Reds` `RdPu` `PuRd`
#'
#'   - Divergent:
#'   `Spectral` `RdYlBu` `RdYlGn`
#'   `BrBG` `RdBu` `RdGy` `PiYG` `PRGn` `PuOr`
#'
#' - **`pals.misc`**
#'
#'   - Sasha Trubetskoy  (2017): *List of 20 Simple, Distinct Colors*: `sasha`
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
#' plot(iris$Sepal.Length, iris$Petal.Length, cex=3, pch=16,
#'     col=autocol(iris$Petal.Width, set='PuBuGn', alpha=0.8, legend_len=12) )
#'   autolegend('topleft', title='Petal.Width', ncol=3)
#'   # Also try simplest "autolegend()" for click-to-draw
#'
#' # Try scales which include NA in both colour and alpha channel
#' # Note, a crude fix to reverse the palette order, and changing the autolegend labels
#' with(airquality, plot(Temp, col=autocol(x=-Solar.R, set='Spectral', alpha=Ozone,
#'     na_colour='black'), pch=16, cex=sqrt(Wind) ))
#'   .autolegend_persistent[[1]] = -.autolegend_persistent[[1]]
#'   autolegend('bottom', inset=1, bty='n', horiz=TRUE)
#'   # Note inset=1 draws on opposite side ie above not below plot area
#'
#'
#' # Here we want a summary plot ordered by level, so need to create a colour vector to match
#' # 'Alphabet' is a built-in colour set, see "palette.pals()"
#' mixedbag = as.factor(sample(letters,1000,replace=TRUE))
#'   plot(x=mixedbag, y=rnorm(1000), col=autocol(levels(mixedbag), set='Alphabet'))
#'   autolegend('bottom', ncol=9, cex=0.7)
#'
#' # Maintain the order of strings
#' barplot(1:8, col=autocol(LETTERS[8:1]))
#'   autolegend('topleft')
#'
#' # Any unusual formats are coerced to numeric and the legend converted back
#' mydates = as.Date('2000-01-01')+0:100
#'   plot(mydates, pch=16, col=autocol(mydates, set=rainbow(10), bias=2) )
#'   autolegend(x=0, y=mydates[100], title='My Dates')
#'
#' # Timeseries objects plot as a line, but can overlay with points()
#' plot(airmiles)
#'   points(airmiles, pch=15, col=autocol(airmiles, set='Reds'))
#'
#' # Use the limits to clip or augment the colour-scale
#' layout(matrix(1:2))
#'   plot(runif(10), col=autocol(1:10, limits=c(0,20)), pch=16,
#'     main='Data split over two plots with same scale')
#'   plot(runif(10), col=autocol(c(100,20:12), limits=c(0,20)), pch=16)
#'   text(1, 0.5, pos=4, xpd=NA,
#' 'This point has a
#' value of 100 but
#' clipped to max
#' colour == 20')
#'   autolegend('bottom', inset=1, horiz=TRUE) # Draws above!
#'   layout(1)
#'
#' @param x Vector to be mapped to colours
#' @param set Colour set to use -- see Details for full list. A default `sasha` or `viridis` is chosen if empty.
#' @param alpha Transparency as a single value or as another vector (recycled to fill).
#'              If it is a vector, all values are scaled from 0:max(alpha) meaning transparent:opaque.
#'              Single values must be in range 0-1. If `NA` no alpha channel is added.
#' @param limits Colour scale limits as absolute range `c(0,10)` or `NA` = full range
#' @param na_colour Colour to represent NA-values, default `NA` returns a colour of `NA` (thus not plotted)
#' @param bias Skew to apply to colour-ramp (>1 increases resolution at low end, <1 at the high end)
#' @param legend_len Continuous legend target size
#'
#' @import graphics
#' @import grDevices
#' @export
autocol = function(x, set = '', alpha = NA, limits = NA, na_colour = NA, bias = 1, legend_len = 6){
  # Sanitise the input arguments
  # TODO

  # Choose whether continuous or categorical datatype based on class(x)
  pal_type = switch (class(x)[1],
    'factor' = 'categorical',
    'character' = 'categorical',
    'logical' = 'categorical',
    'integer' = 'continuous',
    'numeric' = 'continuous',
    'trynumeric'
  )

  if(pal_type=='trynumeric'){
    original_class = class(x) # Will use later to convert legend back to input class
    x = as.numeric(x)
    if(!length(x) > 1) stop('Could not convert as.numeric(x)')
  }

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
    .autolegend_persistent <<- list(legend_labels,legend_fill)
  }

  if(pal_type=='continuous' | pal_type=='trynumeric'){
    chosen_colour_ramp = colorRamp(get_set(set, default = 'viridis'), space = 'Lab', bias = bias)
    # Correct limits
    if(is.na(limits[1]))
      limits = range(x, na.rm = TRUE)

    create_autolegend_data(limits = limits, chosen_colour_ramp = chosen_colour_ramp, legend_len = legend_len,
                           override_class = if(pal_type=='trynumeric') original_class else NA )

    x_scaled = (x - limits[1]) / (limits[2] - limits[1])
    x_scaled = pmin(1,pmax(0, x_scaled))
    # rgb() cannot pass na values, so find and replace these NA for now
    x_scaled[x_na] = 0
    res_pal = rgb(chosen_colour_ramp(x_scaled), maxColorValue = 255)
    res_pal[x_na] = NA
  }

  # Deal with any NA colours
  res_pal[x_na] = na_colour

  # Deal with the alpha channel -- this is the same for categorical and discrete
  # The values are mapped 0 (transparent) to 255 (solid), such that either 1.0
  # is solid, or whatever the maximum value is.
  # col2rgb() allows the res_pal so far to have colour names --> hex codes
  if(!is.na(alpha[1])){
    max_alpha = if(length(alpha)==1) 1 else max(alpha,na.rm=TRUE)
    alpha = pmax(0, alpha, na.rm=TRUE) # Negative alpha and NA are both turned invisible
    res_pal = rgb(t(col2rgb( res_pal )), alpha=255*alpha/max_alpha, maxColorValue=255 )
    }
  return(res_pal)
}

#' Add Auto-Generated Legend
#'
#' Add a legend for the last autocol() set generated
#'
#' If no location (such as 'bottom', or an x,y coordinate) is given, then it
#' calls the locator() crosshairs so the position of the legend can be picked
#' interactively. All arguments are passed to legend(), see ?legend for a full
#' list.
#'
#' Legend labels and fill are both generated by autopal() and autocol() and stored in
#' the hidden object `.autolegend_persistent` which can be manipulated if needs be.
#'
#' See more examples in ?autocol for plot + legend.
#'
#' @examples
#' # Simplest version: click-to-draw with locator()
#' plot(1:10, pch=16, col=autocol(1:10, 'Blues', legend_len=5))
#' # autolegend() # Try me!
#'
#' # Other neat versions -- note ?legend
#' autolegend(x=6, y=4, ncol=2, title='Draw at (6,4)')
#' autolegend('topleft', title='"topleft"', ncol=2, bty='n')
#' autolegend('bottom', inset=1, horiz=TRUE, bty='n', title='Above plot')
#'
#' # Use pch (and optionally pt.cex) in legend -- these get recycled
#' autolegend('bottom', horiz=TRUE, pch=16, pt.cex=3, title='pch=16, pt.cex=3')
#' autolegend('right', pch=1:10, pt.cex=2, title='pch=1:10')
#'
#' # Manipulate the legend text, for example with format()
#' heatmap(as.matrix(eurodist), col=autopal('turbo', limits=range(eurodist)) )
#' .autolegend_persistent[[1]] = format(.autolegend_persistent[[1]],big.mark=',')
#' autolegend('bottom', inset=1, horiz=TRUE, title='Misleading miles between cities')
#'
#' # No helper exists yet for creating size or shape legends -- follow this idea...
#' with(airquality, plot(Temp, pch=16, cex=Solar.R/100, col=autocol(Ozone, set='Reds')))
#' cex_legend = pretty(airquality$Solar.R)
#' legend('bottom', pt.cex=cex_legend/100, legend=cex_legend, pch=1,
#'   horiz=TRUE, title='Solar.R', bty='n' )
#' autolegend('bottom', inset=1, horiz=TRUE, title='Ozone', bty='n')
#'
#' @param ... Arguments passed directly to `legend` -- legend text and fill are taken
#'            automatically from hidden `.autolegend_persistent`. See examples for useful
#'            parameters, including `pch` and `pt.cex`.
#'
#' @import graphics
#' @import grDevices
#' @export
autolegend = function(...){
  if(!exists('.autolegend_persistent')) stop('Must call autocol(...) first to create .autolegend_persistent object')

  if('pch' %in% names(list(...)))  # If pch is provided, we need to use col=   not   fill=
    legend(..., locator(n=1), legend = .autolegend_persistent[[1]], col = .autolegend_persistent[[2]], xpd = NA)

  else
    legend(..., locator(n=1), legend = .autolegend_persistent[[1]], fill = .autolegend_persistent[[2]], xpd = NA)
}

#' Auto-Palette
#'
#' Return a palette vector from one of the built-in sets
#'
#' This can be used where a palette is provided rather than a mapped colour
#' vector, for example image(). The limits must be specified for the `autolegend`
#' to be generated for the new palette.
#' Custom colour limits can be set using `breaks` or `levels` (see examples) if
#' the same colour range is needed across several plots.
#' See ?autocol for list of all available colour sets.
#'
#' @examples
#' image(volcano, col=autopal('RdYlGn', n=100, limits=c(50,200), bias=1.5),
#'     breaks=seq(50,200,length.out=101) )
#'   autolegend('bottom', inset=1, ncol=5)
#'
#' # Or using the slightly smarter filled.contour
#' filled.contour(volcano, col=autopal('RdYlGn', n=20, limits=c(100,150)),
#'   levels=seq(50,200,length.out=21) )
#'
#' @param set Colour set to use -- see ?autocol for full list. A default `sasha` or `viridis` is chosen if empty.
#' @param n Length of colour vector to return, must be at least 2
#' @param limits Colour scale limits to pass to legend eg `c(0,10)` -- if left as `NA` no autolegend will be generated
#' @param bias Skew to apply to colour-ramp (>1 increases resolution at low end, <1 at the high end)
#' @param legend_len Continuous legend target size
#'
#' @import graphics
#' @import grDevices
#' @export
autopal = function(set = '', n = 30, limits = NA, bias = 1, legend_len = 6){
  chosen_colour_ramp = colorRamp(get_set(set), space = 'Lab', bias = bias)
  if(!is.na(limits[1])){
    original_class = NA

    if(!class(limits) %in% c('numeric','integer')){
      original_class = class(limits)
      limits = as.numeric(limits)}

    create_autolegend_data(limits = limits, chosen_colour_ramp = chosen_colour_ramp, legend_len = legend_len,
                           override_class = original_class)
  }
  palcols = rgb(chosen_colour_ramp(c(0,seq(0,1,length.out=n-2),1)), maxColorValue = 255)
  return(palcols)
}

#' @import graphics
#' @import grDevices
create_autolegend_data = function(limits, chosen_colour_ramp, legend_len = 6, override_class = NA){
  # Make legend data -- get pretty intervals and then cap ends to suitable decimal places
  legend_labels = pretty(limits, n = legend_len)
  longest_label = max(nchar(as.character(legend_labels)))
  legend_labels[1] = signif(limits[1], digits = longest_label)
  legend_labels[length(legend_labels)] = signif(limits[2], digits = longest_label)
  legend_scaled = (legend_labels-min(legend_labels))/diff(range(legend_labels))
  legend_fill = rgb(chosen_colour_ramp(legend_scaled), maxColorValue = 255)

  if(!is.na(override_class[1]))
    attr(legend_labels, 'class') = override_class

  # Push legend levels into the parent scope as hidden object for drawing later
  # This is done to allow a colour vector to be returned, and the associated
  # legend information to be stashed for the subsequent add legend
  .autolegend_persistent <<- list(legend_labels,legend_fill)
}
