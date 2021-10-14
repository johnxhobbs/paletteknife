#
# Functions for making axes a bit easier!

#' Auto Axis Tool
#'
#' Overlay base plot axis with custom intervals
#'
#' Major and minor tick marks can be specified in a number of ways:
#'
#'  - As a character string if the axis is datetime, such as 'year' or 'hour'
#'    which are passed to `seq()`. These can be prefixed with an integer multiplier,
#'    for example '6 hour', '30-sec',  or '10year'. Any non-alphanumeric separator
#'    can be used, or none.
#'
#'  - As a tick interval using the default `spacing = TRUE`
#'
#'  - As an approximate number of tick marks to include, using `pretty()` to find
#'    the best interval, using `spacing = FALSE`
#'
#' Major adds labels and ticks, minor is just half-sized ticks marks. Both
#' tick sizes can be changed (or direction changed) using `tck`.
#'
#' Three different datetime axis are possible: year, day-offset, seconds-offset. Use
#' `format` to specify how the label should appear, such as '%b %Y' (see `?strptime`)
#'
#'  - Year should be treated as a conventional numeric axis, use `major=1/12` not `major='month'`
#'
#'  - day-offset is an axis of `class(x)=='Date'` and is identified if the axis range exists
#'    within +/-9e4, meaning within dates 1723 - 2216
#'
#'  - second-offset is an axis of `class(x)=='POSIXct'` and is identified by a range outside
#'    of +/-9e4. This will give very strange results if your entire POSIXct axis is within
#'    24 hours of 1970-01-01
#'
#' A grid can be added at the same time by setting `major_grid` or `minor_grid` to `TRUE`
#' or a colour string. If `TRUE`, a transparent black is used by default.
#'
#' Any other options can be passed through to `axis()` directly (see `?axis`), most
#' notably `las = 2` to rotate the labels, and `cex.axis` for label size.
#'
#' This does NOT work well for `barplot()` categorical axis, for this continue to use
#' the basic `axis()` function with custom labels, see examples.
#'
#' @examples
#' plot(sunspots) # This time series is actually given in decimal years
#'   autoaxis(side=3, major=50, major_grid='coral', minor=10, minor_grid=TRUE, spacing=TRUE)
#'   autoaxis(side=4, major=11, minor=25, spacing=FALSE, las=2, cex.axis=0.5, tck=0.02)
#'
#' plot(seq(as.POSIXct('2020-01-01'),as.POSIXct('2020-01-03'),length.out=1e3),
#'     rnorm(1e3), xlab='POSIXct', xaxt='n')
#'   autoaxis(side=1, major='day', minor='hour', format='%A')
#'   autoaxis(side=3, major='6 hour', format='%H:%M')
#'
#' plot(seq(as.Date('2013-02-01'),as.Date('2020-01-03'),length.out=1e3),
#'     rnorm(1e3), xlab='Date', xaxt='n')
#'   autoaxis(side=1, major='year', minor='quarter', format='%Y')
#'   autoaxis(side=3, minor='3month', minor_grid=TRUE)
#'
#' # For barplot() use base functions - remember to set width=1, space=0
#' # otherwise bars will not be plotted on integer x-coordinates
#' barplot(mtcars$mpg, width=1, space=0, ylab='mpg')
#'   # Adjust the x-axis down by 0.5 so that the tick is in centre of each bar
#'   axis(side=1, at=-0.5+1:length(mtcars$mpg), labels=rownames(mtcars), las=2 )
#'   # Often prettier, label each bar inside the bar itself using text()
#'   text(x=-1+1:length(mtcars$mpg), y=1, pos=4,
#'     labels=rownames(mtcars), srt=90, cex=0.7)
#'   # autoaxis can still be used for adjusting the numeric scale
#'   autoaxis(side=2, major=5, major_grid=TRUE, minor=1, minor_grid=TRUE)
#'
#' @param side  Side to add axis, 1 = bottom, 2 = left, 3 = top, 4 = right
#' @param major Spacing of major axis ticks and labels (or approx. number of
#'              intervals if `spacing = FALSE`). If the axis is date or time,
#'              use a interval specified in `?seq.POSIXt`, such as 'sec' or
#'              'week'
#' @param major_grid Add grid lines corresponding to major axis ticks, `TRUE`
#'              to get default translucent black, otherwise colour (name or hex)
#' @param minor Spacing (or number) of minor ticks (note, no label for minor).
#'              If given as a character string, it will pass to `seq.POSIXt`
#' @param minor_grid Add gridlines for minor ticks, `TRUE` uses transparent
#'              black, otherwise colour string
#' @param format Date or time format for major axis -- `major` must be a
#'              character string in this case
#' @param tck   Size of axis tick: minor axis will always take half this value
#' @param spacing Should `major` and `minor` be interpreted as tick spacing
#'              (default) or approximate number of ticks
#' @param ...   Additional arguemnts passed to `axis()`, for example `las=2`
#'              for perpendicular labels
#'
#' @return No return value (`NULL`)
#'
#' @import graphics
#' @import grDevices
#' @export
autoaxis = function(side, major = NA, major_grid = FALSE, minor = NA, minor_grid = FALSE,
                    format = '%Y-%m-%d', spacing = TRUE, tck=-0.03, ...){
  if(side %in% c(1,3))
    lims = par('usr')[1:2] # Drawing x-axis
  else
    lims = par('usr')[3:4] # Drawing y-axis

  date_axis = class(major)=='character' | class(minor)=='character'
  if(date_axis==TRUE & spacing==FALSE) stop('spacing must be TRUE for time-interval axes')
  date_format = FALSE

  # Is a date axis wanted here?
  if(date_axis){
    # Guess whether datetime is in seconds (POSIXct) or days (as.Date)
    # If plot range is 1723-2216 (Date) or within 24hr of 1970-01-01
    # ==> assume it's a date, not seconds. Pretty safe!
    if(lims[1] > -9e4 & lims[2] < 9e4){
      date_format = TRUE
      lims = as.POSIXct.Date(lims, origin = '1970-01-01')}
    else
      lims = as.POSIXct.numeric(lims, origin = '1970-01-01')

    # Now also check for multiple-of-division for example '6 hour'
    major_multiple = 1
    minor_multiple = 1

    if(substr(major,1,1) %in% 1:9){
      major_multiple = as.integer(sub('[^1-9].*$','',major))
      major = sub('.*[^a-z]','',major)}

    if(substr(minor,1,1) %in% 1:9){
      minor_multiple = as.integer(sub('[^1-9].*$','',minor))
      minor = sub('.*[^a-z]','',minor)}
  }

  # Start off by getting pretty start / finish -- used for spacing = T or F
  if(!is.na(major)) major_at = pretty(lims, 2)
  if(!is.na(minor)) minor_at = pretty(lims, 2)

  # Create the tick 'at'
  # If you want to get this SPACING rather than number-of-ticks, seq()
  if(spacing==TRUE){
    if(!is.na(major)) major_at = seq(major_at[1], major_at[length(major_at)], by=major)
    if(!is.na(minor)) minor_at = seq(minor_at[1], minor_at[length(minor_at)], by=minor)
  }
  # Apply the skip-a-few if something like '6 hour' has been given
  if(date_axis==TRUE){
    if(!is.na(major)) major_at = major_at[seq(1, length(major_at), by=major_multiple)]
    if(!is.na(minor)) minor_at = minor_at[seq(1, length(minor_at), by=minor_multiple)]
  }
  # Other option is to give major as an approx number of ticks
  if(spacing==FALSE){
    if(!is.na(major)) major_at = pretty(lims, major)
    if(!is.na(minor)) minor_at = pretty(lims, minor)
  }

  if(!is.na(major)){
    if(date_axis)
      major_labs = format(major_at,format)
    else
      major_labs = major_at
  }

  if(!is.na(major) & date_axis & date_format)
    major_at = as.numeric(major_at) / 86400 # Do not use as.Date - gets rid of decimal hour
  if(!is.na(minor) & date_axis & date_format)
    minor_at = as.numeric(minor_at) / 86400

  # Add the axis
  if(!is.na(major)) axis(side=side, at=major_at, labels=major_labs, tck=tck, ...)
  if(!is.na(minor)) axis(side=side, at=minor_at, labels=FALSE, tck=tck/2, ...)

  if(major_grid==FALSE & minor_grid==FALSE)
    return(NULL)

  if(major_grid==TRUE) major_grid = '#00000030' # Default transparent grey overlay
  if(minor_grid==TRUE) minor_grid = '#00000010'
  if(major_grid==FALSE) major_grid = NA   # Invisible colour
  if(minor_grid==FALSE) minor_grid = NA
  # Add grid lines - maybe needs more fine tune options here!
  if(side %in% c(1,3)){
    if(!is.na(major)) abline(v = major_at, col = major_grid)
    if(!is.na(minor)) abline(v = minor_at, col = minor_grid)
  }
  if(side %in% c(2,4)){
    if(!is.na(major)) abline(h = major_at, col = major_grid)
    if(!is.na(minor)) abline(h = minor_at, col = minor_grid)
  }

  return(NULL)
}
