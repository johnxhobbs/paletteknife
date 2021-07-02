#
# Functions for making axes a bit easier!

#' Auto Axis Tool
#'
#' Overlay base plot axis with custom intervals
#'
#' @details
#' Major and minor tick marks can be specified in a number of ways:
#'
#'  - As a character string if the axis is datetime, such as 'year' or 'hour'
#'    which are passed to seq().
#'
#'  - As a tick interval using the default `spacing = TRUE`
#'
#'  - As an approximate number of tick marks to include, using pretty() to find
#'    the best interval, using `spacing = FALSE`
#'
#' Major adds labels and ticks, minor is just half-sized ticks marks. Both
#' tick sizes can be changed (or direction changed) using `tck`.
#'
#' Three different datetime axis are possible: year, day-offset, seconds-offset. Use
#' `format` to specify how the label should appear, such as '%b %Y' (see ?strptime)
#'
#'  - Year should be treated as a conventional numeric axis, use major=1/12 not major='month'.
#'
#'  - day-offset is an axis of class(x) = 'Date' and is identified if the axis range exists
#'    within +/-9e4, or within dates 1723 - 2216.
#'
#'  - second-offset is an axis of class(x)='POSIXct' and is identified by a range outside
#'    of +/-9e4. This will give very strange results if your POSIXct axis is within
#'    24 hours of 1970-01-01.
#'
#' A grid can be added at the same time by setting major_grid or minor_grid to `TRUE`
#' or a colour. If `TRUE`, a transparent black is used by default.
#'
#' Any other options can be passed through to axis() directly (see ?axis), most
#' notably `las = 2` to rotate the labels.
#'
#' @examples
#' plot(sunspots) # These time series are actually given in decimal years
#'   autoaxis(side=3, major=50, minor=10, spacing=T, major_grid='coral', minor_grid=T)
#'   autoaxis(side=2, minor=5) #
#'   autoaxis(side=4, major=11, minor=25, spacing=F, las=2, cex.axis=0.5, tck=0.02)
#'
#' plot(seq(as.POSIXct('2020-01-01'),as.POSIXct('2020-01-03'),length.out=1e3), rnorm(1e3), xlab='POSIXct', xaxt='n')
#'   autoaxis(side=1, major='day', minor='hour', format = '%A')
#'
#' plot(seq(as.Date('2013-02-01'),as.Date('2020-01-03'),length.out=1e3), rnorm(1e3), xlab='Date', xaxt='n')
#'   autoaxis(side=1, major='year', minor='quarter', format = '%Y')
#' @export
autoaxis = function(side, major = NA, minor = NA, spacing = TRUE, format = '%Y-%m-%d', tck=-0.03, major_grid = FALSE, minor_grid = FALSE, ...){
  if(side %in% c(1,3))
    lims = par('usr')[1:2] # Drawing x-axis
  else
    lims = par('usr')[3:4] # Drawing y-axis

  date_axis = class(major)=='character' | class(minor)=='character'
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
    # Make sure all sequences start from 1st January
    # Might mean a huge vector if plotting seconds! 31mill...
    #lims = round(lims + c(-0.5, 0.5) * 365*86400, 'year')
  }

  # Create the spacings
  # If you want to get this SPACING rather than number-of-ticks, seq()
  # Start off by getting pretty start / finish
  if(!is.na(major)) major_at = pretty(lims, 2)
  if(!is.na(minor)) minor_at = pretty(lims, 2)
  if(spacing==TRUE){
    if(!is.na(major)) major_at = seq(major_at[1], major_at[length(major_at)], by=major)
    if(!is.na(minor)) minor_at = seq(minor_at[1], minor_at[length(minor_at)], by=minor)
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
    major_at = as.Date(major_at)
  if(!is.na(minor) & date_axis & date_format)
    minor_at = as.Date(minor_at)

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
