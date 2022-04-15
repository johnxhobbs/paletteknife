#

#' Auto Zoom to Create a Dynamic Plot
#'
#' Replaces `plot()` with an interactive loop which allows user to click twice
#' on the plot window to redraw with new limits. Press ESCAPE to finish.
#'
#' Click twice to set the new plot extents. If both clicks are on one of the axis
#' (outside of the plot area) then only this axis is zoomed. Click twice on the
#' same spot to reset the zoom to the entire plot.
#'
#' Decorations such as axis or legends are added using the `decoration` argument.
#' This may change in the future to be more elegant, such as using `match.arg()`
#'
#' @examples
#' autozoom(airmiles)
#'
#' autozoom(faithful, cex=runif(272), decoration=function() {autoaxis(3); autoaxis(4)} )
#'
#' autozoom(faithful, xaxt='n', col=autocol(sample(1:4,272,replace=TRUE)), pch=16,
#'   decoration=function() autolegend('above') )
#'
#' with(airquality, autozoom(Solar.R, Wind, pch=16, cex=3,
#'   col=autocol(Temp,'Reds',alpha=0.5),
#'   decoration=function() autolegend('above') ) )
#'
autozoom = function(x, ..., decoration=function() NULL ){
  # Is it possible to pass plot() as an object? Worst case: pass as string and eval()
  plot(x=x, ...)
  decoration()

  message('Click twice to update zoom limits, double click slowly to reset, press ESCAPE to quit')
  for(i in 1:10){
    zoombox = locator(n=2)
    # Order correctly then flatten
    zoombox = unlist(lapply(zoombox, sort))

    # Break if you hit ESCAPE
    if(length(zoombox) < 4) break

    boundingbox = par('usr')

    # message(diff(zoombox[1:2]) / sum(boundingbox[1:2])*100, '   ', diff(zoombox[3:4]) / sum(boundingbox[3:4])*100 )
    # Reset zoom on double click?
    dbl_click_tolerance = 1e-2
    if(diff(zoombox[1:2]) / sum(boundingbox[1:2]) < dbl_click_tolerance & diff(zoombox[3:4]) / sum(boundingbox[3:4]) < dbl_click_tolerance){
      plot(x=x, ...)
      decoration()
      next
    }

    # Fix zoom so that you can zoom single axis
    if(all(zoombox[1:2] < boundingbox[1]))
      zoombox[1:2] = boundingbox[1:2]
    if(all(zoombox[3:4] < boundingbox[3]))
      zoombox[3:4] = boundingbox[3:4]

    plot(x=x, ..., xlim=zoombox[1:2], ylim=zoombox[3:4], yaxs='i', xaxs='i')
    decoration()
  }
}
