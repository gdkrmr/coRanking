#' Image function for the co-ranking matrix
#'
#' Plots the co-ranking matrix nicely
#'
#' Plots the co-ranking matrix nicely for visual inspection.  uses the
#' \code{image} function internaly, \code{...} is passed down to the image
#' function. The values in the co-ranking matrix are logscaled for
#' better contrast.
#' 
#' @param Q of class \code{coranking}.
#' @param legend if \code{T} plot a legend.
#' @param lwd linewidth in legend
#' @param bty boxtype of legend
#' @param main title of plot
#' @param xlab label of the x axis
#' @param ylab label of the y axis
#' @param col a palette for coloring
#' @param axes ligical draw axes
#' @param ... parameters for the \code{\link[graphics]{image}} function.
#' @author Guido Kramer
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics image.default
#' @export
imageplot <- function(Q, lwd = 2, bty = 'n', main = 'co-ranking matrix',
                      xlab = expression(R), ylab = expression(Ro),
                      col = colorRampPalette(colors = c('gray85', 'red','yellow','green', 'blue'))(100),
                      axes = FALSE, legend = TRUE, ...){
    if(!methods::hasArg(axes)){
         was.missing.axes <- TRUE
         axes = F
    }
    ## if(!hasArg(col)){
    ##     was.missing.col <- T
    ##     col <- colorRampPalette(colors = c('gray85', 'red','yellow','green', 'blue'))(100)
    ##     col <- (col)
    ## }
    ## if(!hasArg(legend)) legend <- TRUE
    ## if(!hasArg(lwd))    lwd <- 2
    ## if(!hasArg(bty))    bty <- 'n'
    ## if(!hasArg(main))   main <- 'co-ranking matrix'
    ## if(!hasArg(xlab))   xlab <- expression(R)
    ## if(!hasArg(ylab))   ylab <- expression(Rho)
    graphics::image.default(log(t(apply(Q,2,rev))),
                  axes = axes, #col = col,
                  ## xlab = xlab, ylab = ylab,
                  main = main,
                  #axes = F,
                  ...)
    if(!axes){
        graphics::axis(2, at = c(0,1), labels = c(nrow(Q), 1))
        graphics::axis(3, at = c(0,1), labels = c(1, nrow(Q)))
    }
    if(legend){
        lgd <- rep('', 100)
        lgd[100] <- 'log(0)'
        lgd[1] <- paste0('log(', max(Q), ')')
        graphics::legend('topright', #1,1,#grconvertX(0, 'device'), grconvertY(1,'device'),
               legend = lgd, #rep('', 100), #c('0', '0.5', '1'),
               lwd = lwd,
               y.intersp = lwd/20,
               col = rev(col),
               bty = bty,
               ...) 
    }
}
