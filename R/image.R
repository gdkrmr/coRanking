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
#' @param ... parameters for the image function.
#' @author Guido Kramer
#' @export
image.coranking <- function(Q, lwd = 2, bty = 'n', main = 'co-ranking matrix',
                            xlab = expression(R), ylab = expression(Ro),
                            col = colorRampPalette(colors = c('gray85', 'red','yellow','green', 'blue'))(100),
                            axes = F, ...){
    require(methods)
    if(!hasArg(axes)){
         was.missing.axes <- T
         axes = F
    }
    ## if(!hasArg(col)){
    ##     was.missing.col <- T
    ##     col <- colorRampPalette(colors = c('gray85', 'red','yellow','green', 'blue'))(100)
    ##     col <- (col)
    ## }
    if(!hasArg(legend)) legend <- T
    ## if(!hasArg(lwd))    lwd <- 2
    ## if(!hasArg(bty))    bty <- 'n'
    ## if(!hasArg(main))   main <- 'co-ranking matrix'
    ## if(!hasArg(xlab))   xlab <- expression(R)
    ## if(!hasArg(ylab))   ylab <- expression(Rho)
    image.default(log(t(apply(Q,2,rev))),
                  axes = axes, #col = col,
                  ## xlab = xlab, ylab = ylab,
                  main = main,
                  #axes = F,
                  ...)
    if(!axes){
        axis(2, at = c(0,1), labels = c(nrow(Q), 1))
        axis(3, at = c(0,1), labels = c(1, nrow(Q)))
    }
    if(legend){
        lgd <- rep('', 100)
        lgd[100] <- 'log(0)'
        lgd[1] <- paste0('log(', max(Q), ')')
        legend('topright', #1,1,#grconvertX(0, 'device'), grconvertY(1,'device'),
               legend = lgd, #rep('', 100), #c('0', '0.5', '1'),
               lwd = lwd,
               y.intersp = lwd/20,
               col = rev(col),
               bty = bty,
               ...)
    }
}
