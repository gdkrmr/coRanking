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
image.coranking <- function(Q, legend = T,  ...){
    if(missing(axes)){
        was.missing.axes <- T
        axes = F
    }
    if(missing(col)){
        was.missing.col <- T
        col <- colorRampPalette(colors = c('red','yellow','green', 'blue'))(100)
    }
    image(log(t(apply(Q,2,rev)), axes, col, ...))
    if(was.missing.axes){
        axis(2, at = c(0,1), labels = c(nrow(Q, 1)))
        axis(3, at = c(0,1), labels = c(1, nrow(Q)))
    }
    if(legend){
        legend(grconvertX(0.5, 'device'), grconvertY(1,'device'),
               c('0', '0.5', '1'), fill = col)
    }
}
