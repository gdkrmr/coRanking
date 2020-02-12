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
imageplot <- function(Q, lwd = 2, bty = "n", main = "co-ranking matrix",
                      xlab = expression(R), ylab = expression(Ro),
                      col = colorRampPalette(colors = c("gray85", "red",
                                                        "yellow", "green",
                                                        "blue"))(100),
                      axes = FALSE, legend = TRUE, ...) {
    graphics::image.default(log(t(apply(Q, 2, rev))),
                            axes = axes,
                            main = main,
                            ...)
    if (!axes) {
        graphics::axis(2, at = c(0, 1), labels = c(nrow(Q), 1))
        graphics::axis(3, at = c(0, 1), labels = c(1, nrow(Q)))
    }
    if (legend) {
        lgd <- rep("", 100)
        lgd[100] <- "log(0)"
        lgd[1] <- paste0("log(", max(Q), ")")
        graphics::legend("topright",
               legend = lgd,
               lwd = lwd,
               y.intersp = lwd / 20,
               col = rev(col),
               bty = bty)
    }
}
