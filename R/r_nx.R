
rnx2qnx <- function(rnx, K = seq_along(rnx), N = length(rnx) + 1) {
  (rnx * (N - 1 - K) + K) / (N - 1)
}

qnx2rnx <- function(qnx, K = seq_along(qnx), N = length(qnx) + 1) {
  rnx <- ((N - 1) * qnx - K) / (N - 1 - K)
  rnx[-length(rnx)]
}

#' The \eqn{Q_{NX}(K)} criterion
#'
#' A curve indicating the percentage of points that are mild in- and extrusions
#' or keep their rank.
#'
#' \deqn{ Q_{NX}(K) = \frac{1}{KN} \sum_{k=1}^{K}\sum_{l=1}^{K}Q_{kl} }
#'
#' @param Q a co-ranking matrix
#' @return A vector with the values for Q_NX(K)
#' @author Guido Kraemer
#' @references
#' Lueks, W., Mokbel, B., Biehl, M., & Hammer, B. (2011). How to
#'   Evaluate Dimensionality Reduction? - Improving the Co-ranking Matrix.
#'   ArXiv:1110.3917 [Cs]. http://arxiv.org/abs/1110.3917
#' @export
Q_NX <- function(Q) {
  nQ <- nrow(Q)
  N <- nQ + 1

  diag(apply(apply(Q, 2, cumsum), 1, cumsum)) / seq_len(nQ) / N
}

#' The \eqn{R_{NX}(K)} criterion
#'
#' A curve indicating the improvement of the embedding over a random embedding
#' for the neighborhood size \eqn{K}. Values range from 0, for a random
#' embedding, to 1 for a perfect embedding.
#'
#' \eqn{R_{NX}(K)} is calculated as follows:
#' \deqn{ Q_{NX}(K) = \sum_{1\leq k\leq K}\sum_{1\leq l\leq K} \frac{q_{kl}}{KN} }
#' Counts the upper left \eqn{K}-by-\eqn{K} block of \eqn{Q}, i.e. it considers the preserved
#' ranks on the diagonal and the permutations within a neighborhood.
#'
#' \deqn{ R_{NX}(K) = \frac{(N-1)Q_{NX}(K)-K}{N-1-K} } A resulting vale of 0
#' corresponds to a random embedding, a value of 1 to a perfect embedding of the
#' \eqn{K}-ary neighborhood.
#'
#' @param Q a co-ranking matrix
#' @return A vector with the values for R_NX(K)
#' @author Guido Kraemer
#' @references
#' Lee, J.A., Lee, J.A., Verleysen, M., 2009. Quality assessment of
#'     dimensionality reduction: Rank-based criteria. Neurocomputing 72.
#'
#' Lee, J. A., Peluffo-Ordóñez, D. H., & Verleysen, M., 2015. Multi-scale
#'   similarities in stochastic neighbour embedding: Reducing dimensionality
#'   while preserving both local and global structure. Neurocomputing, 169,
#'   246–261. https://doi.org/10.1016/j.neucom.2014.12.095
#' @export
R_NX <- function(Q) {
  qnx2rnx(Q_NX(Q))
}


#' Area under the \code{RN_X} curve
#'
#' Area under the \eqn{R_{NX}(K)} curve when \eqn{K} is put on a logarithmic scale.
#'
#' It is calculated as:
#' \deqn{ AUC_{\ln K}(R_{NX}(K)) = \left(\sum_{K=1}^{N-2}R_{NX}(K)/K\right)/\left(\sum_{K=1}^{N-2}1/K\right) }
#'
#' @param R_NX The R_NX curve, a vector of values
#' @return A value, the area under the curve.
#' @author Guido Kraemer
#' @references
#' Lee, J. A., Peluffo-Ordóñez, D. H., & Verleysen, M., 2015. Multi-scale
#'   similarities in stochastic neighbour embedding: Reducing dimensionality
#'   while preserving both local and global structure. Neurocomputing, 169,
#'   246–261. https://doi.org/10.1016/j.neucom.2014.12.095
#' @export
AUC_ln_K <- function(R_NX) {
  K <- seq_along(R_NX)
  return(sum(R_NX / K) / sum(1 / K))
}


#' Plot the \eqn{R_{NX}(K)} curve
#'
#' Produces a plot with the \eqn{R_{NX}(K)} curves from the arguments
#'
#' @param R_NXs A list of R_NX curves, names from the list will appear in the
#'   legend
#' @param pal a vector of colors
#' @param ylim set the y-axis limits of the plot
#' @param ... options for the plotting function
#' @return Nothing, produces a plot.
#' @author Guido Kraemer
#' @references
#' Lee, J. A., Peluffo-Ordóñez, D. H., & Verleysen, M., 2015. Multi-scale
#'   similarities in stochastic neighbour embedding: Reducing dimensionality
#'   while preserving both local and global structure. Neurocomputing, 169,
#'   246–261. https://doi.org/10.1016/j.neucom.2014.12.095
#' @export
plot_R_NX <- function(R_NXs, pal = grDevices::palette(), ylim = c(0, 0.9), ...) {

  if (length(R_NXs) > 1) {
    l  <- length(R_NXs[[1]])
    for (i in 2:length(R_NXs)) {
      if (l != length(R_NXs[[i]]))
        stop("All R_NX values have to be the same length.")
    }
  }
  if (!all(sapply(R_NXs, function(x) { is.null(dim(x)) })))
    stop("R_NX values should not have a dimension.")

  auc_list <- lapply(R_NXs, AUC_ln_K)

  df <- data.frame(R_NXs, check.names = FALSE)

  N <- nrow(df)
  df$K <- seq_len(N)

  graphics::plot(x = 1, y = 1,
                 xlim = c(1, N), ylim = ylim,
                 log = "x",
                 type = "n", bty = "n",
                 ylab = expression(R[NX](K)), xlab = "K",
                 xaxt = "n", las = 1,
                 yaxs = "i", xaxs = "i", ...)

  graphics::axis(1, at = 10^(0:floor(log10(N))),
                 labels = sapply(0:floor(log10(N)),
                                 function(i) parse(text = paste0("10^", i))))

  for (i in seq(0.1, 0.9, by = 0.1)) {
    graphics::abline(h = i, lty = 3, lwd = 0.5)
    graphics::lines(qnx2rnx(i, 1:N, N), lty = 3, lwd = 0.5)
  }

  for (i in seq_along(names(R_NXs))) {
    n <- names(R_NXs)[i]
    graphics::lines(x = df$K, y = df[[n]], col = pal[i])
  }

  graphics::legend("topright",
                   col = pal[seq_along(R_NXs)],
                   legend = paste(sprintf("%0.2f", auc_list), names(R_NXs)),
                   lty = 1, bg = "white")
}
