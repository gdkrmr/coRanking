#' The local continuity meta-criterion
#'
#' Calculate the local continuity meta-criterion from a co-ranking matrix.
#'
#' The local continuity meta-criterion (Chen and Buja, 2006) is defined as
#' \deqn{ LCMC = \frac{K}{1-N} +
#'               \frac{1}{NK} \sum_{(k,l)\in U  L_K} q_{kl} }
#' Higher values mean a better performance of the dimensionality reduction.
#'
#' @param Q a co-ranking matrix
#' @param K vector of integers describing neighborhood size
#' @return A number, the local continuity meta-criterion
#' @author Guido Kraemer
#' @export
LCMC <- function(Q, K = 1:nrow(Q)){
    if (!is.matrix(Q))          stop("Q must be a matrix")
    if (dim(Q)[1] != dim(Q)[2]) stop("Q must be square")
    if (min(K) < 1)             stop("min(K) must be >= 1")
    if (max(K) > nrow(Q))       stop("max(K) must be <= nrow(Q)")

    if (!methods::is(Q, "coranking")) warning("Q should be of class coranking")
    if (typeof(Q) != "integer")       warning("Q should be integer")
    if (typeof(K) != "integer")       warning("K should be integer")

    nQ <- nrow(Q)
    nK <- length(K)
    N <- nQ + 1

    ## the first version is much slower if many values have to be
    ## computed, but I have no clue about the criteria to choose the
    ## "right" method.
    ## Maybe putting Q[1:k, 1:k] in the for loop is
    ## faster than the logical indexing
    if (nK < 0.2 * nQ) {
        lcmc <- numeric(nK)
        for (i in 1:nK) {
            k <- K[i]
            lcmc[i] <- k / (1 - N) + sum(Q[cm.UL_K(k, nQ)]) / N / k
        }
    } else {
        lcmc_ <- diag(apply(apply(Q, 2, cumsum), 1, cumsum)) /
            (1:nQ) / N -
            (1:nQ) / nQ
        lcmc <- lcmc_[K]
    }
    lcmc
}
