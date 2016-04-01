#' The local continuity meta-criterion
#'
#' Calculate the local continuity meta-criterion from a co-ranking matrix.
#'
#' The local continuity meta-criterion **todo:cite** is defined as
#' $$ \text{LCMC} = \frac{K}{1-N} +
#' \frac {1}{NK}\sum_{(k,l)\in\mathbb U \mathbb L_K q_{kl} $$
#' Higher values mean a better performance of the dimensionality reduction.
#'
#' @param Q a co-ranking matrix
#' @param K vector of integers describing neighborhood size
#' @return A number, the local continuity meta-criterion
#' @author Guido Kraemer
#' @export
LCMC <- function(Q, K){
    if(!is(Q, 'matrix')) stop("Q must be a matrix")
    if(!isSymmetric(Q)) stop("Q must be symmetric")
    if(min(K) < 1) stop("min(K) must be >= 1")
    if(max(K) > nrow(Q)) stop("max(K) must be <= nrow(Q)")

    if(!is(Q, 'coranking') warn("Q shoulde be of class coranking")
    if(typeof(Q) != 'integer') warn("Q should be integer")
    if(typeof(K) != 'integer') warn("K should be integer")

    nQ <- nrow(Q)
    nK <- length(K)
    N <- nQ + 1
    lcmc <- numeric(nK)
    for(i in 1:nK){
        k <- K[i]
        lcmc[i] <- k / (1-N) + sum(Q[cm.UL_K(k, nQ)]) / N / k
    }
    lcmc
}
## should be the same:
## cm.LCMC <-
## function(Q, K)
##     cm.Q_NX(Q, K) - K / nrow(Q)
