##' @useDynLib coRanking
## there is really no need to export this one
euclidean <- function(X, use = 'C'){
    if(use == 'C') {
        euclidean_C(X)
    } else {
        euclidean_R(X)
    }
}

euclidean_R <- function(X) {
    as.matrix(dist(X))
}

euclidean_C <- function(X) {
    .Call('euclidean', X)
}

rankmatrix_R <- function(X){
    res <- apply(X, 2, function(x) order(order(x)))
    class(res) <- 'rankmatrix'
    res
}

rankmatrix_C <- function(X) {
    res <- .Call('rankmatrix', X)
    class(res) <- 'rankmatrix'
    res
}

coranking_R <- function(Xi, X) {
    ## remove neighborhood of size 0 and correct dimnames
    res <- table(Ro, R)[-1, -1]
    dimnames(res) <- list(Ro = 1:nrow(res), R = 1:nrow(res))
    class(res) <- "coranking"
    res
}

coranking_C <- function(Xi, X) {
    res <- .Call('coranking', Xi, X)
    dimnames(res) <- list(Ro = 1:nrow(res), R = 1:nrow(res))
    class(res) <- "coranking"
    res
}
