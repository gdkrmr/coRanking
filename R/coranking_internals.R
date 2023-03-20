## there is really no need to export this one
euclidean <- function(X, use = "C"){
    X <- as.matrix(X)
    if (!is.double(X)) stop("X must be of type double")
    if (use == "C") {
        euclidean_C(X)
    } else {
        euclidean_R(X)
    }
}

euclidean_R <- function(X) {
    res <- as.matrix(stats::dist(X))
    dimnames(res) <- NULL
    res
}

##' @useDynLib coRanking C_euclidean
euclidean_C <- function(X) {
    .Call(C_euclidean, as.matrix(X))
}

rankmatrix_R <- function(X){
    f <- function(x) rank(x, ties.method = "first", na.last = FALSE)
    diag(X) <- NA
    res <- apply(X, 1, f)
    res <- res - 1 # in c sorting starts with 0
    dimnames(res) <- NULL
    storage.mode(res) <- "integer"
    class(res) <- "rankmatrix"
    res
}

##' @useDynLib coRanking C_rankmatrix
rankmatrix_C <- function(X) {
    .Call(C_rankmatrix, X)
}

coranking_R <- function(Ro, R) {
    ## remove neighborhood of size 0 and correct dimnames
    res <- table(Ro, R)[-1, -1]
    dimnames(res) <- list(Ro = seq_len(nrow(res)), R = seq_len(nrow(res)))
    class(res) <- "coranking"
    res
}

##' @useDynLib coRanking C_coranking
coranking_C <- function(Ro, R) {
    .Call(C_coranking, Ro, R)
}
