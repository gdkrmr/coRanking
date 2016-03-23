## upper left part
cm.UL_K <- function(K, n) {
    tmp <- matrix(F, n, n)
    tmp[1:K,1:K] <- T
    tmp
}

## uper right part
cm.UR_K <- function(K,n) {
    tmp <- matrix(F, n, n)
    tmp[1:K, (K+1):n] <- T
    tmp
}

## lower left part
cm.LL_K <- function(K,n) {
    tmp <- matrix(F, n, n)
    tmp[(K+1):n, 1:K] <- T
    tmp
}

## lower right part
cm.LR_K <- function(K,n) {
    tmp <- matrix(F, n, n)
    tmp[(K+1):n, (K+1):n] <- T
    tmp
}

## diagonal until K
cm.D_K <- function(K,n) {
    tmp <- matrix(F, n, n)
    tmp[matrix(1:K,K,2)] <- T
    tmp
}

## lower triangle of upper left part
cm.LT_K <- function(K,n) {
    tmp <- lower.tri(diag(n))
    tmp[row(tmp) > K] <- F
    tmp
}

## upper triangle of upper left part
cm.UT_K <- function(K,n) {
    tmp <- upper.tri(diag(n))
    tmp[col(tmp) > K] <- F
    tmp
}

