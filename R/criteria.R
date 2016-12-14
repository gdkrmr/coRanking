## trustworthiness
cm.G_K <- function(K, N)
    if (K < (N / 2)) N * K * (2 * N - 3 * K - 1) else N * (N - K) * (N - K - 1)

cm.M_T <- function(Q, K){
    N <- nrow(Q) + 1
    vals <- (row(Q) - K) * Q
    inds <- cm.LL_K(K, nrow(Q))
    1 - 2 / cm.G_K(K, N) * sum(vals[inds])
}

## continuity
cm.M_C <- function(Q, K) {
    N <- nrow(Q) + 1
    vals <- (col(Q) - K) * Q
    inds <- cm.UR_K(K, nrow(Q))
    1 - 2 / cm.G_K(K, N) * sum(vals[inds])
}

## mean relative rank errors
cm.H_K <- function(K, N)
    N * sum( abs(N - 2 * (1:K)) / (1:K) )

cm.W_n <- function(Q, K){
    N <- nrow(Q) + 1
    vals <- abs(row(Q) - col(Q)) / col(Q) * Q
    inds <- cm.UL_K(K, nrow(Q)) | cm.LL_K(K, nrow(Q))
    sum(vals[inds]) / cm.H_K(K, N)
}
cm.W_nu <- function(Q, K){
    N <- nrow(Q) + 1
    vals <- abs(row(Q) - col(Q)) / col(Q) * Q
    inds <- cm.UL_K(K, nrow(Q)) | cm.UR_K(K, nrow(Q))
    sum(vals[inds]) / cm.H_K(K, N)
}

## weighted mean relative rank errors
cm.C_K <- function(K, N, w, v)
    N * sum( pmax(0, N - 2 * (1:K)) ^ w / (1:K) ^ v )

cm.W_N_vw <- function(Q, K, w, v) {
    N <- nrow(Q) + 1
    vals <- (row(Q) - col(Q)) ^ v / col(Q) ^ w * Q
    inds <- cm.LT_K(K, N) | cm.LL_K(K, N)
    sum(vals[inds]) / cm.C_K(K, N, w, v)
}
cm.W_X_vw <- function(Q, K, w, v){
    N <-  nrow(Q) + 1
    vals <- (col(Q) - row(Q)) ^ v / row(Q) ^ w * Q
    inds <- cm.UT_K(K, nrow(Q)) | cm.UR_K(K, nrow(Q))
    sum(vals[inds]) / cm.C_K(K, N, w, v)
}




## fraction mild K-intrusions
cm.U_N <- function(Q, K)
    sum(Q[cm.UT_K(K, nrow(Q))]) / K / (nrow(Q) + 1)
## fraction mild K-extrusions
cm.U_X <- function(Q, K)
    sum(Q[cm.LT_K(K, nrow(Q))]) / K / (nrow(Q) + 1)
## fraction same rank
cm.U_P <- function(Q, K)
    sum(Q[cm.D_K(K, nrow(Q))]) / K / (nrow(Q) + 1)

cm.LCMC <- cm.U_LC <- LCMC

## overall quality of embedding: Q_NX <- U_P + U_N + U_X
cm.Q_NX <- function(Q, K)
    sum(Q[cm.UL_K(K, nrow(Q))]) / K / (nrow(Q) + 1)
cm.Q_TC <- function(Q, K)
    cm.M_T(Q, K) + cm.M_C(Q, K)
cm.Q_nnu <- function(Q, K)
    2 - cm.W_n(Q, K) - cm.W_nu(Q, K)
cm.Q_wNX_vw <- function(Q, K, v, w)
    2 - cm.W_N_vw(Q, K, v, w) - cm.W_X_vw(Q, K, v, w)

## behavior, intrusive: B_NX > 0, extrusive B_NX < 0
cm.B_NX <- function(Q, K)
    cm.U_N(Q, K) - cm.U_X(Q, K)
cm.B_TC <- function(Q, K)
    cm.M_T(Q, K) - cm.M_C(Q, K)
cm.B_nnu <- function(Q, K)
    cm.W_n(Q, K) - cm.W_nu(Q, K)
cm.B_wNX_vw <- function(Q, K, v, w)
    cm.W_N_vw(Q, K, v, w) - cm.W_X_vw(Q, K, v, w)

## local and global quality
cm.K_max <- function(Q){
    qual <- numeric(nrow(Q))
    for (i in 1:nrow(Q)){
        cat("\r", i, "/", nrow(Q), sep = "")
        qual[i] <- cm.LCMC(Q, i)
    }
    K_max <- which(max(qual) == qual)
    list(K_max    = K_max,
         LCMC_K   = qual,
         Q_local  = mean(qual[1:K_max]),
         Q_global = mean(qual[(K_max + 1):length(qual)]))
}

## 'improved' quality measures
cm.Qtilde_NX <- function(Ro, R, kappa_s, kappa_t){
    N <-  nrow(R) + 1
    w_s <- !( (Ro > kappa_s) & (R > kappa_s))
    w_t <- abs(Ro - R) <= kappa_t
    sum(w_s * w_t) / kappa_s / N
}
## per point quality:
cm.Qtilde_i <- function(Ro, R, kappa_s, kappa_t){
    N <- nrow(R) + 1
    w_s <- !((Ro > kappa_s) & (R > kappa_s))
    w_t <- abs(Ro - R) <= kappa_t
    w_st <- w_s * w_t
    vals <- w_st + t(w_st)
    apply(vals, 2, sum) / 2 / kappa_s / N
}
