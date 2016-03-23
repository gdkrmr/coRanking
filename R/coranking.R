##' Co-Ranking Matrix
##'
##' Calculate the co-ranking matrix to assess the quality of a
##' diminsionality reduction.
##' 
##' Calculate the coranking matrix, to assess the quality of a
##' dimensionality reduction.  \code{Xi} is input in high dimensions,
##' \code{X} is input in low dimensions the type of input is given in
##' \code{input}, if \code{input = 'data'} it will be transformed into
##' a distance matrix with the \code{dist} function, id \code{input ==
##' 'rank'}, \code{Xi} and \code{X} are expected do be rank matrices.
##' @param Xi high dimensional data
##' @param X low dimensional data
##' @param input type of input (see. details)
##' @return a matrix of class \code{'coranking'}
##' @author Guido Kraemer
##' @seealso \code{\link{rankmatrix}}
##' @export
coranking <- function(Xi, X, input = 'data') {
    f <- function(x) order(order(x))
    if(input == 'data') {
        if(dim(Xi)[1] != dim(X)[1])
            stop('number of input rows must be the same')
        dXi <- as.matrix(dist(Xi))
        dX <- as.matrix(dist(X))
        ## because the matrix gets copied, row/columnwise does not
        ## matter!!!
        Ro <- apply(dXi, 2, f)
        R <- apply(dX, 2, f)
    } else if (input == 'dist') {
        Xi <- as.matrix(Xi)
        X <- as.matrix(X)
        if( !all.equal(dim(Xi)[1], dim(Xi)[2], dim(X)[1], dim(X)[2]) )
            stop('input must be the same size and square matrices or of class "dist"')
        if( !isSymmetric(Xi) || !isSymmetric(X) )
            stop('input must be symmetric')
        ## because the matrix gets copied, row/columnwise does not
        ## matter!!!
        Ro <- apply(Xi, 2, f)
        R <- apply(X, 2, f)
    } else if (input == 'rank') {
        if( !all.equal(dim(Xi)[1], dim(Xi)[2], dim(X)[1], dim(X)[2]) )
            stop('input must be the same size and square matrices')
        Ro <- Xi
        R <- X
    } else {
        stop('input must be one of c("data","dist","rank")')
    }

    ## remove neighborhood of size 0 and correct dimnames
    res <- table(Ro, R)[-1, -1]
    dimnames(res) <- list(Ro = 1:nrow(res), R = 1:nrow(res))

    class(res) <- "coranking"
    res
}
