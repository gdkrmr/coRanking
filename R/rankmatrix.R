##' Rank matrix
##'
##' Replaces the elements of \eqn{X} with their rank in the column vector of
##' the distance matrix
##'
##' Each column vector in the distance matrix (or the distance matrix
##' computed from the input) is replaced by a vector indicating the
##' rank of the distance inside that vector.
##'
##' This is a computation step necessary for the co-ranking matrix and
##' provided mainly so that the user has the possibility to save
##' computation time.
##' 
##' @param X data, dist object, or distance matrix
##' @param input type of input
##' @param use if 'C' uses the compiled library, else uses the native
##'     R code
##' @return returns a matrix of class \code{'rankmatrix'}
##' @author Guido Kraemer
##' @export
rankmatrix <- function(X, input = c('data', 'dist'), use = 'C'){
    input <- match.arg(input)
    if(input == 'data'){
        dX <- euclidean(as.matrix(X), use)
        return(rankmatrix(dX, input = 'dist', use))
    } else if(input == 'dist') {
        dX <- as.matrix(X)
        nr <- dim(dX)[1]
        nc <- dim(dX)[2]
        if(nr != nc)
            stop('distance matrices must be square')
        if( !isSymmetric(dX) )
            stop('distance matrices must be symmetric')
        if( !all(diag(dX) == 0) ) 
            stop('diagonal of distance matrix must be 0')
        if( any(dX[-seq(1, nr^2, nr+1)] == 0) )
            warning('0 outside of diagonal in distance matrix')

        if(use == 'C'){
            return(rankmatrix_C(dX))
        } else {
            return(rankmatrix_R(dX))
        }
    }
    stop("input must be 'data' or 'dist'")
}
