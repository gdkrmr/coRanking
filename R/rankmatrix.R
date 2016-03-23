##' Rank matrix
##'
##' Calculates the rank matrix from X
##'
##' TODO: explain
##' @param X data input
##' @param input type of input
##' @return returns a matrix of class \code{'rankmatrix'}
##' @author Guido Kraemer
##' @export
rankmatrix <- function(X, input = 'data'){
    if(input == 'data'){
        dX <- as.matrix(dist(X))
    } else if(input == 'dist') {
        dX <- as.matrix(X)
    } else {
        stop("input must be of type 'data' or 'dist'")
    }
    res <- apply(dX, 2, function(x) order(order(x)))
    class(res) <- 'rankmatrix'
    res
}
