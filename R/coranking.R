#' Co-Ranking Matrix
#'
#' Calculate the co-ranking matrix to assess the quality of a
#' diminsionality reduction.
#' 
#' Calculate the coranking matrix, to assess the quality of a
#' dimensionality reduction.  \code{Xi} is input in high dimensions,
#' \code{X} is input in low dimensions the type of input is given in
#' \code{input}, if \code{input = 'data'} it will be transformed into
#' a distance matrix with the \code{dist} function, id \code{input ==
#' 'rank'}, \code{Xi} and \code{X} are expected do be rank matrices.
#' 
#' @param Xi high dimensional data
#' @param X low dimensional data
#' @param input type of input (see. details)
#' @param use \code{R} or \code{C} backend
#' @return a matrix of class \code{'coranking'}
#' @author Guido Kraemer
#' @seealso \code{\link{rankmatrix}}
#' @export
coranking <- function(Xi, X, input = c("data", "dist", "rank"), use = "C"){
    input <- match.arg(input)
    if (input == "data"){
        if (dim(Xi)[1] != dim(X)[1])
            stop("number of input rows must be the same")

        dXi <- euclidean(Xi, use)
        dX  <- euclidean(X, use)
        return(coranking(dXi, dX, input = "dist", use))
    } else if (input == "dist") {
        if ( !all.equal(dim(Xi)[1], dim(Xi)[2], dim(X)[1], dim(X)[2]) )
            stop("input must be the same size and square matrices ",
                 'or of class "dist"')
        if ( !isSymmetric(Xi) || !isSymmetric(X) )
            stop("input must be symmetric")

        Ro <- rankmatrix(Xi, input = "dist", use)
        R  <- rankmatrix(X,  input = "dist", use)
        return(coranking(Ro, R, input = "rank", use))
    } else if (input == "rank") {
        if ( !all.equal(dim(Xi)[1], dim(Xi)[2], dim(X)[1], dim(X)[2]) )
            stop("input must be the same size and square matrices")
        if ( !is.integer(Xi) || !is.integer(X))
            stop("input must be integer")

        if (use == "C"){
            return(coranking_C(Xi, X))
        } else {
            return(coranking_R(Xi, X))
        }
    }
    stop('input must be one of c("data", "dist", "rank")')
}
