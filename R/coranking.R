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
#' @param input_Xi type of input of Xi (see. details)
#' @param input_X type of input of X (see. details)
#' @param use \code{R} or \code{C} backend
#' @return a matrix of class \code{'coranking'}
#' @author Guido Kraemer
#' @seealso \code{\link{rankmatrix}}
#' @export
coranking <- function(Xi, X,
                      input_Xi = c("data", "dist", "rank"),
                      input_X = input_Xi,
                      use = "C") {

    input_X <- match.arg(input_Xi)
    input_Xi <- match.arg(input_Xi)

    if (input_Xi == "data" || input_Xi == "dist") {
      if (input_Xi == "dist" && !isSymmetric(Xi))
        stop("input must be symmetric")

      rXi <- rankmatrix(Xi, input = input_Xi,  use = use)
    } else {
      rXi <- Xi
    }

    if(input_X == "data" || input_X == "dist") {
      if (input_X == "dist" && !isSymmetric(X))
        stop("input must be symmetric")

      rX <- rankmatrix(X, input = input_X, use = use)
    } else {
      rX <- X
    }

    if (!all.equal(dim(rXi)[1], dim(rXi)[2], dim(rX)[1], dim(rX)[2]))
      stop("input must be the same size and square matrices")
    if (!is.integer(rXi) || !is.integer(rX))
      stop("input must be integer")

    if (use == "C") {
      return(coranking_C(rXi, rX))
    } else {
      return(coranking_R(rXi, rX))
    }
}
