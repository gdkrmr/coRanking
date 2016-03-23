\name{coranking}
\alias{coranking.default}
\alias{coranking}
\alias{co-ranking}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Function to calculate the co-ranking matrix to compare methods of
  dimensionality reduction
%%  ~~function to do ... ~~
}
\description{
  
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
coranking.default(Xi, X, input = "data")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Xi}{
%%     ~~Describe \code{Xi} here~~
}
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{input}{
%%     ~~Describe \code{input} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (Xi, X, input = "data") 
{
    f <- function(x) order(order(x))
    if (input == "data") {
        if (dim(Xi)[1] != dim(X)[1]) 
            stop("number of input rows must be the same")
        dXi <- as.matrix(dist(Xi))
        dX <- as.matrix(dist(X))
        Ro <- apply(dXi, 2, f)
        R <- apply(dX, 2, f)
    }
    else if (input == "dist") {
        if (!all.equal(dim(Xi)[1], dim(Xi)[2], dim(X)[1], dim(X)[2])) 
            stop("input must be the same size and square matrices")
        Ro <- apply(Xi, 2, f)
        R <- apply(X, 2, f)
    }
    else if (input == "rank") {
        if (!all.equal(dim(Xi)[1], dim(Xi)[2], dim(X)[1], dim(X)[2])) 
            stop("input must be the same size and square matrices")
        Ro <- Xi
        R <- X
    }
    else {
        stop("input must be one of c(\"data\",\"dist\",\"rank\")")
    }
    res <- table(Ro, R)[-1, -1]
    dimnames(res) <- list(Ro = 1:nrow(res), R = 1:nrow(res))
    class(res) <- "coranking"
    res
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
