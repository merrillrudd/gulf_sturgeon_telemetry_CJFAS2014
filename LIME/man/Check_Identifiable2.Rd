\name{Check_Identifiable2}
\alias{Check_Identifiable2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Check_Identifiable2(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
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
function (obj) 
{
    ParHat = TMBhelper:::extract_fixed(obj)
    List = NULL
    List[["Hess"]] = optimHess(par = ParHat, fn = obj$fn, gr = obj$gr)
    List[["Eigen"]] = eigen(List[["Hess"]])
    List[["WhichBad"]] = which(List[["Eigen"]]$values < sqrt(.Machine$double.eps))
    RowMax = tryCatch(apply(List[["Eigen"]]$vectors[, List[["WhichBad"]]], 
        MARGIN = 1, FUN = function(vec) {
            max(abs(vec))
        }), error = function(e) NA)
    if (all(is.na(RowMax))) 
        RowMax <- sapply(1:length(List[["Eigen"]]$vectors[, List[["WhichBad"]]]), 
            function(x) max(abs(vec)))
    List[["BadParams"]] = data.frame(Param = names(obj$par), 
        MLE = ParHat, ifelse(RowMax > 0.1, "Bad", "OK"))
    if (length(List[["WhichBad"]]) == 0) {
        message("All parameters are identifiable")
    }
    else {
        print(List[["BadParams"]])
    }
    return(invisible(List))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
