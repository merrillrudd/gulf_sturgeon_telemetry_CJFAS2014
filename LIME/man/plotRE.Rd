\name{plotRE}
\alias{plotRE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotRE(modpath_vec, itervec, modnames = NULL, xaxt = "n", yaxt = "n", value, ylim = c(-1, 1.5), col.plot = "steelblue", col.line = "goldenrod", yr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{modpath_vec}{
%%     ~~Describe \code{modpath_vec} here~~
}
  \item{itervec}{
%%     ~~Describe \code{itervec} here~~
}
  \item{modnames}{
%%     ~~Describe \code{modnames} here~~
}
  \item{xaxt}{
%%     ~~Describe \code{xaxt} here~~
}
  \item{yaxt}{
%%     ~~Describe \code{yaxt} here~~
}
  \item{value}{
%%     ~~Describe \code{value} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{col.plot}{
%%     ~~Describe \code{col.plot} here~~
}
  \item{col.line}{
%%     ~~Describe \code{col.line} here~~
}
  \item{yr}{
%%     ~~Describe \code{yr} here~~
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
function (modpath_vec, itervec, modnames = NULL, xaxt = "n", 
    yaxt = "n", value, ylim = c(-1, 1.5), col.plot = "steelblue", 
    col.line = "goldenrod", yr) 
{
    RE <- t(sapply(itervec, function(x) calcRE(modpath_vec = modpath_vec, 
        iter = x, value = value, yr = yr)))
    colnames(RE) <- modnames
    boxplot(RE, ylim = ylim, xaxt = xaxt, yaxt = yaxt, col = col.plot)
    abline(h = 0, col = col.line, lwd = 2)
    return(RE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
