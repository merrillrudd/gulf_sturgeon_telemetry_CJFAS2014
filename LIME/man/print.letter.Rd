\name{print.letter}
\alias{print.letter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
print.letter(label = "(a)", xy = c(0.1, 0.925), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{label}{
%%     ~~Describe \code{label} here~~
}
  \item{xy}{
%%     ~~Describe \code{xy} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (label = "(a)", xy = c(0.1, 0.925), ...) 
{
    tmp <- par("usr")
    text.x <- tmp[1] + xy[1] * diff(tmp[1:2])
    text.y <- tmp[3] + xy[2] * diff(tmp[3:4])
    text(x = text.x, y = text.y, labels = label, ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
