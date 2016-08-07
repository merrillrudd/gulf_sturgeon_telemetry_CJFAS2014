\name{AgeToLengthComp}
\alias{AgeToLengthComp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
AgeToLengthComp(L_a, CVlen, highs, lows, tyears, N_at, S_a, comp_sample)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{L_a}{
%%     ~~Describe \code{L_a} here~~
}
  \item{CVlen}{
%%     ~~Describe \code{CVlen} here~~
}
  \item{highs}{
%%     ~~Describe \code{highs} here~~
}
  \item{lows}{
%%     ~~Describe \code{lows} here~~
}
  \item{tyears}{
%%     ~~Describe \code{tyears} here~~
}
  \item{N_at}{
%%     ~~Describe \code{N_at} here~~
}
  \item{S_a}{
%%     ~~Describe \code{S_a} here~~
}
  \item{comp_sample}{
%%     ~~Describe \code{comp_sample} here~~
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
function (L_a, CVlen, highs, lows, tyears, N_at, S_a, comp_sample) 
{
    lbprobs <- function(mnl, sdl) return(pnorm(highs, mnl, sdl) - 
        pnorm(lows, mnl, sdl))
    vlprobs <- Vectorize(lbprobs, vectorize.args = c("mnl", "sdl"))
    plba <- t(vlprobs(L_a, L_a * CVlen))
    plba <- plba/rowSums(plba)
    page <- matrix(ncol = dim(plba)[1], nrow = tyears)
    for (y in 1:tyears) page[y, ] <- N_at[, y] * S_a
    page <- page/rowSums(page)
    plb <- matrix(ncol = length(highs), nrow = tyears)
    for (y in 1:tyears) plb[y, ] <- page[y, ] \%*\% plba
    plb <- plb/rowSums(plb)
    LF <- array(0, dim = dim(plb))
    rownames(LF) <- 1:tyears
    for (y in 1:tyears) {
        LF[y, ] <- rmultinom(n = 1, size = comp_sample[y], prob = plb[y, 
            ])
    }
    Outs <- NULL
    Outs$plba <- plba
    Outs$plb <- plb
    Outs$page <- page
    Outs$LF <- LF
    return(Outs)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
