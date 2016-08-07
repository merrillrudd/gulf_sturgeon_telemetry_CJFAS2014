\name{calc_ref}
\alias{calc_ref}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calc_ref(Mat_a, W_a, M, S_a, F, ref = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Mat_a}{
%%     ~~Describe \code{Mat_a} here~~
}
  \item{W_a}{
%%     ~~Describe \code{W_a} here~~
}
  \item{M}{
%%     ~~Describe \code{M} here~~
}
  \item{S_a}{
%%     ~~Describe \code{S_a} here~~
}
  \item{F}{
%%     ~~Describe \code{F} here~~
}
  \item{ref}{
%%     ~~Describe \code{ref} here~~
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
function (Mat_a, W_a, M, S_a, F, ref = FALSE) 
{
    Na0 <- Naf <- rep(NA, length(W_a))
    Na0[1] <- Naf[1] <- 1
    for (a in 2:length(W_a)) {
        if (a < length(W_a)) {
            Na0[a] <- Na0[a - 1] * exp(-M)
            Naf[a] <- Naf[a - 1] * exp(-M - S_a[a - 1] * F)
        }
        if (a == length(W_a)) {
            Na0[a] <- (Na0[a - 1] * exp(-M))/(1 - exp(-M))
            Naf[a] <- (Naf[a - 1] * exp(-M - F * S_a[a - 1]))/(1 - 
                exp(-M - F * S_a[a - 1]))
        }
    }
    SB0 <- sum(Na0[-1] * Mat_a[-1] * W_a[-1])
    SBf <- sum(Naf[-1] * Mat_a[-1] * W_a[-1])
    SPR <- SBf/SB0
    if (ref == FALSE) 
        return(SPR)
    if (ref != FALSE) {
        diff <- ref - SPR
        return(diff)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
