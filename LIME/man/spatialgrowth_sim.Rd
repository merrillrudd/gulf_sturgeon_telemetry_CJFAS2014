\name{spatialgrowth_sim}
\alias{spatialgrowth_sim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
spatialgrowth_sim(n_i, Scale = 2, Sigma2 = 1, SD_spatial = 0.1, linf, beta_y = 0.02)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n_i}{
%%     ~~Describe \code{n_i} here~~
}
  \item{Scale}{
%%     ~~Describe \code{Scale} here~~
}
  \item{Sigma2}{
%%     ~~Describe \code{Sigma2} here~~
}
  \item{SD_spatial}{
%%     ~~Describe \code{SD_spatial} here~~
}
  \item{linf}{
%%     ~~Describe \code{linf} here~~
}
  \item{beta_y}{
%%     ~~Describe \code{beta_y} here~~
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
function (n_i, Scale = 2, Sigma2 = 1, SD_spatial = 0.1, linf, 
    beta_y = 0.02) 
{
    require(RandomFields)
    lat_min <- -4
    lat_max <- -1
    y_i <- runif(n = n_i, min = lat_min, max = lat_max)
    RMmodel <- RMgauss(var = SD_spatial^2, scale = Scale)
    linf_i1 <- linf * exp(RFsimulate(model = RMmodel, x = rep(0, 
        n_i), y = y_i)@data[, 1] - Sigma2/2) * exp(beta_y * (y_i - 
        mean(c(lat_min, lat_max))))
    linf_i <- linf_i1 + (linf - mean(linf_i1))
    df <- data.frame(linf_i = linf_i, y_i = y_i)
    return(df)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
