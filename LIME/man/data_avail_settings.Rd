\name{data_avail_settings}
\alias{data_avail_settings}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
data_avail_settings(avail_set, ESS, simulation = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{avail_set}{
%%     ~~Describe \code{avail_set} here~~
}
  \item{ESS}{
%%     ~~Describe \code{ESS} here~~
}
  \item{simulation}{
%%     ~~Describe \code{simulation} here~~
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
function (avail_set, ESS, simulation = TRUE) 
{
    if (simulation == TRUE) {
        settings <- list()
        Nyears <- 20
        if ("Rich_LC" \%in\% avail_set) {
            settings$Rich_LC <- list(Nyears = Nyears, comp_sample = 1000, 
                obs_per_yr = rep(ESS, Nyears), Nyears_comp = Nyears, 
                alt_yrs = FALSE, sample = FALSE)
        }
        if ("Moderate_LC" \%in\% avail_set) {
            settings$Moderate_LC <- list(Nyears = Nyears, comp_sample = 100, 
                obs_per_yr = rep(ESS, Nyears), Nyears_comp = Nyears, 
                alt_yrs = FALSE, sample = FALSE)
        }
        if ("Sample_LC" \%in\% avail_set) {
            settings$Sample_LC <- list(Nyears = Nyears, comp_sample = 1000, 
                obs_per_yr = rep(ESS, Nyears), Nyears_comp = Nyears, 
                alt_yrs = TRUE, sample = 0.2)
        }
        if ("Index_LC1" \%in\% avail_set) {
            settings$Index_LC1 <- list(Nyears = Nyears, comp_sample = 1000, 
                obs_per_yr = rep(ESS, Nyears), Nyears_comp = 1, 
                alt_yrs = FALSE, sample = FALSE)
        }
        if ("Catch_LC1" \%in\% avail_set) {
            settings$Catch_LC1 <- list(Nyears = Nyears, comp_sample = 1000, 
                obs_per_yr = rep(ESS, Nyears), Nyears_comp = 1, 
                alt_yrs = FALSE, sample = FALSE)
        }
        if ("Index_LC10" \%in\% avail_set) {
            settings$Index_LC10 <- list(Nyears = Nyears, comp_sample = 1000, 
                obs_per_yr = rep(ESS, Nyears), Nyears_comp = 10, 
                alt_yrs = FALSE, sample = FALSE)
        }
        if ("Catch_LC10" \%in\% avail_set) {
            settings$Catch_LC10 <- list(Nyears = Nyears, comp_sample = 1000, 
                obs_per_yr = rep(ESS, Nyears), Nyears_comp = 10, 
                alt_yrs = FALSE, sample = FALSE)
        }
        if ("LC1" \%in\% avail_set) {
            settings$LC1 <- list(Nyears = Nyears, comp_sample = 1000, 
                obs_per_yr = rep(ESS, Nyears), Nyears_comp = 1, 
                alt_yrs = FALSE, sample = FALSE)
        }
        if ("LC2" \%in\% avail_set) {
            settings$LC2 <- list(Nyears = Nyears, comp_sample = 1000, 
                obs_per_yr = rep(ESS, Nyears), Nyears_comp = 2, 
                alt_yrs = FALSE, sample = FALSE)
        }
        if ("LC5" \%in\% avail_set) {
            settings$LC5 <- list(Nyears = Nyears, comp_sample = 1000, 
                obs_per_yr = rep(ESS, Nyears), Nyears_comp = 5, 
                alt_yrs = FALSE, sample = FALSE)
        }
        if ("LC10" \%in\% avail_set) {
            settings$LC10 <- list(Nyears = Nyears, comp_sample = 1000, 
                obs_per_yr = rep(ESS, Nyears), Nyears_comp = 10, 
                alt_yrs = FALSE, sample = FALSE)
        }
        if ("LC20" \%in\% avail_set) {
            settings$LC20 <- list(Nyears = Nyears, comp_sample = 1000, 
                obs_per_yr = rep(ESS, Nyears), Nyears_comp = 20, 
                alt_yrs = FALSE, sample = FALSE)
        }
    }
    return(settings)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
