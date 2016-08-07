\name{model_paths}
\alias{model_paths}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
model_paths(res_dir, modcombos)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res_dir}{
%%     ~~Describe \code{res_dir} here~~
}
  \item{modcombos}{
%%     ~~Describe \code{modcombos} here~~
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
function (res_dir, modcombos) 
{
    devo_path <- function(combo, res_dir) {
        old <- res_dir
        for (i in 1:length(combo)) {
            new <- file.path(old, combo[i])
            old <- new
            dir.create(old, showWarnings = FALSE)
        }
        return(old)
    }
    alldirs <- sapply(1:nrow(modcombos), function(x) devo_path(combo = modcombos[x, 
        ], res_dir = res_dir))
    return(alldirs)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
