\name{create_inputs}
\alias{create_inputs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
create_inputs(param, val, lh_list, data_avail_list)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{param}{
%%     ~~Describe \code{param} here~~
}
  \item{val}{
%%     ~~Describe \code{val} here~~
}
  \item{lh_list}{
%%     ~~Describe \code{lh_list} here~~
}
  \item{data_avail_list}{
%%     ~~Describe \code{data_avail_list} here~~
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
function (param, val, lh_list, data_avail_list) 
{
    dat_input <- c(lh_list, data_avail_list)
    dat_input$log_sigma_C <- log(lh_list$SigmaC)
    dat_input$log_sigma_I <- log(lh_list$SigmaI)
    if (param != FALSE) {
        for (pp in 1:length(param)) {
            dat_input[[param[pp]]] <- val[which(param == param[pp])]
        }
    }
    dat_input$log_CV_L <- log(dat_input$CVlen)
    return(dat_input)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
