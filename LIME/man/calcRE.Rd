\name{calcRE}
\alias{calcRE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calcRE(modpath_vec, iter, value, yr, timeseries = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{modpath_vec}{
%%     ~~Describe \code{modpath_vec} here~~
}
  \item{iter}{
%%     ~~Describe \code{iter} here~~
}
  \item{value}{
%%     ~~Describe \code{value} here~~
}
  \item{yr}{
%%     ~~Describe \code{yr} here~~
}
  \item{timeseries}{
%%     ~~Describe \code{timeseries} here~~
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
function (modpath_vec, iter, value, yr, timeseries = FALSE) 
{
    if (timeseries == FALSE) 
        RE <- rep(NA, length(modpath_vec))
    if (timeseries == TRUE) 
        RE <- matrix(NA, nrow = yr, ncol = length(modpath_vec))
    Rep <- True <- Est <- list()
    for (m in 1:length(modpath_vec)) {
        if (grepl("LBSPR", modpath_vec[m]) == FALSE) {
            if (file.exists(file.path(modpath_vec[m], iter, "Report.rds"))) 
                Rep[[m]] <- readRDS(file.path(modpath_vec[m], 
                  iter, "Report.rds"))
            if (file.exists(file.path(modpath_vec[m], iter, "Report.rds")) == 
                FALSE) 
                next
        }
        if (grepl("LBSPR", modpath_vec[m])) {
            if (length(which(grepl("LBSPR", list.files(file.path(modpath_vec[m], 
                iter))))) == 1) 
                Rep[[m]] <- readRDS(file.path(modpath_vec[m], 
                  iter, "LBSPR_results.rds"))
            if (length(which(grepl("LBSPR", list.files(file.path(modpath_vec[m], 
                iter))))) > 1 & timeseries == FALSE) 
                Rep[[m]] <- readRDS(file.path(modpath_vec[m], 
                  iter, paste0("LBSPR_results_", yr, ".rds")))
            if (length(which(grepl("LBSPR", list.files(file.path(modpath_vec[m], 
                iter))))) > 1 & timeseries == TRUE) 
                Rep[[m]] <- sapply(1:yr, function(x) readRDS(file.path(modpath_vec[m], 
                  iter, paste0("LBSPR_results_", x, ".rds"))))
        }
        if (value == "SPR") {
            if (grepl("LBSPR", modpath_vec[m]) == FALSE) {
                if (timeseries == FALSE) 
                  Est[[m]] <- with(Rep[[m]], calc_ref(Mat_a = Mat_a, 
                    W_a = W_a, M = M, S_a = S_a, F = F_t[yr], 
                    ref = FALSE))
                if (timeseries == TRUE) 
                  Est[[m]] <- with(Rep[[m]], sapply(1:yr, function(x) calc_ref(Mat_a = Mat_a, 
                    W_a = W_a, M = M, S_a = S_a, F = F_t[x], 
                    ref = FALSE)))
            }
            if (grepl("LBSPR", modpath_vec[m])) {
                Est[[m]] <- Rep[[m]]$SPR
            }
            if (file.exists(file.path(modpath_vec[m], iter, "SPR_site.rds"))) {
                True_file <- readRDS(file.path(modpath_vec[m], 
                  iter, "SPR_site.rds"))
                if (timeseries == FALSE) 
                  True[[m]] <- mean(True_file[yr, ])
                if (timeseries == TRUE) 
                  True[[m]] <- rowMeans(True_file)
            }
            if (file.exists(file.path(modpath_vec[m], iter, "SPR_site.rds")) == 
                FALSE) {
                True_file <- readRDS(file.path(modpath_vec[m], 
                  iter, "True.rds"))
                if (timeseries == FALSE) 
                  True[[m]] <- True_file$SPR
                if (timeseries == TRUE) 
                  True[[m]] <- True_file$SPR_t
            }
        }
        if (value == "FFref") {
            if (grepl("LBSPR", modpath_vec[m]) == FALSE) {
                Est[[m]] <- with(Rep[[m]], F_t[yr]/(uniroot(calc_ref, 
                  lower = 0, upper = 50, Mat_a = Mat_a, W_a = W_a, 
                  M = M, S_a = S_a, ref = 0.4)$root))
            }
            if (grepl("LBSPR", modpath_vec[m])) {
                stop("Cannot calculate F-based reference point for LBSPR")
            }
            True_file <- readRDS(file.path(modpath_vec[m], iter, 
                "True.rds"))
            True[[m]] <- with(True_file, F_t[yr]/(uniroot(calc_ref, 
                lower = 0, upper = 50, Mat_a = Mat_a, W_a = W_a, 
                M = M, S_a = S_a, ref = 0.4)$root))
        }
        if (timeseries == FALSE) 
            RE[m] <- (Est[[m]] - True[[m]])/True[[m]]
        if (timeseries == TRUE) 
            RE[, m] <- (Est[[m]] - True[[m]])/True[[m]]
    }
    return(RE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
