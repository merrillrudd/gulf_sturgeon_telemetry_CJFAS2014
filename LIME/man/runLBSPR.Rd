\name{runLBSPR}
\alias{runLBSPR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
runLBSPR(Nyears_comp, inits, iterpath, DataList, species)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Nyears_comp}{
%%     ~~Describe \code{Nyears_comp} here~~
}
  \item{inits}{
%%     ~~Describe \code{inits} here~~
}
  \item{iterpath}{
%%     ~~Describe \code{iterpath} here~~
}
  \item{DataList}{
%%     ~~Describe \code{DataList} here~~
}
  \item{species}{
%%     ~~Describe \code{species} here~~
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
function (Nyears_comp, inits, iterpath, DataList, species) 
{
    require(LBSPR)
    setClass("LB_pars", representation(Species = "character", 
        MK = "numeric", Linf = "numeric", CVLinf = "numeric", 
        L50 = "numeric", L95 = "numeric", Walpha = "numeric", 
        Wbeta = "numeric", FecB = "numeric", Steepness = "numeric", 
        Mpow = "numeric", R0 = "numeric", SL50 = "numeric", SL95 = "numeric", 
        FM = "numeric", SPR = "vector", BinMin = "numeric", BinMax = "numeric", 
        BinWidth = "numeric"), validity = check_pars)
    LB_pars <- new("LB_pars")
    LB_pars@Linf <- inits$linf
    LB_pars@L50 <- inits$ML50
    LB_pars@L95 <- inits$ML95
    LB_pars@MK <- inits$M/inits$vbk
    LB_pars@SL50 <- inits$SL50
    LB_pars@SL95 <- inits$SL95
    LB_pars@SPR <- 0.4
    LB_pars@BinWidth <- inits$binwidth
    LB_pars@Walpha <- inits$lwa
    LB_pars@Wbeta <- inits$lwb
    LB_pars@Steepness <- inits$h
    LB_pars@R0 <- inits$R0
    LB_pars@Species <- species
    setClass("LB_lengths", representation(LMids = "vector", LData = "matrix", 
        Years = "vector", NYears = "numeric", Elog = "numeric"))
    LB_lengths <- new("LB_lengths")
    LB_lengths@LMids <- inits$mids
    LB_lengths@LData <- t(DataList$LF)
    LB_lengths@Years <- 1:Nyears_comp
    LB_lengths@NYears <- Nyears_comp
    lbspr_res <- LBSPRfit(LB_pars = LB_pars, LB_lengths = LB_lengths, 
        yrs = NA, Control = list(modtype = "absel"))
    LBSPR_outs <- list()
    LBSPR_outs$pLF <- lbspr_res@pLCatch
    LBSPR_outs$SL50 <- lbspr_res@Ests[, "SL50"]
    LBSPR_outs$SL95 <- lbspr_res@Ests[, "SL95"]
    LBSPR_outs$FM <- lbspr_res@Ests[, "FM"]
    LBSPR_outs$SPR <- lbspr_res@Ests[, "SPR"]
    saveRDS(LBSPR_outs, file.path(iterpath, "LBSPR_results.rds"))
    rm(LB_lengths)
    rm(LB_pars)
    return(LBSPR_outs)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
