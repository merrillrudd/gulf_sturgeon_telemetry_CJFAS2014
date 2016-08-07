\name{create_lh_list}
\alias{create_lh_list}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
create_lh_list(lh, param_adjust = FALSE, val = FALSE, selex, nlbins = 50)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lh}{
%%     ~~Describe \code{lh} here~~
}
  \item{param_adjust}{
%%     ~~Describe \code{param_adjust} here~~
}
  \item{val}{
%%     ~~Describe \code{val} here~~
}
  \item{selex}{
%%     ~~Describe \code{selex} here~~
}
  \item{nlbins}{
%%     ~~Describe \code{nlbins} here~~
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
function (lh, param_adjust = FALSE, val = FALSE, selex, nlbins = 50) 
{
    if (lh == 1) {
        vbk <- 0.79
        linf <- 37.6
        t0 <- -0.01
        CVlen <- 0.1
        lwa <- 0.00912
        lwb <- 3.09
        M <- 0.42
        R0 <- 1
        h <- 1
        Fequil <- 0.25
        Frate <- 0.2
        F1 <- 0.2
        Fmax <- 1
        qcoef <- 0.01
        SigmaF <- 0.3
        SigmaC <- 0.2
        SigmaI <- 0.2
        SigmaR <- 0.6
        binwidth <- 1
        SL50 <- 24
        SL95 <- 26
        ML50 <- 29
        ML95 <- 32
    }
    if (lh == 2) {
        vbk <- 0.535
        linf <- 17
        t0 <- -0.01
        CVlen <- 0.1
        lwa <- 0.01259
        lwb <- 3.08
        M <- 0.44
        R0 <- 1
        h <- 1
        Fequil <- 0.25
        Frate <- 0.2
        F1 <- 0.2
        Fmax <- 1
        qcoef <- 0.01
        SigmaF <- 0.3
        SigmaC <- 0.2
        SigmaI <- 0.2
        SigmaR <- 0.6
        binwidth <- 1
        SL50 <- 9.4
        SL95 <- 10.8
        ML50 <- 12.1
        ML95 <- 17
    }
    if (lh == 3) {
        vbk <- 0.41
        linf <- 53
        t0 <- -0.01
        CVlen <- 0.1
        lwa <- 0.0049
        lwb <- 3.06
        M <- 0.63
        R0 <- 1
        h <- 1
        Fequil <- 0.25
        Frate <- 0.2
        F1 <- 0.2
        Fmax <- 1
        qcoef <- 0.01
        SigmaF <- 0.3
        SigmaC <- 0.2
        SigmaI <- 0.2
        SigmaR <- 0.6
        binwidth <- 1
        SL50 <- 22
        SL95 <- 26
        ML50 <- 25.9
        ML95 <- 34.4
    }
    if (lh == 4) {
        vbk <- 0.41
        linf <- 34.2
        t0 <- -0.01
        CVlen <- 0.1
        lwa <- 0.0024
        lwb <- 3.15
        M <- 1.25
        R0 <- 1
        h <- 1
        Fequil <- 0.25
        Frate <- 0.2
        F1 <- 0.2
        Fmax <- 1
        qcoef <- 0.01
        SigmaF <- 0.3
        SigmaC <- 0.2
        SigmaI <- 0.2
        SigmaR <- 0.6
        binwidth <- 1
        SL50 <- 13
        SL95 <- 14.5
        ML50 <- 19.4
        ML95 <- 20.4
    }
    if ("linf" \%in\% param_adjust) 
        linf <- val[which(param_adjust == "linf")]
    if ("vbk" \%in\% param_adjust) 
        vbk <- val[which(param_adjust == "vbk")]
    if ("M" \%in\% param_adjust) 
        M <- val[which(param_adjust == "M")]
    if ("CVlen" \%in\% param_adjust) 
        CVlen <- val[which(param_adjust == "CVlen")]
    if ("SigmaR" \%in\% param_adjust) 
        SigmaR <- val[which(param_adjust == "SigmaR")]
    if ("ML50" \%in\% param_adjust) 
        ML50 <- val[which(param_adjust == "ML50")]
    AgeMax <- round(-log(0.01)/M)
    ages <- 0:AgeMax
    Amat <- round(t0 - log(1 - (ML50/linf))/vbk)
    A95 <- Amat + 1
    S50 <- round(t0 - log(1 - (SL50/linf))/vbk)
    S95 <- round(t0 - log(1 - (SL95/linf))/vbk)
    if (S50 == S95) 
        S95 <- S50 + 1
    mids <- seq((binwidth/2), nlbins, by = binwidth)
    highs <- mids + (binwidth/2)
    lows <- mids - (binwidth)/2
    L_a <- linf * (1 - exp(-vbk * (ages - t0)))
    W_a <- lwa * L_a^lwb
    Mat_a <- c(1e-20, 1/(1 + exp(Amat - ages[-1])))
    if (selex == "asymptotic") {
        S_a <- c(1e-20, 1/(1 + exp(-log(19) * (ages[-1] - S50)/(S95 - 
            S50))))
        Syoung <- NA
        Sold <- NA
    }
    if (selex == "dome") {
        S_a_calc <- rep(NA, length(ages))
        Syoung <- 0.8
        Sold <- 8
        A <- sqrt(2/pi)/(Syoung + Sold)
        for (a in 1:length(ages)) {
            if (a == 1) 
                S_a_calc[a] <- 1e-20
            if (a <= S95) 
                S_a_calc[a] <- A * exp(-((ages[a] - S95)^2)/(2 * 
                  Syoung^2))
            if (a > S95) 
                S_a_calc[a] <- A * exp(-((ages[a] - S95)^2)/(2 * 
                  Sold^2))
        }
        S_a <- S_a_calc/max(S_a_calc)
    }
    Outs <- NULL
    Outs$vbk <- vbk
    Outs$linf <- linf
    Outs$t0 <- t0
    Outs$binwidth <- binwidth
    Outs$CVlen <- CVlen
    Outs$SigmaC <- SigmaC
    Outs$SigmaI <- SigmaI
    Outs$SigmaR <- SigmaR
    Outs$R0 <- R0
    Outs$lwa <- lwa
    Outs$lwb <- lwb
    Outs$S50 <- S50
    Outs$S95 <- S95
    Outs$SL50 <- SL50
    Outs$SL95 <- SL95
    Outs$Syoung <- Syoung
    Outs$Sold <- Sold
    Outs$h <- h
    Outs$Fequil <- Fequil
    Outs$Frate <- Frate
    Outs$F1 <- F1
    Outs$qcoef <- qcoef
    Outs$Fmax <- Fmax
    Outs$SigmaF <- SigmaF
    Outs$M <- M
    Outs$AgeMax <- AgeMax
    Outs$mids <- mids
    Outs$highs <- highs
    Outs$lows <- lows
    Outs$S_a <- S_a
    Outs$L_a <- L_a
    Outs$W_a <- W_a
    Outs$Amat <- Amat
    Outs$ML50 <- ML50
    Outs$ML95 <- ML95
    Outs$Mat_a <- Mat_a
    return(Outs)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
