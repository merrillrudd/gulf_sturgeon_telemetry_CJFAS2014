\name{choose_lh_list}
\alias{choose_lh_list}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
choose_lh_list(species, selex)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{species}{
%%     ~~Describe \code{species} here~~
}
  \item{selex}{
%%     ~~Describe \code{selex} here~~
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
function (species, selex) 
{
    if (species == "CRSNAP") {
        vbk <- 0.21
        linf <- 64.58
        t0 <- -0.01
        CVlen <- 0.2
        lwa <- 0.0245
        lwb <- 2.79
        M <- 0.43
        AgeMax <- 23
        R0 <- 1
        h <- 1
        qcoef <- 1e-05
        SigmaF <- 0.1
        SigmaC <- 0.2
        SigmaI <- 0.2
        SigmaR <- 0.6
        S50 <- 3
        S95 <- 5
        L50 <- 34
        binwidth <- 1
        mids <- seq((binwidth/2), linf * 1.5, by = binwidth)
        highs <- mids + (binwidth/2)
        lows <- mids - (binwidth)/2
        F1 <- 0.34
        ages <- 0:AgeMax
        Amat <- round(t0 - log(1 - (L50/linf))/vbk)
        A95 <- Amat + 1
        L95 <- round(linf * (1 - exp(-vbk * (A95 - t0))))
        SL50 <- round(linf * (1 - exp(-vbk * (S50 - t0))))
        SL95 <- round(linf * (1 - exp(-vbk * (S95 - t0))))
        L_a <- linf * (1 - exp(-vbk * (ages - t0)))
        W_a <- lwa * L_a^lwb
        Mat_a <- 1/(1 + exp(Amat - ages))
        if (selex == "asymptotic") {
            S_a <- 1/(1 + exp(-log(19) * (ages - S50)/(S95 - 
                S50)))
            S_a[1] <- 1e-20
            Syoung <- NA
            Sold <- NA
        }
        if (selex == "dome") {
            S_a_calc <- rep(NA, length(ages))
            stop("Need to parameterize dome-shape")
            A <- sqrt(2/pi)/(Syoung + Sold)
            for (a in 1:length(ages)) {
                if (a <= S95) 
                  S_a_calc[a] <- A * exp(-((ages[a] - S95)^2)/(2 * 
                    Syoung^2))
                if (a > S95) 
                  S_a_calc[a] <- A * exp(-((ages[a] - S95)^2)/(2 * 
                    Sold^2))
            }
            S_a_calc[1] <- 1e-20
            S_a <- S_a_calc/max(S_a_calc)
        }
    }
    if (species == "SIGSUT") {
        vbk <- 0.87
        linf <- 36.2
        t0 <- -0.2
        CVlen <- 0.1
        lwa <- 0.0597
        lwb <- 2.754
        R0 <- 1
        h <- 1
        qcoef <- 0.01
        SigmaF <- 0.3
        SigmaC <- 0.2
        SigmaI <- 0.2
        SigmaR <- 0.6
        binwidth <- 1
        L50 <- 20.2
        SL50 <- 11.3
        F1 <- 2
        M <- 1.6 * vbk
        AgeMax <- round(-log(0.01)/M)
        ages <- 0:AgeMax
        Amat <- round(t0 - log(1 - (L50/linf))/vbk)
        A95 <- Amat + 1
        L95 <- round(linf * (1 - exp(-vbk * (A95 - t0))))
        S50 <- ceiling(t0 - log(1 - (SL50/linf))/vbk)
        S95 <- S50 + 1
        SL95 <- round(linf * (1 - exp(-vbk * (S95 - t0))))
        mids <- seq((binwidth/2), by = binwidth, length = 76)
        highs <- mids + (binwidth/2)
        lows <- mids - (binwidth)/2
        L_a <- linf * (1 - exp(-vbk * (ages - t0)))
        W_a <- lwa * L_a^lwb
        Mat_a <- 1/(1 + exp(Amat - ages))
        if (selex == "asymptotic") {
            S_a <- 1/(1 + exp(-log(19) * (ages - S50)/(S95 - 
                S50)))
            S_a[1] <- 1e-20
            Syoung <- NA
            Sold <- NA
        }
        if (selex == "dome") {
            S_a_calc <- rep(NA, length(ages))
            Syoung <- 0.8
            Sold <- 8
            A <- sqrt(2/pi)/(Syoung + Sold)
            for (a in 1:length(ages)) {
                if (a <= S95) 
                  S_a_calc[a] <- A * exp(-((ages[a] - S95)^2)/(2 * 
                    Syoung^2))
                if (a > S95) 
                  S_a_calc[a] <- A * exp(-((ages[a] - S95)^2)/(2 * 
                    Sold^2))
            }
            S_a_calc[1] <- 1e-20
            S_a <- S_a_calc/max(S_a_calc)
        }
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
    Outs$qcoef <- qcoef
    Outs$SigmaF <- SigmaF
    Outs$M <- M
    Outs$F1 <- F1
    Outs$AgeMax <- AgeMax
    Outs$mids <- mids
    Outs$highs <- highs
    Outs$lows <- lows
    Outs$S_a <- S_a
    Outs$L_a <- L_a
    Outs$W_a <- W_a
    Outs$Amat <- Amat
    Outs$L50 <- L50
    Outs$L95 <- L95
    Outs$Mat_a <- Mat_a
    return(Outs)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
