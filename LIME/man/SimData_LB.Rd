\name{SimData_LB}
\alias{SimData_LB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SimData_LB(Nyears, AgeMax, SigmaR, M, F1, S_a, h, qcoef, Frate, Fequil, SigmaF, Fdynamics, Rdynamics, R0, Fmax, CVlen, mids, highs, lows, W_a, L_a, linf, vbk, Mat_a, Amat, comp_sample, Nyears_comp, alt_yrs = FALSE, sample = FALSE, nburn, seed, modname)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Nyears}{
%%     ~~Describe \code{Nyears} here~~
}
  \item{AgeMax}{
%%     ~~Describe \code{AgeMax} here~~
}
  \item{SigmaR}{
%%     ~~Describe \code{SigmaR} here~~
}
  \item{M}{
%%     ~~Describe \code{M} here~~
}
  \item{F1}{
%%     ~~Describe \code{F1} here~~
}
  \item{S_a}{
%%     ~~Describe \code{S_a} here~~
}
  \item{h}{
%%     ~~Describe \code{h} here~~
}
  \item{qcoef}{
%%     ~~Describe \code{qcoef} here~~
}
  \item{Frate}{
%%     ~~Describe \code{Frate} here~~
}
  \item{Fequil}{
%%     ~~Describe \code{Fequil} here~~
}
  \item{SigmaF}{
%%     ~~Describe \code{SigmaF} here~~
}
  \item{Fdynamics}{
%%     ~~Describe \code{Fdynamics} here~~
}
  \item{Rdynamics}{
%%     ~~Describe \code{Rdynamics} here~~
}
  \item{R0}{
%%     ~~Describe \code{R0} here~~
}
  \item{Fmax}{
%%     ~~Describe \code{Fmax} here~~
}
  \item{CVlen}{
%%     ~~Describe \code{CVlen} here~~
}
  \item{mids}{
%%     ~~Describe \code{mids} here~~
}
  \item{highs}{
%%     ~~Describe \code{highs} here~~
}
  \item{lows}{
%%     ~~Describe \code{lows} here~~
}
  \item{W_a}{
%%     ~~Describe \code{W_a} here~~
}
  \item{L_a}{
%%     ~~Describe \code{L_a} here~~
}
  \item{linf}{
%%     ~~Describe \code{linf} here~~
}
  \item{vbk}{
%%     ~~Describe \code{vbk} here~~
}
  \item{Mat_a}{
%%     ~~Describe \code{Mat_a} here~~
}
  \item{Amat}{
%%     ~~Describe \code{Amat} here~~
}
  \item{comp_sample}{
%%     ~~Describe \code{comp_sample} here~~
}
  \item{Nyears_comp}{
%%     ~~Describe \code{Nyears_comp} here~~
}
  \item{alt_yrs}{
%%     ~~Describe \code{alt_yrs} here~~
}
  \item{sample}{
%%     ~~Describe \code{sample} here~~
}
  \item{nburn}{
%%     ~~Describe \code{nburn} here~~
}
  \item{seed}{
%%     ~~Describe \code{seed} here~~
}
  \item{modname}{
%%     ~~Describe \code{modname} here~~
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
function (Nyears, AgeMax, SigmaR, M, F1, S_a, h, qcoef, Frate, 
    Fequil, SigmaF, Fdynamics, Rdynamics, R0, Fmax, CVlen, mids, 
    highs, lows, W_a, L_a, linf, vbk, Mat_a, Amat, comp_sample, 
    Nyears_comp, alt_yrs = FALSE, sample = FALSE, nburn, seed, 
    modname) 
{
    tyears <- nburn + Nyears
    set.seed(seed)
    RecDev <- rnorm(tyears, mean = -SigmaR^2/2, sd = SigmaR)
    FishDev <- rnorm(tyears, mean = -SigmaF^2/2, sd = SigmaF)
    EffDev <- rnorm(tyears, mean = -SigmaF^2/2, sd = SigmaF)
    SB_t <- F_t <- R_t <- rep(NA, tyears)
    Cn_at <- N_at <- matrix(NA, nrow = AgeMax + 1, ncol = tyears)
    if (Fdynamics == "Ramp") 
        Framp_t <- c(rep(0.01, nburn), rampup = seq(F1, Fmax, 
            length = floor(Nyears/2)), peak = rep(Fmax, floor((Nyears - 
            floor(Nyears/2))/2)), managed = rep(Fmax/3, Nyears - 
            floor(Nyears/2) - floor((Nyears - floor(Nyears/2))/2)))
    if (Fdynamics == "Constant") 
        Fconstant_t <- c(rep(0.01, nburn), rep(Fequil, Nyears))
    if (Fdynamics == "Increasing") 
        Finc_t <- c(rep(0.01, nburn), seq(0.01, Fmax, length = Nyears))
    if (Rdynamics == "Pulsed") 
        Rpulse_t <- c(rep(R0, nburn), initial = rep(R0, floor(Nyears/3)), 
            pulse_down = rep(R0/3, floor(Nyears/3)), pulse_up = rep(R0, 
                Nyears - floor(Nyears/3)))
    if (Rdynamics == "Pulsed_up") 
        Rpulse_t <- c(rep(R0, nburn), initial = rep(R0, floor(Nyears/3)), 
            pulse_up = rep(R0 * 3, floor(Nyears/3)), pulse_down = rep(R0, 
                Nyears - floor(Nyears/3)))
    if (Rdynamics == "Constant") 
        Rconstant_t <- rep(R0, tyears)
    if (Fdynamics == "Endogenous") 
        F_t[1] <- 0.01
    if (Fdynamics == "Ramp") 
        F_t[1] <- Framp_t[1]
    if (Fdynamics == "Constant") 
        F_t[1] <- Fconstant_t[1]
    if (Fdynamics == "Increasing") 
        F_t[1] <- Finc_t[1]
    if (Fdynamics == "None") 
        F_t[1] <- 0
    R_t[1] <- R0
    for (a in 1:length(L_a)) {
        if (a == 1) {
            N_at[a, 1] <- R_t[1]
        }
        if (a > 1 & a < length(L_a)) {
            N_at[a, 1] <- N_at[a - 1, 1] * exp(-M - F_t[1] * 
                S_a[a - 1])
        }
        if (a == length(L_a)) {
            N_at[a, 1] <- (N_at[a - 1, 1] * exp(-M - F_t[1] * 
                S_a[a - 1]))/(1 - exp(-M - F_t[1] * S_a[a - 1]))
        }
    }
    SB_t[1] <- sum(N_at[, 1] * W_a * S_a)
    Cn_at[, 1] <- N_at[, 1] * (1 - exp(-M - F_t[1] * S_a)) * 
        (F_t[1] * S_a)/(M + F_t[1] * S_a)
    Na0 <- rep(NA, length(W_a))
    if (Rdynamics == "Pulsed") {
        R0 <- median(Rpulse_t[-c(1:nburn)])
    }
    Na0[1] <- R0
    for (a in 2:length(W_a)) {
        Na0[a] <- R0 * exp(-M * (a - 1))
    }
    SB0 <- sum(Na0[-1] * Mat_a[-1] * W_a[-1])
    for (y in 2:tyears) {
        if (Fdynamics == "Endogenous") {
            if (y <= nburn) 
                F_t[y] <- 0.01
            if (y > nburn) 
                F_t[y] <- F_t[y - 1] * (SB_t[y - 1]/(Fequil * 
                  SB0))^Frate * exp(FishDev[y])
        }
        if (Fdynamics == "Ramp") {
            F_t[y] <- Framp_t[y] * exp(FishDev[y])
        }
        if (Fdynamics == "Constant") {
            F_t[y] <- Fconstant_t[y] * exp(FishDev[y])
        }
        if (Fdynamics == "Increasing") {
            F_t[y] <- Finc_t[y] * exp(FishDev[y])
        }
        if (Fdynamics == "None") {
            F_t[y] <- 0
        }
        if (Rdynamics == "Constant") {
            R_t[y] <- Rconstant_t[y] * exp(RecDev[y])
        }
        if (Rdynamics == "Pulsed") {
            R_t[y] <- Rpulse_t[y] * exp(RecDev[y])
        }
        if (Rdynamics == "Pulsed_up") {
            R_t[y] <- Rpulse_t[y] * exp(RecDev[y])
        }
        if (Rdynamics == "BH") {
            h <- 0.7
            R_t[y] <- (4 * h * R0 * SB_t[y - 1]/(SB0 * (1 - h) + 
                SB_t[y - 1] * (5 * h - 1))) * exp(RecDev[y])
        }
        for (a in 1:length(L_a)) {
            if (a == 1) {
                N_at[a, y] <- R_t[y]
            }
            if (a > 1 & a < length(L_a)) {
                N_at[a, y] <- N_at[a - 1, y - 1] * exp(-M - F_t[y - 
                  1] * S_a[a - 1])
            }
            if (a == length(L_a)) {
                N_at[a, y] <- (N_at[a - 1, y - 1] + N_at[a, y - 
                  1]) * exp(-M - F_t[y - 1] * S_a[a - 1])
            }
        }
        SB_t[y] <- sum((N_at[, y] * W_a * Mat_a)[-1])
        Cn_at[, y] <- N_at[, y] * (1 - exp(-M - F_t[y] * S_a)) * 
            (F_t[y] * S_a)/(M + F_t[y] * S_a)
    }
    Cn_t <- colSums(Cn_at)
    N_t <- colSums(N_at[-1, ])
    D_t <- SB_t/SB0
    if (sample != FALSE) {
        C_t <- Cn_t * sample
    }
    if (sample == FALSE) {
        C_t <- Cn_t
    }
    CPUE_t <- qcoef * SB_t * exp(EffDev)
    LFinfo <- AgeToLengthComp(L_a = L_a, CVlen = CVlen, highs = highs, 
        lows = lows, tyears = tyears, N_at = N_at, S_a = S_a, 
        comp_sample = rep(comp_sample, tyears))
    plba <- LFinfo$plba
    plb <- LFinfo$plb
    page <- LFinfo$page
    LF <- LFinfo$LF
    L_t <- vector(length = tyears)
    for (y in 1:tyears) {
        vul_pop <- sum(N_at[, y] * S_a)
        vul_lengths <- sum(vul_pop * plb[y, ] * mids)
        L_t[y] <- vul_lengths/vul_pop
    }
    CPUE_tout <- CPUE_t[-c(1:nburn)]
    C_tout <- C_t[-c(1:nburn)]
    names(C_tout) <- names(CPUE_tout) <- 1:Nyears
    LFout <- LF[-c(1:nburn), ]
    rownames(LFout) <- 1:Nyears
    R_tout <- R_t[-c(1:nburn)]
    N_tout <- N_t[-c(1:nburn)]
    SB_tout <- SB_t[-c(1:nburn)]
    D_tout <- D_t[-c(1:nburn)]
    F_tout <- F_t[-c(1:nburn)]
    L_tout <- L_t[-c(1:nburn)]
    N_atout <- N_at[, -c(1:nburn)]
    if (alt_yrs == TRUE) {
        yrs <- Nyears:1
        index <- rep(c(1, 1, 0, 0), Nyears/4)
        yr_vec <- rev(yrs[which(index == 1)])
        C_tout <- C_tout[which(names(C_tout) \%in\% yr_vec)]
        CPUE_tout <- CPUE_tout[which(names(CPUE_tout) \%in\% yr_vec)]
        LFout <- LFout[which(rownames(LFout) \%in\% yr_vec), ]
    }
    LFindex <- (Nyears - Nyears_comp + 1):Nyears
    LFout <- LFout[LFindex, ]
    if (is.vector(LFout)) {
        LFout <- t(as.matrix(LFout))
        rownames(LFout) <- (Nyears - Nyears_comp + 1):Nyears
    }
    SPR_t <- sapply(1:length(F_tout), function(x) calc_ref(Mat_a = Mat_a, 
        W_a = W_a, M = M, S_a = S_a, F = F_tout[x]))
    SPR <- SPR_t[length(SPR_t)]
    lbins <- lows
    if (Nyears_comp > 1) 
        ML_t <- sapply(1:nrow(LFout), function(x) sum(LFout[x, 
            ] * lbins)/sum(LFout[x, ]))
    if (Nyears_comp == 1) 
        ML_t <- sum(LFout * lbins)/sum(LFout)
    DataList <- list(I_t = CPUE_tout, C_t = C_tout, DataScenario = modname, 
        LF = LFout, SigmaR = SigmaR, R_t = R_tout, N_t = N_tout, 
        SB_t = SB_tout, D_t = D_tout, F_t = F_tout, L_t = L_tout, 
        N_at = N_atout, Amat = Amat, Mat_a = Mat_a, SB0 = SB0, 
        Nyears = Nyears, L_a = L_a, W_a = W_a, AgeMax = AgeMax, 
        M = M, S_a = S_a, plb = plb, plba = plba, page = page, 
        R0 = R0, SPR = SPR, SPR_t = SPR_t, ML_t = ML_t, nlbins = length(mids))
    return(DataList)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
