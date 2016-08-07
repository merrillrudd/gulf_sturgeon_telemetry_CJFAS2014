\name{FormatInput_LB}
\alias{FormatInput_LB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
FormatInput_LB(Nyears, DataList, linf, vbk, t0, M, AgeMax, lbhighs, lbmids, Mat_a, lwa, lwb, log_sigma_C, log_sigma_I, log_CV_L, F1, SigmaR, qcoef, R0, S50, S95, version, model, RecDev_biasadj, site, Fpen, Dpen, Dprior, SigRpen, SigRprior, obs_per_yr, SigmaF, RecType, FType, LType, h, SelexTypeDesc, est_sigma, REML, estimate_same, start_f)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Nyears}{
%%     ~~Describe \code{Nyears} here~~
}
  \item{DataList}{
%%     ~~Describe \code{DataList} here~~
}
  \item{linf}{
%%     ~~Describe \code{linf} here~~
}
  \item{vbk}{
%%     ~~Describe \code{vbk} here~~
}
  \item{t0}{
%%     ~~Describe \code{t0} here~~
}
  \item{M}{
%%     ~~Describe \code{M} here~~
}
  \item{AgeMax}{
%%     ~~Describe \code{AgeMax} here~~
}
  \item{lbhighs}{
%%     ~~Describe \code{lbhighs} here~~
}
  \item{lbmids}{
%%     ~~Describe \code{lbmids} here~~
}
  \item{Mat_a}{
%%     ~~Describe \code{Mat_a} here~~
}
  \item{lwa}{
%%     ~~Describe \code{lwa} here~~
}
  \item{lwb}{
%%     ~~Describe \code{lwb} here~~
}
  \item{log_sigma_C}{
%%     ~~Describe \code{log_sigma_C} here~~
}
  \item{log_sigma_I}{
%%     ~~Describe \code{log_sigma_I} here~~
}
  \item{log_CV_L}{
%%     ~~Describe \code{log_CV_L} here~~
}
  \item{F1}{
%%     ~~Describe \code{F1} here~~
}
  \item{SigmaR}{
%%     ~~Describe \code{SigmaR} here~~
}
  \item{qcoef}{
%%     ~~Describe \code{qcoef} here~~
}
  \item{R0}{
%%     ~~Describe \code{R0} here~~
}
  \item{S50}{
%%     ~~Describe \code{S50} here~~
}
  \item{S95}{
%%     ~~Describe \code{S95} here~~
}
  \item{version}{
%%     ~~Describe \code{version} here~~
}
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{RecDev_biasadj}{
%%     ~~Describe \code{RecDev_biasadj} here~~
}
  \item{site}{
%%     ~~Describe \code{site} here~~
}
  \item{Fpen}{
%%     ~~Describe \code{Fpen} here~~
}
  \item{Dpen}{
%%     ~~Describe \code{Dpen} here~~
}
  \item{Dprior}{
%%     ~~Describe \code{Dprior} here~~
}
  \item{SigRpen}{
%%     ~~Describe \code{SigRpen} here~~
}
  \item{SigRprior}{
%%     ~~Describe \code{SigRprior} here~~
}
  \item{obs_per_yr}{
%%     ~~Describe \code{obs_per_yr} here~~
}
  \item{SigmaF}{
%%     ~~Describe \code{SigmaF} here~~
}
  \item{RecType}{
%%     ~~Describe \code{RecType} here~~
}
  \item{FType}{
%%     ~~Describe \code{FType} here~~
}
  \item{LType}{
%%     ~~Describe \code{LType} here~~
}
  \item{h}{
%%     ~~Describe \code{h} here~~
}
  \item{SelexTypeDesc}{
%%     ~~Describe \code{SelexTypeDesc} here~~
}
  \item{est_sigma}{
%%     ~~Describe \code{est_sigma} here~~
}
  \item{REML}{
%%     ~~Describe \code{REML} here~~
}
  \item{estimate_same}{
%%     ~~Describe \code{estimate_same} here~~
}
  \item{start_f}{
%%     ~~Describe \code{start_f} here~~
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
function (Nyears, DataList, linf, vbk, t0, M, AgeMax, lbhighs, 
    lbmids, Mat_a, lwa, lwb, log_sigma_C, log_sigma_I, log_CV_L, 
    F1, SigmaR, qcoef, R0, S50, S95, version, model, RecDev_biasadj, 
    site, Fpen, Dpen, Dprior, SigRpen, SigRprior, obs_per_yr, 
    SigmaF, RecType, FType, LType, h, SelexTypeDesc, est_sigma, 
    REML, estimate_same, start_f) 
{
    if (grepl("lb_statespace_v6", version)) {
        if ((grepl("Rich", model) | grepl("Moderate", model) | 
            grepl("Sample", model)) & grepl("LC", model)) {
            Data <- list(n_t = Nyears, n_lb = ncol(DataList$LF), 
                n_c = length(DataList$C_t), n_i = length(DataList$I_t), 
                n_lc = nrow(DataList$LF), n_ml = 0, start_f = start_f, 
                T_yrs = 1:Nyears, C_yrs = as.numeric(names(DataList$C_t)), 
                I_yrs = as.numeric(names(DataList$I_t)), LC_yrs = as.numeric(rownames(DataList$LF)), 
                ML_yrs = as.vector(0), rel_c = 0, rel_i = 0, 
                obs_per_yr = obs_per_yr, RecType = RecType, I_t = DataList$I_t, 
                C_t = DataList$C_t, ML_t = as.vector(0), LF = DataList$LF, 
                linf = linf, vbk = vbk, t0 = t0, M = M, h = h, 
                AgeMax = AgeMax, lbhighs = lbhighs, lbmids = lbmids, 
                Mat_a = Mat_a, lwa = lwa, lwb = lwb, Fpen = Fpen, 
                SigRpen = SigRpen, SigRprior = SigRprior, RecDev_biasadj = RecDev_biasadj)
        }
        if ((grepl("Rich", model) | grepl("Moderate", model) | 
            grepl("Sample", model)) & grepl("ML", model)) {
            Data <- list(n_t = Nyears, n_lb = ncol(DataList$LF), 
                n_c = length(DataList$C_t), n_i = length(DataList$I_t), 
                n_lc = 0, n_ml = nrow(DataList$LF), start_f = start_f, 
                T_yrs = 1:Nyears, C_yrs = as.numeric(names(DataList$C_t)), 
                I_yrs = as.numeric(names(DataList$I_t)), LC_yrs = as.vector(0), 
                ML_yrs = as.numeric(rownames(DataList$LF)), rel_c = 0, 
                rel_i = 0, obs_per_yr = obs_per_yr, RecType = RecType, 
                I_t = DataList$I_t, C_t = DataList$C_t, ML_t = rowMeans(DataList$LF), 
                LF = as.matrix(0), linf = linf, vbk = vbk, t0 = t0, 
                M = M, h = h, AgeMax = AgeMax, lbhighs = lbhighs, 
                lbmids = lbmids, Mat_a = Mat_a, lwa = lwa, lwb = lwb, 
                Fpen = Fpen, SigRpen = SigRpen, SigRprior = SigRprior, 
                RecDev_biasadj = RecDev_biasadj)
        }
        if (grepl("Index", model) & grepl("LC", model)) {
            if (is.matrix(DataList$LF)) {
                n_lc <- nrow(DataList$LF)
                LC_yrs <- as.numeric(rownames(DataList$LF))
                LF <- as.matrix(DataList$LF)
            }
            if (is.vector(DataList$LF)) {
                n_lc <- 1
                LC_yrs <- Nyears
                LF <- t(as.matrix(DataList$LF))
            }
            Data <- list(n_t = Nyears, n_lb = ncol(DataList$LF), 
                n_c = 0, n_i = length(DataList$I_t), n_lc = n_lc, 
                n_ml = 0, start_f = start_f, T_yrs = 1:Nyears, 
                C_yrs = as.vector(0), I_yrs = as.numeric(names(DataList$I_t)), 
                LC_yrs = LC_yrs, ML_yrs = as.vector(0), rel_c = 0, 
                rel_i = 0, obs_per_yr = obs_per_yr, RecType = RecType, 
                I_t = DataList$I_t, C_t = as.vector(0), ML_t = as.vector(0), 
                LF = LF, linf = linf, vbk = vbk, t0 = t0, M = M, 
                h = h, AgeMax = AgeMax, lbhighs = lbhighs, lbmids = lbmids, 
                Mat_a = Mat_a, lwa = lwa, lwb = lwb, Fpen = Fpen, 
                SigRpen = SigRpen, SigRprior = SigRprior, RecDev_biasadj = RecDev_biasadj)
        }
        if (grepl("Index", model) & grepl("ML", model)) {
            if (is.matrix(DataList$LF)) {
                n_ml <- nrow(DataList$LF)
                ML_yrs <- as.numeric(rownames(DataList$LF))
                LF <- as.matrix(DataList$LF)
            }
            if (is.vector(DataList$LF)) {
                n_ml <- 1
                ML_yrs <- Nyears
                LF <- t(as.matrix(DataList$LF))
            }
            Data <- list(n_t = Nyears, n_lb = ncol(DataList$LF), 
                n_c = 0, n_i = length(DataList$I_t), n_lc = 0, 
                n_ml = n_ml, start_f = start_f, T_yrs = 1:Nyears, 
                C_yrs = as.vector(0), I_yrs = as.numeric(names(DataList$I_t)), 
                LC_yrs = as.vector(0), ML_yrs = ML_yrs, rel_c = 0, 
                rel_i = 0, obs_per_yr = obs_per_yr, RecType = RecType, 
                I_t = DataList$I_t, C_t = as.vector(0), ML_t = rowMeans(LF), 
                LF = as.matrix(0), linf = linf, vbk = vbk, t0 = t0, 
                M = M, h = h, AgeMax = AgeMax, lbhighs = lbhighs, 
                lbmids = lbmids, Mat_a = Mat_a, lwa = lwa, lwb = lwb, 
                Fpen = Fpen, SigRpen = SigRpen, SigRprior = SigRprior, 
                RecDev_biasadj = RecDev_biasadj)
        }
        if (grepl("Catch", model) & grepl("LC", model)) {
            if (is.matrix(DataList$LF)) {
                n_lc <- nrow(DataList$LF)
                LC_yrs <- as.numeric(rownames(DataList$LF))
                LF <- as.matrix(DataList$LF)
            }
            if (is.vector(DataList$LF)) {
                n_lc <- 1
                LC_yrs <- Nyears
                LF <- t(as.matrix(DataList$LF))
            }
            Data <- list(n_t = Nyears, n_lb = ncol(DataList$LF), 
                n_c = length(DataList$C_t), n_i = 0, n_lc = n_lc, 
                n_ml = 0, start_f = start_f, T_yrs = 1:Nyears, 
                C_yrs = as.numeric(names(DataList$C_t)), I_yrs = as.vector(0), 
                LC_yrs = LC_yrs, ML_yrs = as.vector(0), rel_c = 0, 
                rel_i = 0, obs_per_yr = obs_per_yr, RecType = RecType, 
                I_t = as.vector(0), C_t = DataList$C_t, ML_t = as.vector(0), 
                LF = LF, linf = linf, vbk = vbk, t0 = t0, M = M, 
                h = h, AgeMax = AgeMax, lbhighs = lbhighs, lbmids = lbmids, 
                Mat_a = Mat_a, lwa = lwa, lwb = lwb, Fpen = Fpen, 
                SigRpen = SigRpen, SigRprior = SigRprior, RecDev_biasadj = RecDev_biasadj)
        }
        if (grepl("Catch", model) & grepl("ML", model)) {
            if (is.matrix(DataList$LF)) {
                n_ml <- nrow(DataList$LF)
                ML_yrs <- as.numeric(rownames(DataList$LF))
                LF <- as.matrix(DataList$LF)
            }
            if (is.vector(DataList$LF)) {
                n_ml <- 1
                ML_yrs <- Nyears
                LF <- t(as.matrix(DataList$LF))
            }
            Data <- list(n_t = Nyears, n_lb = ncol(DataList$LF), 
                n_c = length(DataList$C_t), n_i = 0, n_lc = 0, 
                n_ml = n_ml, start_f = start_f, T_yrs = 1:Nyears, 
                C_yrs = as.numeric(names(DataList$C_t)), I_yrs = s.vector(0), 
                LC_yrs = as.vector(0), ML_yrs = ML_yrs, rel_c = 0, 
                rel_i = 0, obs_per_yr = obs_per_yr, RecType = RecType, 
                I_t = as.vector(0), C_t = DataList$C_t, ML_t = rowMeans(LF), 
                LF = as.matrix(0), linf = linf, vbk = vbk, t0 = t0, 
                M = M, h = h, AgeMax = AgeMax, lbhighs = lbhighs, 
                lbmids = lbmids, Mat_a = Mat_a, lwa = lwa, lwb = lwb, 
                Fpen = Fpen, SigRpen = SigRpen, SigRprior = SigRprior, 
                RecDev_biasadj = RecDev_biasadj)
        }
        if (grepl("LC", model) & grepl("Index", model) == FALSE & 
            grepl("Catch", model) == FALSE & grepl("Rich", model) == 
            FALSE & grepl("Moderate", model) == FALSE & grepl("Sample", 
            model) == FALSE) {
            if (is.matrix(DataList$LF)) {
                n_lc <- nrow(DataList$LF)
                LC_yrs <- as.numeric(rownames(DataList$LF))
                LF <- as.matrix(DataList$LF)
            }
            if (is.vector(DataList$LF)) {
                n_lc <- 1
                LC_yrs <- Nyears
                LF <- t(as.matrix(DataList$LF))
            }
            Data <- list(n_t = Nyears, n_lb = ncol(DataList$LF), 
                n_c = 0, n_i = 0, n_lc = n_lc, n_ml = 0, start_f = start_f, 
                T_yrs = 1:Nyears, C_yrs = as.vector(0), I_yrs = as.vector(0), 
                LC_yrs = LC_yrs, ML_yrs = as.vector(0), rel_c = 0, 
                rel_i = 0, obs_per_yr = obs_per_yr, RecType = RecType, 
                I_t = as.vector(0), C_t = as.vector(0), ML_t = as.vector(0), 
                LF = LF, linf = linf, vbk = vbk, t0 = t0, M = M, 
                h = h, AgeMax = AgeMax, lbhighs = lbhighs, lbmids = lbmids, 
                Mat_a = Mat_a, lwa = lwa, lwb = lwb, Fpen = Fpen, 
                SigRpen = SigRpen, SigRprior = SigRprior, RecDev_biasadj = RecDev_biasadj)
        }
    }
    Parameters <- list(log_F_sd = log(SigmaF), log_F_t_input = log(rep(F1, 
        Nyears)), log_q_I = log(qcoef), beta = log(R0), log_sigma_R = log(SigmaR), 
        logS50 = log(S50), logS95 = log(S95), log_sigma_C = log_sigma_C, 
        log_sigma_I = log_sigma_I, log_CV_L = log_CV_L, Nu_input = rep(0, 
            Nyears))
    Map = list()
    if (estimate_same == TRUE) {
        Map[["beta"]] <- NA
        Map[["beta"]] <- factor(Map[["beta"]])
        Map[["log_q_I"]] <- NA
        Map[["log_q_I"]] <- factor(Map[["log_q_I"]])
    }
    if ("log_F_sd" \%in\% est_sigma == FALSE) {
        Map[["log_F_sd"]] <- NA
        Map[["log_F_sd"]] <- factor(Map[["log_F_sd"]])
    }
    if ("log_sigma_R" \%in\% est_sigma == FALSE) {
        Map[["log_sigma_R"]] <- NA
        Map[["log_sigma_R"]] <- factor(Map[["log_sigma_R"]])
    }
    if ("log_sigma_C" \%in\% est_sigma == FALSE) {
        Map[["log_sigma_C"]] <- NA
        Map[["log_sigma_C"]] <- factor(Map[["log_sigma_C"]])
    }
    if ("log_sigma_I" \%in\% est_sigma == FALSE) {
        Map[["log_sigma_I"]] <- NA
        Map[["log_sigma_I"]] <- factor(Map[["log_sigma_I"]])
    }
    if ("log_CV_L" \%in\% est_sigma == FALSE) {
        Map[["log_CV_L"]] <- NA
        Map[["log_CV_L"]] <- factor(Map[["log_CV_L"]])
    }
    if (all(est_sigma == FALSE)) {
        Map[["log_CV_L"]] <- NA
        Map[["log_CV_L"]] <- factor(Map[["log_CV_L"]])
        Map[["log_sigma_C"]] <- NA
        Map[["log_sigma_C"]] <- factor(Map[["log_sigma_C"]])
        Map[["log_sigma_I"]] <- NA
        Map[["log_sigma_I"]] <- factor(Map[["log_sigma_I"]])
        Map[["log_sigma_R"]] <- NA
        Map[["log_sigma_R"]] <- factor(Map[["log_sigma_R"]])
    }
    if (start_f > 0) {
        Map[["log_F_t_input"]] = 1:length(Parameters[["log_F_t_input"]])
        Map[["log_F_t_input"]][1:start_f] <- NA
        Map[["log_F_t_input"]] <- factor(Map[["log_F_t_input"]])
    }
    if (estimate_same == FALSE) {
        if (grepl("Index", model)) {
            Map[["beta"]] <- NA
            Map[["beta"]] <- factor(Map[["beta"]])
        }
        if (grepl("Catch", model)) {
            Map[["log_q_I"]] <- NA
            Map[["log_q_I"]] <- factor(Map[["log_q_I"]])
        }
        if (grepl("LC", model) & grepl("Index", model) == FALSE & 
            grepl("Catch", model) == FALSE & grepl("Rich", model) == 
            FALSE & grepl("Moderate", model) == FALSE & grepl("Sample", 
            model) == FALSE) {
            Map[["log_q_I"]] <- NA
            Map[["log_q_I"]] <- factor(Map[["log_q_I"]])
            Map[["beta"]] <- NA
            Map[["beta"]] <- factor(Map[["beta"]])
        }
    }
    if (length(Map) == 0) 
        Map <- NULL
    if (REML == FALSE) 
        Random <- c("Nu_input")
    if (REML == TRUE) {
        Random_vec <- c("Nu_input", "log_F_t_input", "log_q_I", 
            "beta", "logS50", "logS95")
        Random <- Random_vec[which(Random_vec \%in\% names(Map) == 
            FALSE)]
    }
    if ("log_F_sd" \%in\% est_sigma) 
        Random <- c(Random, "log_F_t_input")
    Return <- list(Parameters = Parameters, Data = Data, Random = Random, 
        Map = Map)
    return(Return)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
