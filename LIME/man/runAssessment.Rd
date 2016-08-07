\name{runAssessment}
\alias{runAssessment}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
runAssessment(modpath, DataList, lh_inputs, data_avail, est_sigma, biascorrect = TRUE, sensitivity_inputs = NULL, sensitivity_ESS = NULL, REML = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{modpath}{
%%     ~~Describe \code{modpath} here~~
}
  \item{DataList}{
%%     ~~Describe \code{DataList} here~~
}
  \item{lh_inputs}{
%%     ~~Describe \code{lh_inputs} here~~
}
  \item{data_avail}{
%%     ~~Describe \code{data_avail} here~~
}
  \item{est_sigma}{
%%     ~~Describe \code{est_sigma} here~~
}
  \item{biascorrect}{
%%     ~~Describe \code{biascorrect} here~~
}
  \item{sensitivity_inputs}{
%%     ~~Describe \code{sensitivity_inputs} here~~
}
  \item{sensitivity_ESS}{
%%     ~~Describe \code{sensitivity_ESS} here~~
}
  \item{REML}{
%%     ~~Describe \code{REML} here~~
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
function (modpath, DataList, lh_inputs, data_avail, est_sigma, 
    biascorrect = TRUE, sensitivity_inputs = NULL, sensitivity_ESS = NULL, 
    REML = FALSE) 
{
    if (is.null(sensitivity_inputs)) {
        param <- FALSE
        val <- FALSE
    }
    inits <- create_inputs(lh_list = lh_inputs, data_avail_list = NULL, 
        param = param, val = val)
    Nyears <- DataList$Nyears
    if (biascorrect == FALSE) 
        vec <- 1
    if (biascorrect == TRUE) 
        vec <- 1:2
    Sdreport <- NA
    ParList <- NA
    df <- NA
    for (bb in vec) {
        if (all(is.na(Sdreport))) 
            RecDev_biasadj <- rep(0, Nyears)
        if (all(is.na(Sdreport)) == FALSE) {
            SD <- summary(Sdreport)
            RecDev_biasadj <- 1 - SD[which(rownames(SD) == "Nu_input"), 
                "Std. Error"]^2/Report$sigma_R^2
        }
        TmbList <- FormatInput_LB(Nyears = Nyears, DataList = DataList, 
            linf = inits$linf, vbk = inits$vbk, t0 = inits$t0, 
            M = inits$M, AgeMax = inits$AgeMax, lbhighs = inits$highs, 
            lbmids = inits$mids, Mat_a = inits$Mat_a, lwa = inits$lwa, 
            lwb = inits$lwb, log_sigma_C = inits$log_sigma_C, 
            log_sigma_I = inits$log_sigma_I, log_CV_L = inits$log_CV_L, 
            F1 = inits$F1, SigmaR = inits$SigmaR, qcoef = inits$qcoef, 
            R0 = inits$R0, S50 = inits$S50, S95 = inits$S95, 
            version = Version, model = data_avail, RecDev_biasadj = RecDev_biasadj, 
            SigmaF = inits$SigmaF, Fpen = 1, Dpen = 0, Dprior = c(0.8, 
                0.2), SigRpen = 1, SigRprior = c(inits$SigmaR, 
                0.2), obs_per_yr = DataList$obs_per_year, RecType = 0, 
            FType = 0, LType = 1, h = inits$h, SelexTypeDesc = "asymptotic", 
            est_sigma = est_sigma, REML = REML, site = 1, estimate_same = FALSE)
        if (bb == 1) 
            saveRDS(TmbList, file.path(modpath, "Inputs1.rds"))
        if (bb == 2) 
            saveRDS(TmbList, file.path(modpath, "Inputs2.rds"))
        dyn.load(paste0(run_exe, "\\", dynlib(Version)))
        if (all(is.na(ParList))) 
            ParList <- TmbList[["Parameters"]]
        obj <- MakeADFun(data = TmbList[["Data"]], parameters = ParList, 
            random = TmbList[["Random"]], map = TmbList[["Map"]], 
            inner.control = list(maxit = 1000), hessian = FALSE)
        if (bb == 1) 
            obj_save <- NA
        obj$env$inner.control <- c(obj$env$inner.control, step.tol = c(1e-08, 
            1e-12)[1], tol10 = c(1e-06, 1e-08)[1], grad.tol = c(1e-08, 
            1e-12)[1])
        obj$hessian <- FALSE
        Upr = rep(Inf, length(obj$par))
        Upr[match("log_sigma_R", names(obj$par))] = log(2)
        Upr[match("logS95", names(obj$par))] = log(inits$AgeMax)
        Upr[match("log50", names(obj$par))] = obj$par[match("logS95", 
            names(obj$par))]
        Upr[which(names(obj$par) == "log_F_t_input")] = log(5)
        Upr[match("log_F_sd", names(obj$par))] <- log(2)
        Lwr <- rep(-Inf, length(obj$par))
        if (bb == 1) 
            opt_save <- NA
        opt <- tryCatch(nlminb(start = obj$par, objective = obj$fn, 
            gradient = obj$gr, upper = Upr, lower = Lwr, control = list(trace = 1, 
                eval.max = 10000, iter.max = 10000, rel.tol = 1e-10)), 
            error = function(e) NA)
        if (all(is.na(opt)) == FALSE) {
            opt[["final_gradient"]] = obj$gr(opt$par)
            opt_save <- opt
        }
        for (i in 1:10) {
            if (all(is.na(opt))) {
                obj <- MakeADFun(data = TmbList[["Data"]], parameters = ParList, 
                  random = TmbList[["Random"]], map = TmbList[["Map"]], 
                  inner.control = list(maxit = 1000), hessian = FALSE)
                opt <- tryCatch(nlminb(start = obj$env$last.par.best[-obj$env$random] + 
                  rnorm(length(obj$par), 0, 0.2), objective = obj$fn, 
                  gradient = obj$gr, upper = Upr, lower = Lwr, 
                  control = list(trace = 1, eval.max = 10000, 
                    iter.max = 10000, rel.tol = 1e-10)), error = function(e) NA)
            }
            if (all(is.na(opt)) == FALSE) {
                opt[["final_gradient"]] = obj$gr(opt$par)
                opt_save <- opt
                obj_save <- obj
                break
            }
        }
        if (all(is.na(opt)) == FALSE) {
            for (i in 1:5) {
                if (abs(min(opt[["final_gradient"]])) > 0.01) {
                  obj <- MakeADFun(data = TmbList[["Data"]], 
                    parameters = ParList, random = TmbList[["Random"]], 
                    map = TmbList[["Map"]], inner.control = list(maxit = 1000), 
                    hessian = FALSE)
                  opt <- tryCatch(nlminb(start = obj$env$last.par.best[-obj$env$random] + 
                    rnorm(length(obj$par), 0, 0.2), objective = obj$fn, 
                    gradient = obj$gr, upper = Upr, lower = Lwr, 
                    control = list(trace = 1, eval.max = 10000, 
                      iter.max = 10000, rel.tol = 1e-10)), error = function(e) NA)
                  if (all(is.na(opt))) {
                    opt <- opt_save
                    obj <- obj_save
                  }
                  if (all(is.na(opt)) == FALSE) {
                    opt[["final_gradient"]] = obj$gr(opt$par)
                    opt_save <- opt
                    obj_save <- obj
                  }
                }
                if (abs(min(opt[["final_gradient"]])) <= 0.01) 
                  break
            }
            df <- data.frame(opt$final_gradient, names(obj$par), 
                opt$par)
        }
        if (bb == length(vec)) {
            if (all(is.na(opt))) 
                write("NAs final gradient", file.path(modpath, 
                  "NAs_final_gradient.txt"))
            if (all(is.na(opt) == FALSE)) 
                if (abs(min(opt[["final_gradient"]])) > 0.01) 
                  write(opt[["final_gradient"]], file.path(modpath, 
                    "high_final_gradient.txt"))
        }
        ParList <- obj$env$parList(x = obj$par, par = obj$env$last.par.best)
        Report = tryCatch(obj$report(), error = function(x) NA)
        if (bb == length(vec)) 
            saveRDS(Report, file.path(modpath, "Report.rds"))
        Sdreport = tryCatch(sdreport(obj), error = function(x) NA)
        if (bb == length(vec)) 
            saveRDS(Sdreport, file.path(modpath, "Sdreport.rds"))
        Derived = Calc_derived_quants(Obj = obj)
        if (bb == length(vec)) 
            saveRDS(Derived, file.path(modpath, "Derived_quants.rds"))
        dyn.unload(paste0(run_exe, "\\", dynlib(Version)))
    }
    write.csv(df, file.path(modpath, "df.csv"))
    rm(opt)
    rm(obj)
    rm(TmbList)
    rm(opt_save)
    rm(obj_save)
    return(df)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
