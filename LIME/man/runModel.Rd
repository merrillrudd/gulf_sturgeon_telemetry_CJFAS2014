\name{runModel}
\alias{runModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
runModel(modpath, itervec, estimate_same = FALSE, REML = FALSE, est_sigma, biascorrect = TRUE, data_avail, lh_list, sensitivity_inputs = NULL, sensitivity_ESS = NULL, rewrite, start_f)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{modpath}{
%%     ~~Describe \code{modpath} here~~
}
  \item{itervec}{
%%     ~~Describe \code{itervec} here~~
}
  \item{estimate_same}{
%%     ~~Describe \code{estimate_same} here~~
}
  \item{REML}{
%%     ~~Describe \code{REML} here~~
}
  \item{est_sigma}{
%%     ~~Describe \code{est_sigma} here~~
}
  \item{biascorrect}{
%%     ~~Describe \code{biascorrect} here~~
}
  \item{data_avail}{
%%     ~~Describe \code{data_avail} here~~
}
  \item{lh_list}{
%%     ~~Describe \code{lh_list} here~~
}
  \item{sensitivity_inputs}{
%%     ~~Describe \code{sensitivity_inputs} here~~
}
  \item{sensitivity_ESS}{
%%     ~~Describe \code{sensitivity_ESS} here~~
}
  \item{rewrite}{
%%     ~~Describe \code{rewrite} here~~
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
function (modpath, itervec, estimate_same = FALSE, REML = FALSE, 
    est_sigma, biascorrect = TRUE, data_avail, lh_list, sensitivity_inputs = NULL, 
    sensitivity_ESS = NULL, rewrite, start_f) 
{
    lh_num <- ifelse(grepl("LH1", modpath), 1, ifelse(grepl("LH2", 
        modpath), 2, ifelse(grepl("LH3", modpath), 3, ifelse(grepl("LH4", 
        modpath), 4, stop("No match to life history number")))))
    lh_choose <- lh_list[[lh_num]]
    for (iter in itervec) {
        iterpath <- file.path(modpath, iter)
        if (rewrite == FALSE) {
            if (file.exists(file.path(iterpath, "Derived_quants.rds"))) 
                next
            if (any(grepl("LBSPR_results", list.files(path = iterpath)))) 
                next
            if (file.exists(file.path(iterpath, "NAs_final_gradient.txt"))) 
                next
            if (file.exists(file.path(iterpath, "high_final_gradient.txt"))) 
                next
        }
        DataList <- readRDS(file.path(iterpath, "True.rds"))
        modname <- DataList$DataScenario
        if (grepl("MixedEffects", modname)) 
            modname <- strsplit(modname, "_")[[1]][2]
        if (grepl("LBSPR", modname)) 
            modname <- strsplit(modname, "_")[[1]][2]
        if (is.null(sensitivity_inputs)) {
            param <- FALSE
            val <- FALSE
        }
        if (is.null(sensitivity_inputs) == FALSE) {
            param_set <- c("M", "linf", "vbk", "CVlen", "SigmaR")
            param_set_input <- paste0("sens_", param_set)
            param <- param_set[which(sapply(1:length(param_set), 
                function(x) grepl(param_set_input[x], modpath)))]
            val_index <- ifelse(grepl("Low", modpath), 1, ifelse(grepl("High", 
                modpath), 2, stop("Not set up for specified level of sensitivity")))
            val <- as.numeric(sensitivity_inputs[[param]][val_index, 
                lh_num])
        }
        inits <- create_inputs(lh_list = lh_choose, data_avail_list = data_avail[[modname]], 
            param = param, val = val)
        Nyears <- inits$Nyears
        if (grepl("LBSPR", modpath) == TRUE) {
            if (file.exists(file.path(iterpath, "LBSPR_results.rds"))) 
                next
            Nyears_comp <- as.numeric(strsplit(modname, "C")[[1]][2])
            lbspr <- runLBSPR(Nyears_comp = Nyears_comp, inits = inits, 
                iterpath = iterpath, DataList = DataList, species = as.character(lh_num))
        }
        if (grepl("LBSPR", modpath) == FALSE) {
            if (biascorrect == FALSE) 
                vec <- 1
            if (biascorrect == TRUE) 
                vec <- 1:2
            Sdreport <- NA
            ParList <- NA
            df <- NULL
            for (bb in vec) {
                if (all(is.na(Sdreport))) 
                  RecDev_biasadj <- rep(0, Nyears)
                if (all(is.na(Sdreport)) == FALSE) {
                  SD <- summary(Sdreport)
                  RecDev_biasadj <- 1 - SD[which(rownames(SD) == 
                    "Nu_input"), "Std. Error"]^2/Report$sigma_R^2
                }
                if (all(is.na(RecDev_biasadj))) 
                  RecDev_biasadj <- rep(0, Nyears)
                TmbList <- FormatInput_LB(Nyears = Nyears, DataList = DataList, 
                  linf = inits$linf, vbk = inits$vbk, t0 = inits$t0, 
                  M = inits$M, AgeMax = inits$AgeMax, lbhighs = inits$highs, 
                  lbmids = inits$mids, Mat_a = inits$Mat_a, lwa = inits$lwa, 
                  lwb = inits$lwb, log_sigma_C = inits$log_sigma_C, 
                  log_sigma_I = inits$log_sigma_I, log_CV_L = inits$log_CV_L, 
                  F1 = inits$F1, SigmaR = inits$SigmaR, qcoef = inits$qcoef, 
                  R0 = inits$R0, S50 = inits$S50, S95 = inits$S95, 
                  version = Version, model = as.character(modname), 
                  RecDev_biasadj = RecDev_biasadj, SigmaF = inits$SigmaF, 
                  Fpen = 1, Dpen = 0, Dprior = c(0.8, 0.2), SigRpen = 1, 
                  SigRprior = c(inits$SigmaR, 0.2), obs_per_yr = inits$obs_per_yr, 
                  RecType = 0, FType = 0, LType = 1, h = inits$h, 
                  SelexTypeDesc = "asymptotic", est_sigma = est_sigma, 
                  REML = REML, site = 1, estimate_same = FALSE, 
                  start_f = start_f)
                if (bb == 1) 
                  saveRDS(TmbList, file.path(iterpath, "Inputs1.rds"))
                if (bb == 2) 
                  saveRDS(TmbList, file.path(iterpath, "Inputs2.rds"))
                dyn.load(paste0(run_exe, "\\", dynlib(Version)))
                if (all(is.na(ParList))) 
                  ParList <- TmbList[["Parameters"]]
                if (bb == 1) {
                  obj_save <- NULL
                  jnll <- NULL
                  opt_save <- NULL
                  opt_save[["final_gradient"]] <- NA
                }
                obj <- MakeADFun(data = TmbList[["Data"]], parameters = ParList, 
                  random = TmbList[["Random"]], map = TmbList[["Map"]], 
                  inner.control = list(maxit = 1000), hessian = FALSE)
                obj$env$inner.control <- c(obj$env$inner.control, 
                  step.tol = c(1e-08, 1e-12)[1], tol10 = c(1e-06, 
                    1e-08)[1], grad.tol = c(1e-08, 1e-12)[1])
                Upr = rep(Inf, length(obj$par))
                Upr[match("log_sigma_R", names(obj$par))] = log(2)
                Upr[match("logS95", names(obj$par))] = log(inits$AgeMax)
                Upr[match("log50", names(obj$par))] = obj$par[match("logS95", 
                  names(obj$par))]
                Upr[which(names(obj$par) == "log_F_t_input")] = log(5)
                Upr[match("log_F_sd", names(obj$par))] <- log(2)
                Lwr <- rep(-Inf, length(obj$par))
                opt <- tryCatch(nlminb(start = obj$par, objective = obj$fn, 
                  gradient = obj$gr, upper = Upr, lower = Lwr, 
                  control = list(trace = 1, eval.max = 10000, 
                    iter.max = 10000, rel.tol = 1e-10)), error = function(e) NA)
                jnll <- obj$report()$jnll
                if (all(is.na(opt)) == FALSE) {
                  opt[["final_gradient"]] = obj$gr(opt$par)
                  opt_save <- opt
                  obj_save <- obj
                  jnll_save <- obj_save$report()$jnll
                }
                for (i in 1:10) {
                  if (all(is.na(opt))) {
                    obj <- MakeADFun(data = TmbList[["Data"]], 
                      parameters = ParList, random = TmbList[["Random"]], 
                      map = TmbList[["Map"]], inner.control = list(maxit = 1000), 
                      hessian = FALSE)
                    obj$env$inner.control <- c(obj$env$inner.control, 
                      step.tol = c(1e-08, 1e-12)[1], tol10 = c(1e-06, 
                        1e-08)[1], grad.tol = c(1e-08, 1e-12)[1])
                    opt <- tryCatch(nlminb(start = obj$env$last.par.best[-obj$env$random] + 
                      rnorm(length(obj$par), 0, 0.2), objective = obj$fn, 
                      gradient = obj$gr, upper = Upr, lower = Lwr, 
                      control = list(trace = 1, eval.max = 10000, 
                        iter.max = 10000, rel.tol = 1e-10)), 
                      error = function(e) NA)
                    jnll <- obj$report()$jnll
                  }
                  if (all(is.na(opt)) == FALSE) {
                    opt[["final_gradient"]] = obj$gr(opt$par)
                    opt_save <- opt
                    obj_save <- obj
                    jnll_save <- jnll
                    break
                  }
                }
                if (all(is.na(opt_save)) == FALSE) {
                  for (i in 1:5) {
                    if (all(is.na(opt_save[["final_gradient"]]) == 
                      FALSE)) {
                      if (abs(min(opt_save[["final_gradient"]])) > 
                        0.01) {
                        obj <- MakeADFun(data = TmbList[["Data"]], 
                          parameters = ParList, random = TmbList[["Random"]], 
                          map = TmbList[["Map"]], inner.control = list(maxit = 1000), 
                          hessian = FALSE)
                        obj$env$inner.control <- c(obj$env$inner.control, 
                          step.tol = c(1e-08, 1e-12)[1], tol10 = c(1e-06, 
                            1e-08)[1], grad.tol = c(1e-08, 1e-12)[1])
                        opt <- tryCatch(nlminb(start = obj$env$last.par.best[-obj$env$random] + 
                          rnorm(length(obj$par), 0, 0.2), objective = obj$fn, 
                          gradient = obj$gr, upper = Upr, lower = Lwr, 
                          control = list(trace = 1, eval.max = 10000, 
                            iter.max = 10000, rel.tol = 1e-10)), 
                          error = function(e) NA)
                        jnll <- obj$report()$jnll
                      }
                    }
                    if (all(is.na(opt)) == FALSE & jnll <= jnll_save) {
                      opt[["final_gradient"]] = obj$gr(opt$par)
                      opt_save <- opt
                      obj_save <- obj
                      jnll_save <- jnll
                    }
                    if (abs(min(opt_save[["final_gradient"]])) <= 
                      0.01) 
                      break
                  }
                }
                if (all(is.na(opt_save)) == FALSE) 
                  df <- data.frame(opt_save$final_gradient, names(obj_save$par), 
                    opt_save$par)
                if (bb == length(vec)) {
                  if (all(is.null(opt_save))) 
                    write("NAs final gradient", file.path(iterpath, 
                      "NAs_final_gradient.txt"))
                  if (all(is.null(opt_save) == FALSE)) 
                    if (abs(min(opt_save[["final_gradient"]])) > 
                      0.01) 
                      write(opt_save[["final_gradient"]], file.path(iterpath, 
                        "high_final_gradient.txt"))
                }
                ParList <- obj_save$env$parList(x = obj_save$par, 
                  par = obj_save$env$last.par.best)
                Report = tryCatch(obj_save$report(), error = function(x) NA)
                if (bb == length(vec)) 
                  saveRDS(Report, file.path(iterpath, "Report.rds"))
                Sdreport = tryCatch(sdreport(obj_save), error = function(x) NA)
                if (bb == length(vec)) 
                  saveRDS(Sdreport, file.path(iterpath, "Sdreport.rds"))
                if (bb == length(vec)) {
                  Derived = Calc_derived_quants(Obj = obj_save)
                  if (bb == length(vec)) 
                    saveRDS(Derived, file.path(iterpath, "Derived_quants.rds"))
                }
            }
            if (iter == 1) 
                write.csv(df, file.path(modpath, "df.csv"))
            rm(Report)
            rm(Sdreport)
            rm(TmbList)
            rm(opt)
            rm(obj)
            rm(df)
            rm(opt_save)
            rm(obj_save)
        }
    }
    return(paste0(max(itervec), " iterates run in ", modpath))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
