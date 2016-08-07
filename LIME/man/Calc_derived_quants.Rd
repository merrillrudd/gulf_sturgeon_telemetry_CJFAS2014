\name{Calc_derived_quants}
\alias{Calc_derived_quants}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Calc_derived_quants(Obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Obj}{
%%     ~~Describe \code{Obj} here~~
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
function (Obj) 
{
    Data = Obj$env$data
    ParHat = Obj$env$parList()
    Report = Obj$report()
    SPR <- with(Report, calc_ref(Mat_a = Mat_a, W_a = W_a, M = M, 
        S_a = S_a, F = F_t[length(F_t)], ref = FALSE))
    F30 <- with(Report, uniroot(calc_ref, lower = 0, upper = 50, 
        Mat_a = Mat_a, W_a = W_a, M = M, S_a = S_a, ref = 0.3)$root)
    F40 <- with(Report, uniroot(calc_ref, lower = 0, upper = 50, 
        Mat_a = Mat_a, W_a = W_a, M = M, S_a = S_a, ref = 0.4)$root)
    FF30 <- Report$F_t[length(Report$F_t)]/F30
    FF40 <- Report$F_t[length(Report$F_t)]/F40
    TB_t = as.vector(Report$W_a \%*\% t(Report$N_ta))
    Cw_t <- as.vector(Report$W_a \%*\% t(Report$Cn_ta))
    Yield_Fn = function(Fmean, Return_type = "Yield") {
        Data_new = Data
        Data_new[["n_t"]] = 1000
        Data_new[["n_c"]] = 1000
        Data_new[["n_i"]] = 1000
        Data_new[["n_lc"]] = 1000
        Data_new[["T_yrs"]] = 1:1000
        Data_new[["C_yrs"]] = 1:1000
        Data_new[["I_yrs"]] = 1:1000
        Data_new[["LC_yrs"]] = 1:1000
        Data_new[["obs_per_yr"]] = rep(Data[["obs_per_yr"]][1], 
            1000)
        Data_new[["RecDev_biasadj"]] = rep(0, Data_new[["n_t"]])
        Data_new[["C_t"]] = rep(1, Data_new[["n_t"]])
        names(Data_new[["C_t"]]) <- Data_new[["C_yrs"]]
        Data_new[["LF"]] = matrix(0, nrow = Data_new[["n_t"]], 
            ncol = Data_new[["n_lb"]])
        rownames(Data_new[["LF"]]) <- Data_new[["LC_yrs"]]
        Data_new[["I_t"]] = cbind(1, rep(1, Data_new[["n_t"]]))
        names(Data_new[["I_t"]]) <- Data_new[["I_yrs"]]
        ParHat_new = ParHat
        ParHat_new[["log_F_t_input"]] = rep(log(Fmean + 1e-10), 
            Data_new[["n_t"]])
        ParHat_new[["Nu_input"]] = rep(0, Data_new[["n_t"]])
        Obj_new = MakeADFun(data = Data_new[1:length(Data_new)], 
            parameters = ParHat_new[1:length(ParHat_new)], inner.control = list(maxit = 1000))
        Report_new = Obj_new$report()
        if (Return_type == "Yield") 
            Return = rev(Report_new$Cw_t_hat)[1]
        if (Return_type == "Report") 
            Return = Report_new
        return(Return)
    }
    Fmsy = optimize(f = Yield_Fn, interval = c(0, 3), maximum = TRUE)$maximum
    Report_msy = Yield_Fn(Fmean = Fmsy, Return_type = "Report")
    Report_0 = Yield_Fn(Fmean = 0, Return_type = "Report")
    TBmsy = rev(as.vector(Report$W_a \%*\% t(Report_msy$N_ta)))[1]
    SBmsy = rev(Report_msy$SB_t)[1]
    MSY = rev(Report_msy$Cw_t_hat)[1]
    TB0 = rev(as.vector(Report$W_a \%*\% t(Report_0$N_ta)))[1]
    SB0 = rev(Report_0$SB_t)[1]
    Return <- list(SPR = SPR, F30 = F30, F40 = F40, FF30 = FF30, 
        FF40 = FF40, Fmsy = Fmsy, SB0 = SB0, TB0 = TB0, TB_t = TB_t, 
        SB_t = Report$SB_t, MSY = MSY, TBmsy = TBmsy, SBmsy = SBmsy)
    return(Return)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
