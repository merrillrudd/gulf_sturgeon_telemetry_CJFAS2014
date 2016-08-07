\name{plotResults}
\alias{plotResults}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotResults(Data, Report, Sdreport, Derived_quants, flag_convergence, parameter, xaxt = TRUE, ylab = FALSE, simulation = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
%%     ~~Describe \code{Data} here~~
}
  \item{Report}{
%%     ~~Describe \code{Report} here~~
}
  \item{Sdreport}{
%%     ~~Describe \code{Sdreport} here~~
}
  \item{Derived_quants}{
%%     ~~Describe \code{Derived_quants} here~~
}
  \item{flag_convergence}{
%%     ~~Describe \code{flag_convergence} here~~
}
  \item{parameter}{
%%     ~~Describe \code{parameter} here~~
}
  \item{xaxt}{
%%     ~~Describe \code{xaxt} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{simulation}{
%%     ~~Describe \code{simulation} here~~
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
function (Data, Report, Sdreport, Derived_quants, flag_convergence, 
    parameter, xaxt = TRUE, ylab = FALSE, simulation = TRUE) 
{
    FUN = function(InputMat, log = TRUE) {
        if (log == TRUE) 
            return(c(exp(InputMat[, 1] - 1.96 * InputMat[, 2]), 
                rev(exp(InputMat[, 1] + 1.96 * InputMat[, 2]))))
        if (log == FALSE) 
            return(c(InputMat[, 1] - 1.96 * InputMat[, 2], rev(InputMat[, 
                1] + 1.96 * InputMat[, 2])))
    }
    if (simulation == TRUE) 
        years <- 1:length(Data$SB_t)
    if (simulation == FALSE) {
        years <- Data$years_i
        years_real <- Data$years
    }
    if (parameter == "B") {
        if (simulation == TRUE) 
            Mat <- cbind(Year = years, Data = Data$SB_t, Est = Report$SB_t_hat)
        if (simulation == FALSE) 
            Mat <- cbind(Year = years, Data = rep(NA, length(years)), 
                Est = Report$SB_t_hat)
        ymax <- 3
        matplot(y = Mat[, c("Data", "Est")], x = Mat[, c("Year")], 
            type = "l", col = c("black", "red"), lty = "solid", 
            ylim = c(0, ymax), lwd = 2, xaxt = "n")
        if (all(is.na(Sdreport)) == FALSE) 
            if (!("condition" \%in\% names(attributes(Sdreport)))) 
                polygon(y = FUN(summary(Sdreport)[which(rownames(summary(Sdreport)) == 
                  "lSB_t"), ]), x = c(Mat[, c("Year")], rev(Mat[, 
                  c("Year")])), col = rgb(1, 0, 0, alpha = 0.2), 
                  border = NA)
        mtext("Biomass", side = 3, line = -2, font = 2)
        if (ylab == TRUE) 
            mtext("Biomass", side = 2, line = 3, font = 2)
    }
    if (parameter == "N") {
        if (simulation == TRUE) 
            Mat <- cbind(Year = years, Data = Data$N_t, Est = Report$N_t_hat)
        if (simulation == FALSE) 
            Mat <- cbind(Year = years, Data = rep(NA, length(years)), 
                Est = Report$N_t_hat)
        ymax <- 4
        matplot(y = Mat[, c("Data", "Est")], x = Mat[, c("Year")], 
            type = "l", col = c("black", "red"), lty = "solid", 
            ylim = c(0, ymax), lwd = 2, xaxt = "n")
        if (all(is.na(Sdreport)) == FALSE) 
            if (!("condition" \%in\% names(attributes(Sdreport)))) 
                polygon(y = FUN(summary(Sdreport)[which(rownames(summary(Sdreport)) == 
                  "lN_t"), ]), x = c(Mat[, c("Year")], rev(Mat[, 
                  c("Year")])), col = rgb(1, 0, 0, alpha = 0.2), 
                  border = NA)
        mtext("Abundance", side = 3, line = -2, font = 2)
        if (ylab == TRUE) 
            mtext("Abundance", side = 2, line = 3, font = 2)
    }
    if (parameter == "ML") {
        tml <- Data$ML_t
        pml <- rep(NA, length(years))
        names(pml) <- years
        pml[which(names(pml) \%in\% names(Data$ML_t))] <- tml
        Mat <- cbind(Year = years, Data = pml, Est = Report$L_t_hat)
        ymax <- max(Report$L_t_hat) * 1
        matplot(y = Mat[, c("Data", "Est")], x = Mat[, c("Year")], 
            type = "l", col = c("black", "red"), lty = "solid", 
            ylim = c(0, ymax), lwd = 2, xaxt = "n")
        if (all(is.na(Sdreport)) == FALSE) 
            if (!("condition" \%in\% names(attributes(Sdreport)))) 
                polygon(y = FUN(summary(Sdreport)[which(rownames(summary(Sdreport)) == 
                  "L_t_hat"), ], log = FALSE), x = c(Mat[, c("Year")], 
                  rev(Mat[, c("Year")])), col = rgb(1, 0, 0, 
                  alpha = 0.2), border = NA)
        mtext("Mean Length", side = 3, line = -2, font = 2)
        if (ylab == TRUE) 
            mtext("Mean Length", side = 2, line = 3, font = 2)
    }
    if (parameter == "R") {
        if (simulation == TRUE) 
            Mat <- cbind(Year = years, Data = Data$R_t, Est = Report$R_t_hat)
        if (simulation == FALSE) 
            Mat <- cbind(Year = years, Data = rep(NA, length(years)), 
                Est = Report$R_t_hat)
        ymax <- 3
        matplot(y = Mat[, c("Data", "Est")], x = Mat[, c("Year")], 
            type = "l", col = c("black", "red"), lty = "solid", 
            ylim = c(0, ymax), lwd = 2, xaxt = "n")
        if (all(is.na(Sdreport)) == FALSE) 
            if (!("condition" \%in\% names(attributes(Sdreport)))) 
                polygon(y = FUN(summary(Sdreport)[which(rownames(summary(Sdreport)) == 
                  "lR_t"), ]), x = c(Mat[, c("Year")], rev(Mat[, 
                  c("Year")])), col = rgb(1, 0, 0, alpha = 0.2), 
                  border = NA)
        mtext("Recruitment", side = 3, line = -2, font = 2)
        if (ylab == TRUE) 
            mtext("Recruitment", side = 2, line = 3, font = 2)
    }
    if (parameter == "F") {
        NAs <- rep(NA, length(years))
        if (simulation == TRUE) 
            Mat <- cbind(Year = years, Data = Data$F_t, Est = Report$F_t_hat)
        if (simulation == FALSE) 
            Mat <- cbind(Year = years, Data = NAs, Est = Report$F_t)
        ymax <- 2
        matplot(y = Mat[, c("Data", "Est")], x = Mat[, c("Year")], 
            type = "l", col = c("black", "red"), lty = "solid", 
            ylim = c(0, ymax), lwd = 2, xaxt = "n")
        if (all(is.na(Sdreport)) == FALSE) 
            if (!("condition" \%in\% names(attributes(Sdreport)))) 
                polygon(y = FUN(summary(Sdreport)[which(rownames(summary(Sdreport)) == 
                  "lF_t"), ]), x = c(Mat[, c("Year")], rev(Mat[, 
                  c("Year")])), col = rgb(1, 0, 0, alpha = 0.2), 
                  border = NA)
        mtext("Fishing Mortality", side = 3, line = -2, font = 2)
        if (ylab == TRUE) 
            mtext("Fishing Mortality", side = 2, line = 3, font = 2)
    }
    if (parameter == "D") {
        if (simulation == TRUE) 
            Mat <- cbind(Year = years, Data = Data$D_t, Est = Report$Depl)
        if (simulation == FALSE) 
            Mat <- cbind(Year = years, Data = rep(NA, length(years)), 
                Est = Report$Depl)
        ymax <- 1
        matplot(y = Mat[, c("Data", "Est")], x = Mat[, c("Year")], 
            type = "l", col = c("black", "red"), lty = "solid", 
            ylim = c(0, ymax), lwd = 2, xaxt = "n")
        if (all(is.na(Sdreport)) == FALSE) 
            if (!("condition" \%in\% names(attributes(Sdreport)))) 
                polygon(y = FUN(summary(Sdreport)[which(rownames(summary(Sdreport)) == 
                  "lD_t"), ]), x = c(Mat[, c("Year")], rev(Mat[, 
                  c("Year")])), col = rgb(1, 0, 0, alpha = 0.2), 
                  border = NA)
        mtext("Relative abundance", side = 3, line = -2, font = 2)
        if (ylab == TRUE) 
            mtext("Relative abundance", side = 2, line = 3, font = 2)
    }
    if (parameter == "C") {
        tcatch <- Data$C_t
        pcatch <- rep(NA, length(years))
        names(pcatch) <- years
        pcatch[which(names(pcatch) \%in\% names(Data$C_t))] <- tcatch
        Mat <- cbind(Year = years, Data = pcatch, Est = Report$C_t_hat)
        ymax <- 5
        matplot(y = Mat[, c("Data", "Est")], x = Mat[, c("Year")], 
            type = "l", col = c("black", "red"), lty = "solid", 
            ylim = c(0, ymax), lwd = 2, xaxt = "n")
        if (all(is.na(Sdreport)) == FALSE) 
            if (!("condition" \%in\% names(attributes(Sdreport)))) 
                polygon(y = FUN(summary(Sdreport)[which(rownames(summary(Sdreport)) == 
                  "lC_t"), ]), x = c(Mat[, c("Year")], rev(Mat[, 
                  c("Year")])), col = rgb(1, 0, 0, alpha = 0.2), 
                  border = NA)
        mtext("Catch", side = 3, line = -2, font = 2)
        if (ylab == TRUE) 
            mtext("Catch", side = 2, line = 3, font = 2)
    }
    if (parameter == "I") {
        tindex <- Data$I_t
        pindex <- rep(NA, length(years))
        names(pindex) <- years
        pindex[which(names(pindex) \%in\% names(Data$I_t))] <- tindex
        Mat <- cbind(Year = years, Data = pindex, Est = Report$I_t_hat)
        ymax <- 3
        matplot(y = Mat[, c("Data", "Est")], x = Mat[, c("Year")], 
            type = "l", col = c("black", "red"), lty = "solid", 
            ylim = c(0, ymax), lwd = 2, xaxt = "n")
        if (all(is.na(Sdreport)) == FALSE) 
            if (!("condition" \%in\% names(attributes(Sdreport)))) 
                polygon(y = FUN(summary(Sdreport)[which(rownames(summary(Sdreport)) == 
                  "lI_t"), ]), x = c(Mat[, c("Year")], rev(Mat[, 
                  c("Year")])), col = rgb(1, 0, 0, alpha = 0.2), 
                  border = NA)
        mtext("Index", side = 3, line = -2, font = 2)
        if (ylab == TRUE) 
            mtext("Index", side = 2, line = 3, font = 2)
    }
    if (xaxt == TRUE) {
        axis(1, at = seq(1, 20, by = 5), labels = years_real[seq(1, 
            20, by = 5)])
        mtext(side = 1, "Year", line = 4)
    }
    if (parameter == "kobe") {
        plot(x = 1, y = 1, type = "n", xaxs = "i", yaxs = "i", 
            xlim = c(0, 1), ylim = c(0, 3), xaxt = "n", yaxt = "n")
        abline(h = 1, lty = 2, lwd = 2)
        abline(v = 0.3, lty = 2, col = "forestgreen", lwd = 2)
        abline(v = 0.4, lty = 2, col = "steelblue", lwd = 2)
        points(x = Derived_quants$SPR, y = Derived_quants$FF30, 
            col = "forestgreen", pch = 19, cex = 2)
        points(x = Derived_quants$SPR, y = Derived_quants$FF40, 
            col = "steelblue", pch = 19, cex = 2)
        axis(1, at = seq(0, 1, by = 0.2))
        axis(2, at = seq(0, 3, by = 0.2))
        mtext(side = 1, "SPR", line = 2)
        mtext(side = 2, "F/Fref", line = 2)
        legend("topright", col = c("forestgreen", "steelblue"), 
            legend = c("30\%", "40\%"), title = "Target SPR", pch = 19)
    }
    if (flag == TRUE) 
        mtext(side = 3, "model not converged", col = "red", font = 2, 
            outer = TRUE, line = 2, cex = 2)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
