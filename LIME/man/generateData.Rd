\name{generateData}
\alias{generateData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
generateData(modpath, modname, itervec, spatial, Fdynamics, Rdynamics, LType = 1, plotML = FALSE, plotLF_compare = FALSE, plotLF = FALSE, selex = "asymptotic", write = TRUE, lh_list, data_avail_list)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{modpath}{
%%     ~~Describe \code{modpath} here~~
}
  \item{modname}{
%%     ~~Describe \code{modname} here~~
}
  \item{itervec}{
%%     ~~Describe \code{itervec} here~~
}
  \item{spatial}{
%%     ~~Describe \code{spatial} here~~
}
  \item{Fdynamics}{
%%     ~~Describe \code{Fdynamics} here~~
}
  \item{Rdynamics}{
%%     ~~Describe \code{Rdynamics} here~~
}
  \item{LType}{
%%     ~~Describe \code{LType} here~~
}
  \item{plotML}{
%%     ~~Describe \code{plotML} here~~
}
  \item{plotLF_compare}{
%%     ~~Describe \code{plotLF_compare} here~~
}
  \item{plotLF}{
%%     ~~Describe \code{plotLF} here~~
}
  \item{selex}{
%%     ~~Describe \code{selex} here~~
}
  \item{write}{
%%     ~~Describe \code{write} here~~
}
  \item{lh_list}{
%%     ~~Describe \code{lh_list} here~~
}
  \item{data_avail_list}{
%%     ~~Describe \code{data_avail_list} here~~
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
function (modpath, modname, itervec, spatial, Fdynamics, Rdynamics, 
    LType = 1, plotML = FALSE, plotLF_compare = FALSE, plotLF = FALSE, 
    selex = "asymptotic", write = TRUE, lh_list, data_avail_list) 
{
    lh_num <- ifelse(grepl("LH1", modpath), 1, ifelse(grepl("LH2", 
        modpath), 2, ifelse(grepl("LH3", modpath), 3, ifelse(grepl("LH4", 
        modpath), 4, stop("No match to life history number")))))
    lh_choose <- lh_list[[lh_num]]
    Nyears_comp <- data_avail_list$Nyears_comp
    Nyears <- data_avail_list$Nyears
    for (iter in itervec) {
        if (write == TRUE) {
            iterpath <- file.path(modpath, iter)
            dir.create(iterpath, showWarnings = FALSE)
        }
        DataList <- with(c(lh_choose, data_avail_list), SimData_LB(Nyears = Nyears, 
            AgeMax = AgeMax, M = M, F1 = F1, h = h, S_a = S_a, 
            qcoef = qcoef, Frate = Frate, Fequil = Fequil, SigmaF = SigmaF, 
            Fdynamics = Fdynamics, Rdynamics = Rdynamics, R0 = R0, 
            Fmax = Fmax, CVlen = CVlen, mids = mids, highs = highs, 
            lows = lows, W_a = W_a, L_a = L_a, Mat_a = Mat_a, 
            Amat = Amat, comp_sample = comp_sample, SigmaR = SigmaR, 
            Nyears_comp = Nyears_comp, alt_yrs = FALSE, sample = FALSE, 
            nburn = 20, seed = iter, modname = modname))
        if (spatial == TRUE) {
            set.seed(max(itervec) + iter)
            spatial_sim <- spatialgrowth_sim(n_i = 20, linf = lh_choose$linf)
            if (write == TRUE) 
                saveRDS(spatial_sim, file.path(iterpath, "spatial_sim.rds"))
            lh_spatial <- lapply(1:nrow(spatial_sim), function(x) create_lh_list(lh = lh_num, 
                param_adjust = c("linf", "ML50"), val = c(spatial_sim[x, 
                  "linf_i"], lh_choose$ML50 * (spatial_sim[x, 
                  "linf_i"]/lh_choose$linf)), selex = "asymptotic", 
                nlbins = 60))
            DataList_site <- lapply(1:length(lh_spatial), function(x) with(c(lh_spatial[[x]], 
                data_avail_list), SimData_LB(Nyears = Nyears, 
                AgeMax = AgeMax, M = M, F1 = F1, h = h, S_a = S_a, 
                qcoef = qcoef, Frate = Frate, Fequil = Fequil, 
                SigmaF = SigmaF, Fdynamics = Fdynamics, Rdynamics = Rdynamics, 
                R0 = R0, Fmax = Fmax, CVlen = CVlen, mids = mids, 
                highs = highs, lows = lows, W_a = W_a, L_a = L_a, 
                Mat_a = Mat_a, Amat = Amat, comp_sample = comp_sample, 
                SigmaR = SigmaR, Nyears_comp = Nyears_comp, alt_yrs = FALSE, 
                sample = FALSE, nburn = 20, seed = iter, modname = modname)))
            SPR_site <- sapply(1:length(DataList_site), function(x) DataList_site[[x]]$SPR_t)
            RelAbund_site <- sapply(1:length(DataList_site), 
                function(x) DataList_site[[x]]$D_t[length(DataList_site[[x]]$D_t)])
            if (write == TRUE) 
                saveRDS(SPR_site, file.path(iterpath, "SPR_site.rds"))
            if (Nyears_comp > 1) 
                LF_site <- lapply(1:length(DataList_site), function(x) DataList_site[[x]]$LF)
            if (Nyears_comp == 1) 
                LF_site <- lapply(1:length(DataList_site), function(x) as.matrix(DataList_site[[x]]$LF))
            ncols_site <- sapply(1:length(LF_site), function(x) ncol(LF_site[[x]]))
            for (i in 1:length(LF_site)) {
                ncol <- ncol(LF_site[[i]])
                if (ncol < max(ncols_site)) {
                  add <- max(ncols_site) - ncol
                  LF_site[[i]] <- cbind(LF_site[[i]], matrix(0, 
                    nrow = nrow(LF_site[[i]]), ncol = add))
                }
            }
            LF_site_array <- array(NA, dim = c(dim(LF_site[[1]]), 
                length(LF_site)))
            ML_t_site <- matrix(NA, nrow = nrow(LF_site[[1]]), 
                ncol = length(LF_site))
            for (i in 1:length(LF_site)) {
                LF_site_array[, , i] <- as.matrix(LF_site[[i]])
                if (Nyears_comp > 1) 
                  ML_t_site[, i] <- sapply(1:nrow(LF_site_array[, 
                    , i]), function(x) sum(LF_site[[i]][x, ] * 
                    1:ncol(LF_site[[i]]))/sum(LF_site[[i]][x, 
                    ]))
                if (Nyears_comp == 1) 
                  ML_t_site[, i] <- sum(LF_site[[i]] * 1:length(LF_site[[i]]))/sum(LF_site[[i]])
            }
            rownames(LF_site_array) <- rownames(ML_t_site) <- (Nyears - 
                Nyears_comp + 1):Nyears
            LF_pool <- matrix(NA, nrow = nrow(LF_site[[1]]), 
                ncol = max(ncols_site))
            for (i in 1:nrow(LF_site[[1]])) {
                for (j in 1:ncol(LF_site[[1]])) {
                  LF_pool[i, j] <- sum(sapply(1:length(LF_site), 
                    function(x) LF_site[[x]][i, j]))
                }
            }
            rownames(LF_pool) <- (Nyears - Nyears_comp + 1):Nyears
            if (plotLF == TRUE) {
                par(mfrow = c(4, 4), mar = c(0, 0, 0, 0), omi = c(1, 
                  1, 1, 1))
                barplot(DataList$LF[nrow(DataList$LF), ]/sum(DataList$LF[nrow(DataList$LF), 
                  ]), axes = F, xlim = c(0, 45), ylim = c(0, 
                  0.2), col = "tomato")
                mtext(side = 3, "no spatial process", font = 2, 
                  line = -3, cex = 2)
                axis(2, at = pretty(c(0, 0.2)))
                for (i in 1:length(LF_site)) {
                  barplot(LF_site[[i]][nrow(LF_site[[i]]), ]/sum(LF_site[[i]][nrow(LF_site[[i]]), 
                    ]), axes = F, xlim = c(0, 45), ylim = c(0, 
                    0.2))
                  mtext(paste0("site ", i), side = 3, font = 2, 
                    line = -3, cex = 2)
                  if (i \%in\% 12:15) 
                    axis(1, at = pretty(c(0, 45)))
                  if (i\%\%4 == 0) 
                    axis(2, at = pretty(c(0, 0.2)))
                }
                mtext(side = 1, "Length bin (1 cm)", outer = TRUE, 
                  line = 3)
                mtext(side = 2, "Proportion", outer = TRUE, line = 3)
            }
            if (plotLF_compare == TRUE) {
                par(mfrow = c(2, 1), mar = c(0, 0, 0, 0), omi = c(1, 
                  1, 1, 1))
                barplot(DataList$LF[nrow(DataList$LF), ]/sum(DataList$LF[nrow(DataList$LF), 
                  ]), axes = F, xlim = c(0, 45), ylim = c(0, 
                  0.2), col = "tomato")
                mtext(side = 3, "no spatial process", font = 2, 
                  line = -3, cex = 2)
                axis(2, at = pretty(c(0, 0.2)))
                barplot(LF_pool[nrow(LF_pool), ]/sum(LF_pool[nrow(LF_pool), 
                  ]), axes = F, xlim = c(0, 45), ylim = c(0, 
                  0.2), col = "black")
                mtext(side = 3, "spatial process pooled", font = 2, 
                  line = -3, cex = 2)
                axis(1, at = pretty(c(0, 45)))
                axis(2, at = pretty(c(0, 0.2)))
                mtext(side = 1, "Length bin (1 cm)", outer = TRUE, 
                  line = 3)
                mtext(side = 2, "Proportion", outer = TRUE, line = 3)
            }
            ML_t_pool <- sapply(1:nrow(LF_pool), function(x) sum(LF_pool[x, 
                ] * 1:ncol(LF_pool))/sum(LF_pool[x, ]))
            if (plotML == TRUE) {
                par(mfrow = c(4, 4), mar = c(0, 0, 0, 0), omi = c(1, 
                  1, 1, 1))
                plot(ML_t_pool, col = "red", type = "o", pch = 17, 
                  lwd = 2, ylim = c(0, linf), xaxt = "n", yaxt = "n")
                axis(2, cex = 1.2, las = 2)
                mtext(side = 3, "pooled", font = 2, line = -1.5)
                for (i in 1:length(DataList_site)) {
                  plot(DataList_site[[i]]$ML_t, col = "black", 
                    pch = 19, lwd = 2, ylim = c(0, linf), xaxt = "n", 
                    yaxt = "n")
                  lines(ML_t_pool, col = "red", pch = 17, type = "o")
                  if (i \%in\% c(12:15)) 
                    axis(1, cex = 1.2)
                  if (i \%in\% c(4, 8, 12)) 
                    axis(2, cex = 1.2, las = 2)
                  mtext(paste0("site ", i), side = 3, font = 2, 
                    line = -1.5)
                }
                legend("bottomright", legend = c("site-specific", 
                  "pooled"), pch = c(19, 17), col = c("black", 
                  "red"))
                mtext("Year", outer = TRUE, line = 3, side = 1)
                mtext("Mean length in catch (cm)", outer = TRUE, 
                  line = 3, side = 2)
            }
            if (LType == 1) {
                DataList$LF <- LF_pool
                DataList$ML_t <- ML_t_pool
            }
            if (LType == 0) {
                DataList$LF <- LF_site_array
                DataList$ML_t <- ML_t_site
            }
            rm(spatial_sim)
            rm(lh_spatial)
        }
        DataList_out <- DataList
        if (write == TRUE) 
            saveRDS(DataList_out, file.path(iterpath, "True.rds"))
        if (write == FALSE) 
            return(DataList_out)
        rm(DataList)
        rm(DataList_out)
        rm(iterpath)
    }
    if (write == TRUE) 
        return(paste0(length(itervec), " iterates of data generated in ", 
            modpath))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
