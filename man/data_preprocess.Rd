\name{data_preprocess}
\alias{data_preprocess}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
data_preprocess(XT, XS, YT, miss.cutoff = 0.995, num.min.lvs = 7, rare.cutoff = ceiling(nrow(XT) * 0.01), cat.trans = "None", num.trans = "None", num.miss = "None")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{XT}{
%%     ~~Describe \code{XT} here~~
}
  \item{XS}{
%%     ~~Describe \code{XS} here~~
}
  \item{YT}{
%%     ~~Describe \code{YT} here~~
}
  \item{miss.cutoff}{
%%     ~~Describe \code{miss.cutoff} here~~
}
  \item{num.min.lvs}{
%%     ~~Describe \code{num.min.lvs} here~~
}
  \item{rare.cutoff}{
%%     ~~Describe \code{rare.cutoff} here~~
}
  \item{cat.trans}{
%%     ~~Describe \code{cat.trans} here~~
}
  \item{num.trans}{
%%     ~~Describe \code{num.trans} here~~
}
  \item{num.miss}{
%%     ~~Describe \code{num.miss} here~~
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
function (XT, XS, YT, miss.cutoff = 0.995, num.min.lvs = 7, rare.cutoff = ceiling(nrow(XT) * 
    0.01), cat.trans = "None", num.trans = "None", num.miss = "None") 
{
    require(data.table)
    all.missing <- names(which(sapply(XT, function(x) length(which(is.na(x)))) > 
        nrow(XT) * miss.cutoff))
    if (length(all.missing) > 0) {
        XT <- XT[, setdiff(names(XT), all.missing), with = FALSE]
        XS <- XS[, setdiff(names(XS), all.missing), with = FALSE]
    }
    cat("\nStep 01: removed", length(all.missing), "features (missing values)\n")
    cat.feats <- names(which(sapply(XT, class) == "character"))
    num.feats <- setdiff(names(XT), cat.feats)
    num.minlvs.feats <- names(which(sapply(XT[, num.feats, with = FALSE], 
        function(x) length(unique(x))) < num.min.lvs))
    if (length(num.minlvs.feats) > 0) {
        for (j in num.minlvs.feats) {
            set(XT, i = NULL, j, as.character(XT[[j]]))
            set(XS, i = NULL, j, as.character(XS[[j]]))
        }
    }
    cat("\nStep 02: coerced", length(num.minlvs.feats), "numerical features in categorical ones (low unique values)\n")
    cat.feats <- union(cat.feats, num.minlvs.feats)
    num.feats <- setdiff(num.feats, num.minlvs.feats)
    num.low.presence <- names(which(sapply(XT[, num.feats, with = FALSE], 
        function(x) length(which(!is.na(x)))/nrow(XT)) < 2 * 
        rare.cutoff/nrow(XT)))
    if (length(num.low.presence) > 0) {
        for (j in num.low.presence) {
            XT.median.cutoff <- median(XT[[j]], na.rm = TRUE)
            new.j <- rep(NA, length(XT[[j]]))
            val.j <- rep(NA, length(XS[[j]]))
            new.j[which(XT[[j]] <= XT.median.cutoff)] <- "LOW"
            new.j[which(XT[[j]] > XT.median.cutoff)] <- "HIGH"
            val.j[which(XS[[j]] <= XT.median.cutoff)] <- "LOW"
            val.j[which(XS[[j]] > XT.median.cutoff)] <- "HIGH"
            set(XT, i = NULL, j, new.j)
            set(XS, i = NULL, j, val.j)
        }
    }
    cat("\nStep 03: coerced", length(num.low.presence), "numerical features in categorical ones (missing values)\n")
    cat.feats <- union(cat.feats, num.low.presence)
    num.feats <- setdiff(num.feats, num.low.presence)
    for (j in cat.feats) {
        XT.rare.cat.feat <- names(which(table(XT[[j]]) < rare.cutoff))
        if (length(XT.rare.cat.feat) > 0 & (!j \%in\% num.low.presence)) {
            set(XT, which(XT[[j]] \%in\% XT.rare.cat.feat), j, 
                "RARE")
            set(XS, which(XS[[j]] \%in\% XT.rare.cat.feat), j, 
                "RARE")
        }
        XT.miss.cat.ids <- which(is.na(XT[[j]]))
        XT.rare.cat.ids <- which(XT[[j]] == "RARE")
        XS.miss.cat.ids <- which(is.na(XS[[j]]))
        XS.rare.cat.ids <- which(XS[[j]] == "RARE")
        if (length(XT.rare.cat.ids) + length(XT.miss.cat.ids) > 
            0) {
            if ((length(XT.rare.cat.ids) > rare.cutoff & length(XT.miss.cat.ids) > 
                rare.cutoff)) {
                set(XT, XT.miss.cat.ids, j, "MISS")
                set(XS, XS.miss.cat.ids, j, "MISS")
            }
            else {
                set(XT, XT.miss.cat.ids, j, "RARE")
                set(XS, XS.miss.cat.ids, j, "RARE")
            }
        }
        XS.levels <- unique(XS[[j]])
        XT.levels <- unique(XT[[j]])
        XS.levels.diff <- setdiff(XS.levels, XT.levels)
        if (length(XS.levels.diff) > 0) {
            set(XS, which(XS[[j]] \%in\% XS.levels.diff), j, "RARE")
        }
    }
    cat.noinfo <- names(which(sapply(XT[, cat.feats, with = FALSE], 
        function(x) length(unique(x))) == 1))
    if (length(cat.noinfo) > 0) {
        XT <- XT[, setdiff(names(XT), cat.noinfo), with = FALSE]
        XS <- XS[, setdiff(names(XS), cat.noinfo), with = FALSE]
    }
    cat("\nStep 04: removed", length(cat.noinfo), "categorical features (non informative)\n")
    cat.feats <- union(cat.feats, num.minlvs.feats)
    if (cat.trans == "HotEncode") {
        dummy.feats <- c()
        for (j in cat.feats) {
            inds <- unique(XT[[j]])
            dummy.feats <- c(dummy.feats, paste0(j, ".", inds))
            XT[, `:=`((paste0(j, ".", inds)), lapply(inds, function(x) (XT[[j]] == 
                x) + 0))]
            XS[, `:=`((paste0(j, ".", inds)), lapply(inds, function(x) (XS[[j]] == 
                x) + 0))]
        }
        noinfo.dummy.feats <- c(names(which(sapply(XT[, dummy.feats, 
            with = FALSE], sum) < rare.cutoff)), names(which(sapply(XT[, 
            dummy.feats, with = FALSE], sum) == nrow(XT))))
        XT <- XT[, setdiff(names(XT), c(cat.feats, noinfo.dummy.feats)), 
            with = FALSE]
        XS <- XS[, setdiff(names(XS), c(cat.feats, noinfo.dummy.feats)), 
            with = FALSE]
    }
    if (cat.trans == "RelRisk") {
        for (j in cat.feats) {
            inds <- unique(XT[[j]])
            new.j <- as.numeric(rep(NA, length(XT[[j]])))
            val.j <- as.numeric(rep(NA, length(XS[[j]])))
            for (i in inds) {
                if (length(which(XT[[j]] == i)) > rare.cutoff) {
                  YT.rel.risk <- mean(YT[which(XT[[j]] == i)])
                  new.j[which(XT[[j]] == i)] <- YT.rel.risk
                  val.j[which(XS[[j]] == i)] <- YT.rel.risk
                }
            }
            new.j[is.na(new.j)] <- mean(YT)
            val.j[is.na(val.j)] <- mean(YT)
            set(XT, i = NULL, j, new.j)
            set(XS, i = NULL, j, val.j)
        }
    }
    cat.feats <- names(which(sapply(XT, class) == "character"))
    num.feats <- setdiff(names(XT), cat.feats)
    if (length(cat.feats) > 0) {
        for (j in cat.feats) {
            set(XT, i = NULL, j, as.factor(XT[[j]]))
            set(XS, i = NULL, j, factor(XS[[j]], levels = levels(XT[[j]])))
        }
    }
    cat("\nStep 05: coerced", length(cat.feats), "categorical features in factors\n")
    summary(XT)
    if (num.miss == "Mean") {
        for (j in num.feats) {
            set(XT, which(is.na(XT[[j]])), j, mean(XT[, XT[[j]]], 
                na.rm = TRUE))
            set(XS, which(is.na(XS[[j]])), j, mean(XT[, XT[[j]]], 
                na.rm = TRUE))
        }
    }
    if (num.miss == "Median") {
        for (j in num.feats) {
            set(XT, which(is.na(XT[[j]])), j, median(XT[, XT[[j]]], 
                na.rm = TRUE))
            set(XS, which(is.na(XS[[j]])), j, median(XT[, XT[[j]]], 
                na.rm = TRUE))
        }
    }
    if (num.miss == "Zero") {
        for (j in num.feats) {
            set(XT, which(is.na(XT[[j]])), j, 0)
            set(XS, which(is.na(XS[[j]])), j, 0)
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
