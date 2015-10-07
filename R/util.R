pvalcol <- function(pvals, col = c("darkgreen", "yellow", "red"),
                    pvlims = c(1, 0.05, 0.01)) {

  ifelse(pvals < pvlims[3], col[3],
         ifelse(pvals < pvlims[2], col[2],
                col[1]))
}

plotcols <- function() c("blue", "red", "green", "gold", "violet", "cyan")
