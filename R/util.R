#' Associate colors to \emph{p}-values
#'
#' Associate colors to \emph{p}-values according to their value.
#'
#' @param pvals Vector of \emph{p}-values to which associate colors.
#' @param col Colors Vector of colors to associate with the \emph{p}-values
#' given in \code{pvals} according to the limits specified in \code{pvlims}.
#' @param pvlims Vector of \emph{p}-value upper limits, first value should be 1.
#'
#' @return A vector of colors associated with \emph{p}-values given in
#' \code{pvals}.
#'
#' @keywords internal
#'
#' @examples
#' micompr:::pvalcol(c(0.06, 0.9, 0.0001, 0.3, 0.2, 0.02),
#'                   c("darkgreen", "yellow", "red"))
#' # [1] "darkgreen" "darkgreen" "red"       "darkgreen" "darkgreen" "yellow"
#'
pvalcol <- function(pvals, col, pvlims = c(0.05, 0.01)) {

  idxs <- sapply(pvals, function(p) match(F, p < c(pvlims, -Inf)))
  colors <- col[idxs]
  colors
}

#' Default colors for plots in \code{micomp} package
#'
#' Default colors for plots in \code{micomp} package.
#'
#' @return Default colors for plots in \code{micomp} package.
#'
#' @keywords internal
#'
#' @examples
#' micompr:::plotcols()
#' # [1] "blue"   "red"    "green"  "gold"   "violet" "cyan"
#'
plotcols <- function() {
  c("blue", "red", "green", "gold", "violet", "cyan")
}
