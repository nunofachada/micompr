#' Title
#'
#' @param ve 
#' @param data 
#' @param groups 
#'
#' @return
#' @export
#'
#' @examples
compare_models <- function(ve, data, factors) {

  # Check parameters
  if (ve < 0 || ve > 1) 
    stop("'ve' parameter must be between 0 and 1.")
  if (length(factors) != dim(data)[1])
    stop("Number of observations in 'groups' and 'data' does not match.")
  
  # Perform PCA
  pca <- prcomp(data)
  
  # Explained variances
  eig <- (pca$sdev)^2
  varexp <- eig/sum(eig)
  cumvar <- cumsum(varexp)
  npcs <- which(cumvar > ve)[1]

  # Manova
  if (npcs > 1) {
    mnv_res <- manova(pca$x[,1:npcs] ~ factors)
  } else {
    mnv_res <- aov(pca$x[,1] ~ factors)
  }
  
  # t-test 1st PC
  # TODO: Should be ANOVA for more than two groups - Determine p-values for all PCs
  ttest_res <- t.test(pca$x[,1] ~ factors)
  
  # Mann-Whitney
  # TODO: Should be Kruskal-Wallis for more than two groups
  mw_res <- wilcox.test(pca$x[,1] ~ factors)
  
  # Return
  cmpmod <- list(scores=pca$x, mnv=mnv_res, ttest=ttest_res, mw=mw_res, varexp=varexp, npcs=npcs, ve=ve)
  class(cmpmod) <- "cmpmodels"
  cmpmod
  
}

print.cmpmodels <- function(cmpmod) {
  cat("Number of PCs which explain ", cmpmod$ve * 100, "% of variance: ", cmpmod$npcs, "\n", sep="")
  if (cmpmod$npcs > 1) {
    cat("P-Value for MANOVA along", cmpmod$npcs, "dimensions:", summary(cmpmod$mnv)$stats[1,6], "\n")
  } else {
    cat("P-Value for ANOVA (1st PC):", summary(cmpmod$mnv)[[1]]$"Pr(>F)"[1], "\n")
  }
  cat("P-Value for t-test (1st PC):", cmpmod$ttest$p.value, "\n")
  cat("P-Value for Mann-Whitney test (1st PC):", cmpmod$mw$p.value, "\n")

}