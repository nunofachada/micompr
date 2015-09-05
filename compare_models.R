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
    mnv_res <- NA
  }
  
  # t-test 1st PC - Should be ANOVA for more than two groups
  ttest_res <- t.test(pca$x[,1] ~ factors)
  
  # Mann-Whitney - Should be Kruskal-Wallis for more than two groups
  mw_res <- wilcox.test(pca$x[,1] ~ factors)
  
  # Return
  list(scores=pca$x, mnv=mnv_res, ttest=ttest_res, mw=mw_res, varexp=varexp, npcs=npcs)
}