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
  if (nlevels(factors) < 2)
    stop("At least two factors are required to perform model comparison.")
  
  # Perform PCA
  pca <- prcomp(data)
  
  # Explained variances
  eig <- (pca$sdev)^2
  varexp <- eig/sum(eig)
  cumvar <- cumsum(varexp)
  npcs <- which(cumvar > ve)[1]

  # Manova
  if (npcs > 1) {
    mnvtest <- manova(pca$x[,1:npcs] ~ factors)
    mnvpval <- summary(mnvtest)$stats[1,6]
  } else {
    mnvtest <- NULL
    mnvpval <- NA
  }
  
  parpvals <- vector(mode="numeric", length=npcs)
  partests <- list()
  nonparpvals <- vector(mode="numeric", length=npcs)
  nonpartests <- list()
  
  if (nlevels(factors) == 2) { # Use two-group tests

    # Cycle through each PC
    for (i in 1:npcs) {
      
      # Parametric test (t-test) for each PC
      partests[[i]] <- t.test(pca$x[,i] ~ factors)
      parpvals[i] <- partests[[i]]$p.value
      
      # Non-parametric test (Mann-Whitney) for each PC
      nonpartests[[i]] <- wilcox.test(pca$x[,i] ~ factors)
      nonparpvals[i] <- nonpartests[[i]]$p.value
      
    }
    
  } else { # Use multi-group tests (npcs > 2)

    # Cycle through each PC
    for (i in 1:npcs) {
      
      # Parametric test (ANOVA) for each PC
      partests[[i]] <- aov(pca$x[,i] ~ factors)
      parpvals[i] <- summary(partests[[i]])[[1]]$"Pr(>F)"[1]
      
      # Non-parametric test (Kruskal-Wallis) for each PC
      nonpartests[[i]] <- kruskal.test(pca$x[,i] ~ factors)
      nonparpvals[i] <- nonpartests[[i]]$p.value
      
    }
    
  }
  
  # Return
  cmpmod <- list(scores=pca$x, factors=factors, varexp=varexp, npcs=npcs, ve=ve, 
                 p.values=list(manova=mnvpval, parametric=parpvals, nonparametric=nonparpvals),
                 tests=list(manova=mnvtest, parametric=partests, nonparametric=nonpartests))
  class(cmpmod) <- "cmpmodels"
  cmpmod
  
}

print.cmpmodels <- function(cmpmod) {

  cat("Number of PCs which explain ", cmpmod$ve * 100, "% of variance: ", cmpmod$npcs, "\n", sep="")
  if (cmpmod$npcs > 1) {
    cat("P-Value for MANOVA along", cmpmod$npcs, "dimensions:", cmpmod$p.values$manova, "\n")
  }
  cat("P-Value for t-test (1st PC):", cmpmod$p.values$parametric[1], "\n")
  cat("P-Value for Mann-Whitney test (1st PC):", cmpmod$p.values$parametric[2], "\n")

}

plot.cmpmodels <- function(cmpmod, col=c("blue","red","green","gold","violet","cyan"), ...) {
  
  par(mfrow=c(2,2))
  
  # Score plot (first two PCs)
  plot.default(cmpmod$scores[,1], cmpmod$scores[,2], col=col[as.numeric(cmpmod$factors)], 
               xlab=paste("PC1 (", round(cmpmod$varexp[1] * 100, 2), "%)", sep = ""), 
               ylab=paste("PC2 (", round(cmpmod$varexp[2] * 100, 2), "%)", sep = ""), 
               main="Score plot", ...)
  
  # Explained variance bar plot
  barplot(cmpmod$varexp[1:cmpmod$npcs], names.arg=as.character(1:cmpmod$npcs), 
          main="Explained variance by PC", xlab="PC", ylab="Var. exp. (%)", ...)
  
  # Parametric p-values bar plot
  barplot(cmpmod$p.values$parametric, names.arg=as.character(1:cmpmod$npcs), 
          main="Parametric p-values by PC", xlab="PC", ylab="Prob.", ...)
  
  # Non-parametric p-values bar plot
  barplot(cmpmod$p.values$nonparametric, names.arg=as.character(1:cmpmod$npcs), 
          main="Non-parametric p-values by PC", xlab="PC", ylab="Prob.", ...)
  
}