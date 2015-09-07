#' Title
#'
#' @param name
#' @param ve 
#' @param data 
#' @param factors 
#'
#' @return
#' @export
#'
#' @examples
compare_output <- function(name, ve, data, factors) {

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
  cmpout <- list(scores=pca$x, factors=factors, varexp=varexp, npcs=npcs, ve=ve, name=name,
                 p.values=list(manova=mnvpval, parametric=parpvals, nonparametric=nonparpvals),
                 tests=list(manova=mnvtest, parametric=partests, nonparametric=nonpartests))
  class(cmpout) <- "cmpoutput"
  cmpout
  
}

print.cmpoutput <- function(cmpout) {

  cat("Output name:", cmpout$name, "\n")
  cat("Number of PCs which explain ", cmpout$ve * 100, "% of variance: ", cmpout$npcs, "\n", sep="")
  if (cmpout$npcs > 1) {
    cat("P-Value for MANOVA along", cmpout$npcs, "dimensions:", cmpout$p.values$manova, "\n")
  }
  cat("P-Value for t-test (1st PC):", cmpout$p.values$parametric[1], "\n")
  cat("P-Value for Mann-Whitney test (1st PC):", cmpout$p.values$parametric[2], "\n")

}

plot.cmpoutput <- function(cmpout, col=c("blue","red","green","gold","violet","cyan"), ...) {
  
  par(mfrow=c(2,2))
  
  # Score plot (first two PCs)
  plot.default(cmpout$scores[,1], cmpout$scores[,2], col=col[as.numeric(cmpout$factors)], 
               xlab=paste("PC1 (", round(cmpout$varexp[1] * 100, 2), "%)", sep = ""), 
               ylab=paste("PC2 (", round(cmpout$varexp[2] * 100, 2), "%)", sep = ""), 
               main="Score plot", ...)
  
  # Explained variance bar plot
  barplot(cmpout$varexp[1:cmpout$npcs], names.arg=as.character(1:cmpout$npcs), 
          main="Explained variance by PC", xlab="PC", ylab="Var. exp. (%)", ...)
  
  # Parametric p-values bar plot
  barplot(cmpout$p.values$parametric, names.arg=as.character(1:cmpout$npcs), 
          main="Parametric p-values by PC", xlab="PC", ylab="Prob.", ...)
  
  # Non-parametric p-values bar plot
  barplot(cmpout$p.values$nonparametric, names.arg=as.character(1:cmpout$npcs), 
          main="Non-parametric p-values by PC", xlab="PC", ylab="Prob.", ...)
  
}