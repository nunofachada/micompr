#' Title
#'
#' @param name
#' @param ve 
#' @param data 
#' @param factors 
#'
#' @return Compared outputs
#' @export
#'
#' @examples #' micomp()
cmpoutput <- function(name, ve, data, factors) {

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
  
  # Assumptions list
  assumptions <- list()
  class(assumptions) <- "cmpoutput_assumptions"

  # Manova
  if (npcs > 1) {
    mnvtest <- manova(pca$x[,1:npcs] ~ factors)
    mnvpval <- summary(mnvtest)$stats[1,6]
    assumptions$manova <- assumptions_manova(pca$x[,1:npcs], factors)
  } else {
    mnvtest <- NULL
    mnvpval <- NA
    assumptions$manova <- NULL
  }
  
  parpvals <- vector(mode="numeric", length=npcs)
  partests <- list()
  nonparpvals <- vector(mode="numeric", length=npcs)
  nonpartests <- list()
  
  if (nlevels(factors) == 2) { # Use two-group tests

    # Cycle through each PC
    for (i in 1:npcs) {
      
      # Parametric test (t-test) for each PC
      partests[[i]] <- t.test(pca$x[,i] ~ factors, var.equal = T)
      parpvals[i] <- partests[[i]]$p.value
      assumptions$ttest <- assumptions_paruv(pca$x[,1:npcs], factors)
      
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
      assumptions$ttest <- assumptions_paruv(pca$x[,1:npcs], factors)
      
      # Non-parametric test (Kruskal-Wallis) for each PC
      nonpartests[[i]] <- kruskal.test(pca$x[,i] ~ factors)
      nonparpvals[i] <- nonpartests[[i]]$p.value
      
    }
    
  }
  
  # Return
  cmpout <- list(scores=pca$x, factors=factors, varexp=varexp, npcs=npcs, ve=ve, name=name,
                 p.values=list(manova=mnvpval, parametric=parpvals, nonparametric=nonparpvals),
                 tests=list(manova=mnvtest, parametric=partests, nonparametric=nonpartests),
                 assumptions=assumptions)
  class(cmpout) <- "cmpoutput"
  cmpout
  
}

#' Title
#'
#' @param cmpout 
#'
#' @return todo
#' @export
#'
#' @examples #' todo()
print.cmpoutput <- function(cmpout) {
  
  if (length(unique(cmpout$factors)) == 2) {
    test_names <- c("t-test", "Mann-Whitney U test")
  } else {
    test_names <- c("ANOVA test", "Kruskal-Wallis test")
  }

  cat("Output name:", cmpout$name, "\n")
  cat("Number of PCs which explain ", cmpout$ve * 100, "% of variance: ", cmpout$npcs, "\n", sep="")
  if (cmpout$npcs > 1) {
    cat("P-Value for MANOVA along", cmpout$npcs, "dimensions:", cmpout$p.values$manova, "\n")
  }
  cat("P-Value for", test_names[1], "(1st PC):", cmpout$p.values$parametric[1], "\n")
  cat("P-Value for", test_names[2], "(1st PC):", cmpout$p.values$nonparametric[1], "\n")

}

#' Title
#'
#' @param cmpout 
#'
#' @return todo
#' @export
#'
#' @examples #' todo()
summary.cmpoutput <- function(cmpout) {

    if (length(unique(cmpout$factors)) == 2) {
    test_names <- c("t-test", "Mann-Whitney")
  } else {
    test_names <- c("ANOVA", "Kruskal-Wallis")
  }
  
  list(output.name=cmpout$name, 
      num.pcs=cmpout$npcs, 
      var.exp=cmpout$ve,
      manova.pval=cmpout$p.values$manova, 
      parametric.test=test_names[1],
      parametric.pvals=cmpout$p.values$parametric,
      nonparametric.test=test_names[2],
      nonparametric.pvals=cmpout$p.values$nonparametric)
}

#' Title
#'
#' @param cmpout 
#' @param col 
#' @param ... 
#'
#' @return todo
#' @export
#'
#' @examples #' todo()
plot.cmpoutput <- function(cmpout, col=micomp:::plotcols(), ...) {
  
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

#' Title
#'
#' @param cmpoass 
#' @param ... 
#'
#' @return todo
#' @export
#'
#' @examples #' todo
plot.cmpoutput_assumptions <- function(cmpoass, ...) {
  
  if (exists('manova', where=cmpoass)) {
    plot(cmpoass$ttest, extra=1, ...)
    plot(cmpoass$manova, ...)
  } else {
    # No extra for multivariate assumptions, just plot univariate stuff
    plot(cmpoass$ttest, ...)
  }
}

#' Title
#'
#' @param obj 
#' @param ... 
#'
#' @return todo
#' @export
#'
#' @examples #' todo
assumptions.cmpoutput <- function(obj, ...) {
  obj$assumptions
}