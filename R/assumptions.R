#' Title
#'
#' @param obj 
#' @param ... 
#'
#' @return todo
#' @export
#'
#' @examples #' todo
assumptions <- function(obj, ...) UseMethod("assumptions")

#' Title
#'
#' @param data 
#' @param factors 
#'
#' @return to do
#' @export
#'
#' @examples #' todo
assumptions_manova <- function(data, factors) {
  
  # `assumpt` will be a list containing the test for multivariate normality 
  # and the test for the homogeneity of covariance matrices
  assumpt = list()
  class(assumpt) <- "assumptions_manova"
  assumpt$mvntest <- list()
  assumpt$vartest <- NULL

  # Number of variables
  nvars <- dim(data)[2]
  
  # Cycle through factors for the multivariate normality tests
  for (f in unique(factors)) {
    # Indexes of current factor level
    fidx <- factors==f
    nobs <- sum(fidx)
    assumpt$mvntest[[f]] <- MVN::roystonTest(data[fidx,1:min(nobs-1, nvars)])
  }
  
  # Perform the homogeneity of covariance matrices test (Box's M)
  maxvars <- min(table(factors)-1, nvars)
  assumpt$vartest <- biotools::boxM(data[,1:maxvars], factors)
  
  assumpt
}

#' Title
#'
#' @param data 
#' @param factors 
#'
#' @return to do
#' @export
#'
#' @examples #' todo
assumptions_paruv <- function(data, factors) {
  
  # Number of variables
  nvars <- dim(data)[2]
  if (is.null(nvars)) nvars <- 1
  
  # `assumpt` will be a list containing the tests for univariate normality 
  # assumptions and the test for the homogeneity of variances assumption.
  assumpt = list()
  class(assumpt) <- "assumptions_paruv"
  assumpt$uvntest <- list()
  assumpt$vartest <- list()
  
  # Cycle through each variable
  for (d in 1:nvars) {

    # Get current data
    if (nvars == 1) {
      currdata <- data
    } else {
      currdata <- data[, d]
    }
    
    # Perform univariate normality tests for each group for current variable
    for (f in unique(factors)) {
      if (!is.list(assumpt$uvntest[[f]])) assumpt$uvntest[[f]] <- list()
      assumpt$uvntest[[f]][[d]] <- shapiro.test(currdata[factors==f])
    }
  
    # Perform the homogeneity of variances test for current variable
    assumpt$vartest[[d]] <- bartlett.test(currdata ~ factors)
  
  }

  assumpt
  
}

#' Title
#'
#' @param asmnv 
#'
#' @return todo
#' @export
#'
#' @examples #' todo
print.assumptions_manova <- function(asmnv) {
  
  for (grp in names(asmnv$mvntest)) {
    cat(grp, ":", asmnv$mvntest[[grp]]@p.value, "\n")
  }
  cat("Var:", asmnv$vartest$p.value, "\n")
}

#' Title
#'
#' @param aspuv 
#'
#' @return todo
#' @export
#'
#' @examples #' todo
print.assumptions_paruv <- function(aspuv) {
  
  maxvars <- min(5, length(aspuv$uvntest[[1]]))
  
  for (grp in names(aspuv$uvntest)) {
    cat(grp, ": ")
    for (i in 1:maxvars) {
      cat("", aspuv$uvntest[[grp]][[i]]$p.value)
    }
    cat(" ... \n")
  }

  cat("Var :")
  for (i in 1:maxvars) {
    cat("", aspuv$vartest[[i]]$p.value)
  }
  cat(" ... \n")
}

#' Title
#'
#' @param asmnv 
#' @param ... 
#'
#' @return todo
#' @export
#'
#' @examples #' todo
plot.assumptions_manova <- function(asmnv, ...) {
  
  pvals <- sapply(asmnv$mvntest, function(x) x@p.value)
  barplot(pvals, main="Royston test p-values", sub="Multivariate normality",
          xlab="Groups", ylab="Probability", col=micomp:::pvalcol(pvals), ...)

}

#' Title
#'
#' @param aspuv 
#' @param extra 
#' @param ... 
#'
#' @return todo
#' @export
#'
#' @examples #' todo
plot.assumptions_paruv <- function(aspuv, extra=0, ...) {
  
  # Number of vars in the PC plots
  nvars <- length(aspuv$uvntest[[1]])

  # One plot for each factor + 1 for the variance + extra for more plots
  nplots <- length(aspuv$uvntest) + 1 + extra
  
  # Plot matrix side dimension 
  side_dim <- ceiling(sqrt(nplots))
  
  par(mfrow=c(side_dim, side_dim))
  # Plot the Bartlett test p-values by PC
  vardata <- sapply(aspuv$vartest, function(x) x$p.value)
  barplot(vardata, names.arg=as.character(1:nvars), 
          main="p-values for the Bartlett test", sub="Homogeneity of Variances", 
          xlab="PC", ylab="Probability", col=micomp:::pvalcol(vardata), ...)
  
  # Plot the Shapiro-Wilk p-values by PC for each factor
  for (grp in names(aspuv$uvntest)) {
    normdata <- sapply(aspuv$uvntest[[grp]], function(x) x$p.value)
    barplot(normdata, names.arg=as.character(1:nvars), 
            sub=grp, main="p-values for the SW normality test",  
            xlab="PC", ylab="Probability", col=micomp:::pvalcol(normdata), ...)
  }
  
}
