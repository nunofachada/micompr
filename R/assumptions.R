#' Get assumptions for parametric tests applied to the comparison of simulation
#' outputs
#'
#' Generic function to get the assumptions for parametric tests applied to the
#' comparison of simulation outputs.
#'
#' @param obj Object from which to get the assumptions.
#'
#' @return Assumptions for parametric tests applied to the comparison of
#' simulation outputs.
#'
#' @export
#'
#' @examples
#' NULL
#'
assumptions <- function(obj) UseMethod("assumptions")

#' Determine the assumptions for the MANOVA test
#'
#' Determine two assumptions for the MANOVA test: a) multivariate normality
#' of each group; b) homogeneity of covariance matrices.
#'
#' @param data Data used for the MANOVA test (rows correspond to observations,
#' columns to dependent variables).
#' @param factors Groups to which rows of \code{data} belong to (independent
#' variables).
#'
#' @return An object of class \code{assumptions_manova} which is a list
#' containing two elements:
#' \describe{
#'  \item{\code{mvntest}}{List of results from the Royston multivariate
#'        normality test (\code{\link[MVN]{roystonTest}}), one result per
#'        group.}
#'  \item{\code{vartest}}{Result of Box's M test for homogeneity of covariance
#'        matrices (\code{\link[biotools]{boxM}}).}
#' }
#'
#' @export
#'
#' @examples
#' NULL
#'
assumptions_manova <- function(data, factors) {

  # Don't return MANOVA assumptions if the required packages are not installed.
  if (!requireNamespace("MVN", quietly = TRUE) ||
      !requireNamespace("biotools", quietly = TRUE) ) {
    message("MANOVA assumptions require 'MVN' and 'biotools' packages.")
    return(NULL)
  }

  # `assumpt` will be a list containing the test for multivariate normality
  # and the test for the homogeneity of covariance matrices
  assumpt <- list()
  class(assumpt) <- "assumptions_manova"
  assumpt$mvntest <- list()
  assumpt$vartest <- NULL

  # Number of variables
  nvars <- dim(data)[2]

  # Cycle through factors for the multivariate normality tests
  for (f in unique(factors)) {
    # Indexes of current factor level
    fidx <- factors == f
    nobs <- sum(fidx)
    assumpt$mvntest[[f]] <- MVN::roystonTest(data[fidx,1:min(nobs - 1, nvars)])
  }

  # Perform the homogeneity of covariance matrices test (Box's M)
  maxvars <- min(table(factors) - 1, nvars)
  assumpt$vartest <- biotools::boxM(data[,1:maxvars], factors)

  assumpt
}

#' Determine the assumptions for the parametric comparison test
#'
#' Determine two assumptions for the parametric comparison tests (i.e. either
#' \code{\link{t.test}} or \code{\link{aov}}) for each principal component,
#' namely: a) univariate normality of each group; b) homogeneity of variances.
#'
#' @param data Data used in the parametric test (rows correspond to
#' observations, columns to principal components).
#' @param factors Groups to which rows of \code{data} belong to.
#'
#' @return An object of class \code{assumptions_paruv} which is a list
#' containing two elements:
#' \describe{
#'  \item{\code{uvntest}}{List of results from the Shapiro-Wilk normality test
#'  (\code{\link{shapiro.test}}), one result per group per principal component.}
#'  \item{\code{vartest}}{Result of Bartlett test for homogeneity of variances
#'        (\code{\link{bartlett.test}}).}
#' }
#'
#' @export
#'
#' @examples
#' NULL
#'
assumptions_paruv <- function(data, factors) {

  # Number of variables
  nvars <- dim(data)[2]
  if (is.null(nvars)) nvars <- 1

  # `assumpt` will be a list containing the tests for univariate normality
  # assumptions and the test for the homogeneity of variances assumption.
  assumpt <- list()
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
      assumpt$uvntest[[f]][[d]] <- shapiro.test(currdata[factors == f])
    }

    # Perform the homogeneity of variances test for current variable
    assumpt$vartest[[d]] <- bartlett.test(currdata ~ factors)

  }

  assumpt

}

#' Print information about the assumptions of the MANOVA test
#'
#' Print information about objects of class \code{assumptions_manova}, which
#' represent the assumptions of the MANOVA test performed on a comparison of
#' simulation output.
#'
#' @param asmnv Object of class \code{assumptions_manova}.
#'
#' @return The argument \code{asmnv}, invisibly, as for all \code{\link{print}}
#' methods.
#'
#' @export
#'
#' @examples
#' NULL
#'
print.assumptions_manova <- function(asmnv) {

  for (grp in names(asmnv$mvntest)) {
    cat(grp, ":", asmnv$mvntest[[grp]]@p.value, "\n")
  }
  cat("Var:", asmnv$vartest$p.value, "\n")

  invisible(asmnv)
}

#' Print information about the assumptions of the parametric test
#'
#' Print information about objects of class \code{assumptions_paruv}, which
#' represent the assumptions of the parametric test (i.e. either
#' \code{\link{t.test}} or \code{\link{aov}}) performed on a comparison of
#' simulation output.
#'
#' @param aspuv Object of class \code{assumptions_paruv}.
#'
#' @return The argument \code{aspuv}, invisibly, as for all \code{\link{print}}
#' methods.
#'
#' @export
#'
#' @examples
#' NULL
#'
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

  invisible(aspuv)
}

#' Plot \emph{p}-values for testing the multivariate normality assumptions of
#' the MANOVA test
#'
#' Plot method for objects of class \code{\link{assumptions_manova}} which
#' presents a bar plot containing the \emph{p}-values produced by the Royston
#' multivariate normality test (\code{\link[MVN]{roystonTest}}) for each group
#' being compared.
#'
#' @param asmnv Objects of class \code{\link{assumptions_manova}}.
#' @param ... Extra options passed to \code{\link{plot.default}}.
#'
#' @return None.
#'
#' @export
#'
#' @examples
#' NULL
#'
plot.assumptions_manova <- function(asmnv, ...) {

  pvals <- sapply(asmnv$mvntest, function(x) x@p.value)
  barplot(pvals, main = "Royston test p-values", sub = "Multivariate normality",
          xlab = "Groups", ylab = "Probability", col = micompr:::pvalcol(pvals),
          ...)

  invisible(NULL)
}

#' Plot \emph{p}-values for testing the assumptions of the parametric tests used
#' in simulation output comparison
#'
#' Plot method for objects of class \code{\link{assumptions_paruv}}
#' containing \emph{p}-values produced by testing the assumptions of the
#' parametric tests used for comparing simulation output.
#'
#' One bar plot is presented for the Bartlett test
#' (\code{\link{bartlett.test}}), showing the respective \emph{p}-values along
#' principal component. \emph{s} bar plots are presented for the Shapiro-Wilk
#' (\code{\link{shapiro.test}}), where \emph{s} is the number of groups being
#' compared; individual bars in each plot represent the \emph{p}-values
#' associated with each principal component.
#'
#' @param aspuv Objects of class \code{\link{assumptions_paruv}}.
#' @param extra Number of extra sub-plot spaces to create (useful when this
#' function is called from another which will produce more plots).
#' @param ... Extra options passed to \code{\link{plot.default}}.
#'
#' @return None.
#'
#' @export
#'
#' @examples
#' NULL
#'
plot.assumptions_paruv <- function(aspuv, extra = 0, ...) {

  # Number of vars in the PC plots
  nvars <- length(aspuv$uvntest[[1]])

  # One plot for each factor + 1 for the variance + extra for more plots
  nplots <- length(aspuv$uvntest) + 1 + extra

  # Plot matrix side dimension
  side_dim <- ceiling(sqrt(nplots))

  par(mfrow = c(side_dim, side_dim))
  # Plot the Bartlett test p-values by PC
  vardata <- sapply(aspuv$vartest, function(x) x$p.value)
  barplot(vardata, names.arg = as.character(1:nvars),
          main = "p-values for the Bartlett test",
          sub = "Homogeneity of Variances",
          xlab = "PC", ylab = "Probability",
          col = micompr:::pvalcol(vardata), ...)

  # Plot the Shapiro-Wilk p-values by PC for each factor
  for (grp in names(aspuv$uvntest)) {
    normdata <- sapply(aspuv$uvntest[[grp]], function(x) x$p.value)
    barplot(normdata, names.arg = as.character(1:nvars),
            sub = grp, main = "p-values for the SW normality test",
            xlab = "PC", ylab = "Probability",
            col = micompr:::pvalcol(normdata),
            ...)
  }

  invisible(NULL)

}
