#' Compares simulation output
#'
#' Compares one output from several runs of two or more model implementations.
#'
#' @param name Comparison name (useful when calling this function to perform
#' multiple comparisons).
#' @param ve Percentage (between 0 and 1) of variance explained by the \emph{q}
#' principal components (i.e. number of dimensions) used in MANOVA.
#' @param data An \emph{n} x \emph{m} matrix, where \emph{n} is the total number
#' of output observations (runs) and \emph{m} is the number of variables (i.e.
#' output length).
#' @param factors Factors (or groups) associated with each observation.
#'
#' @return  Object of class \code{cmpoutput} containing the following data:
#' \describe{
#'  \item{scores}{\emph{n} x \emph{n} matrix containing projections of
#'        simulation output data in the principal components space. Rows
#'        correspond to observations, columns to principal components. }
#'  \item{factors}{Factors (or groups) associated with each observation.}
#'  \item{varexp}{Percentage of variance explained by each principal component.}
#'  \item{npcs}{Number of principal components which explain \code{ve}
#'        percentage of variance.}
#'  \item{ve}{Percentage (between 0 and 1) of variance explained by the \emph{q}
#'        principal components (i.e. number of dimensions) used in MANOVA.}
#'  \item{name}{Comparison name (useful when calling this function to perform
#'        multiple comparisons).}
#'  \item{p.values}{\emph{P}-values for the performed statistical tests, namely:
#'    \describe{
#'      \item{manova}{\emph{P}-values for the MANOVA test for \code{npcs}
#'            principal components.}
#'      \item{parametric}{Vector of \emph{p}-values for the parametric test
#'            applied to groups along each principal component (\emph{t}-test
#'            for 2 groups, ANOVA for more than 2 groups).}
#'      \item{nonparametric}{Vector of \emph{p}-values for the non-parametric
#'            test applied to groups along each principal component
#'            (Mann-Whitney U test for 2 groups, Kruskal-Wallis test for more
#'            than 2 groups).}
#'    }
#'  }
#'  \item{tests}{
#'    \describe{
#'      \item{manova}{Object returned by the \code{\link{manova}} function.}
#'      \item{parametric}{List of objects returned by applying
#'            \code{\link{t.test}} (two groups) or \code{\link{aov}} (more than
#'            two groups) to each principal component.}
#'      \item{nonparametric}{List of objects returned by applying
#'            \code{\link{wilcox.test}} (two groups) or
#'            \code{\link{kruskal.test}} (more than two groups) to each
#'            principal component.}
#'    }
#'  }
#' }
#'
#' @export
#'
#' @examples
#'
#' # Comparing the first output ("Pop.Sheep") of one the provided datasets.
#' cmp <-
#'  cmpoutput("SheepPop", 0.8, pphpc_ok$data[["Pop.Sheep"]], pphpc_ok$factors)
#'
#' # Compare bogus outputs created from 2 random sources, 5 observations per
#' # source, 20 variables each, yielding a 10 x 20 data matrix.
#' data <- matrix(c(rnorm(100), rnorm(100, mean = 1)), nrow = 10, byrow = TRUE)
#' factors <- factor(c(rep("A", 5), rep("B", 5)))
#' cmp <- cmpoutput("Bogus", 0.7, data, factors)
#'
cmpoutput <- function(name, ve, data, factors) {

  # Check parameters
  if (ve < 0 || ve > 1)
    stop("'ve' parameter must be between 0 and 1.")
  if (length(factors) != dim(data)[1])
    stop("Number of observations in 'data' and 'factors' does not match.")
  if (nlevels(factors) < 2)
    stop("At least two factors are required to perform model comparison.")

  # Perform PCA
  pca <- prcomp(data)

  # Explained variances
  eig <- (pca$sdev) ^ 2
  varexp <- eig / sum(eig)
  cumvar <- cumsum(varexp)
  npcs <- which(cumvar > ve)[1]

  # Manova
  if (npcs > 1) {
    # Can only use manova if more than one variable
    mnvtest <- manova(pca$x[,1:npcs] ~ factors)
    mnvpval <- summary(mnvtest)$stats[1,6]
  } else {
    # Only one variable, can't use manova
    mnvtest <- NULL
    mnvpval <- NA
  }

  parpvals <- vector(mode = "numeric", length = npcs)
  partests <- list()
  nonparpvals <- vector(mode = "numeric", length = npcs)
  nonpartests <- list()

  if (nlevels(factors) == 2) {
    # Use two-group tests

    # Cycle through each PC
    for (i in 1:npcs) {

      # Parametric test (t-test) for each PC
      partests[[i]] <- t.test(pca$x[,i] ~ factors, var.equal = T)
      parpvals[i] <- partests[[i]]$p.value

      # Non-parametric test (Mann-Whitney) for each PC
      nonpartests[[i]] <- wilcox.test(pca$x[,i] ~ factors)
      nonparpvals[i] <- nonpartests[[i]]$p.value

    }

  } else {
    # Use multi-group tests (npcs > 2)

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
  cmpout <- list(scores = pca$x, factors = factors, varexp = varexp,
                 npcs = npcs, ve = ve, name = name,
                 p.values = list(manova = mnvpval, parametric = parpvals,
                                 nonparametric = nonparpvals),
                 tests = list(manova = mnvtest, parametric = partests,
                            nonparametric = nonpartests))
  class(cmpout) <- "cmpoutput"
  cmpout

}

#' Print information about comparison of simulation output
#'
#' Print information about objects of class \code{cmpoutput}.
#'
#' @param cmpout Object of class \code{cmpoutput}.
#'
#' @return The argument \code{cmpout}, invisibly, as for all \code{\link{print}}
#' methods.
#'
#' @export
#'
#' @examples
#'
#' # Comparing the fifth output of the pphpc_diff dataset, which contains
#' # simulation output data from two implementations of the PPHPC model executed
#' # with a different parameter.
#'
#' cmpoutput("WolfPop", 0.7, pphpc_diff$data[[5]], pphpc_diff$factors)
#'
#' ## Output name: WolfPop
#' ## Number of PCs which explain 70% of variance: 2
#' ## P-Value for MANOVA along 2 dimensions: 5.533087e-08
#' ## P-Value for t-test (1st PC): 1.823478e-08
#' ## P-Value for Mann-Whitney U test (1st PC): 1.082509e-05
#'
print.cmpoutput <- function(cmpout) {

  if (length(unique(cmpout$factors)) == 2) {
    test_names <- c("t-test", "Mann-Whitney U test")
  } else {
    test_names <- c("ANOVA test", "Kruskal-Wallis test")
  }

  cat("Output name:", cmpout$name, "\n")
  cat("Number of PCs which explain ", cmpout$ve * 100, "% of variance: ",
      cmpout$npcs, "\n", sep = "")
  if (cmpout$npcs > 1) {
    cat("P-Value for MANOVA along", cmpout$npcs, "dimensions:",
        cmpout$p.values$manova, "\n")
  }
  cat("P-Value for", test_names[1], "(1st PC):",
      cmpout$p.values$parametric[1], "\n")
  cat("P-Value for", test_names[2], "(1st PC):",
      cmpout$p.values$nonparametric[1], "\n")

  invisible(cmpout)

}

#' Summary method for comparison of simulation outputs
#'
#' Summary method for objects of class \code{cmpoutput}.
#'
#' @param cmpout Object of class \code{cmpoutput}.
#'
#' @return A list with the following components:
#' \describe{
#'  \item{num.pcs}{Number of principal components which explain \code{var.exp}
#'        percentage of variance.}
#'  \item{var.exp}{Minimum percentage of variance which must be explained by the
#'        number of principal components used for the MANOVA test.}
#'  \item{manova.pval}{\emph{P}-value of the MANOVA test.}
#'  \item{parametric.test}{Name of the used parametric test.}
#'  \item{parametric.pvals}{Vector of $p$-values returned by applying the
#'        parametric test to each principal component.}
#'  \item{nonparametric.test}{Name of the used non-parametric test.}
#'  \item{nonparametric.pvals}{Vector of $p$-values returned by applying the
#'        non-parametric test to each principal component.}
#' }
#'
#' @export
#'
#' @examples
#'
#' # Comparing the concatenated output of the pphpc_noshuff dataset, which
#' # contains simulation output data from two implementations of the PPHPC model
#' # executed with a minor implementation difference.
#'
#' summary(
#'   cmpoutput("All", 0.6, pphpc_noshuff$data[["All"]], pphpc_noshuff$factors)
#' )
#'
#' ## $output.name
#' ## [1] "All"
#' ##
#' ## $num.pcs
#' ## [1] 3
#' ##
#' ## $var.exp
#' ## [1] 0.6
#' ##
#' ## $manova.pval
#' ## [1] 0.003540518
#' ##
#' ## $parametric.test
#' ## [1] "t-test"
#' ##
#' ## $parametric.pvals
#' ## [1] 0.001073006 0.788283742 0.172055145
#' ##
#' ## $nonparametric.test
#' ## [1] "Mann-Whitney"
#' ##
#' ## $nonparametric.pvals
#' ## [1] 0.0007252809 0.9117971811 0.3149992422
#'
summary.cmpoutput <- function(cmpout) {

  if (length(unique(cmpout$factors)) == 2) {
    test_names <- c("t-test", "Mann-Whitney")
  } else {
    test_names <- c("ANOVA", "Kruskal-Wallis")
  }

  list(output.name = cmpout$name,
       num.pcs = cmpout$npcs,
       var.exp = cmpout$ve,
       manova.pval = cmpout$p.values$manova,
       parametric.test = test_names[1],
       parametric.pvals = cmpout$p.values$parametric,
       nonparametric.test = test_names[2],
       nonparametric.pvals = cmpout$p.values$nonparametric)
}

#' Plot comparison of simulation output
#'
#' Plot objects of class  \code{cmpoutput}.
#'
#' This method produces four sub-plots, namely:
#' \itemize{
#'   \item Scatter plot containing the projection of output observations on the
#'         first two dimensions of the principal components space.
#'   \item Bar plot of the percentage of variance explain per principal
#'         component.
#'   \item Bar plot of \emph{p}-values for the parametric test for each
#'         principal component.
#'   \item Bar plot of \emph{p}-values for the non-parametric test for each
#'         principal component.
#' }
#'
#' @param cmpout Object of class \code{cmpoutput}.
#' @param col Vector of colors to use on observations of different groups
#' (scatter plot only).
#' @param ... Extra options passed to \code{\link{plot.default}}.
#'
#' @return None.
#'
#' @export
#'
#' @examples
#'
#' # Comparing the concatenated output of the pphpc_ok dataset, which
#' # contains simulation output data from two similar implementations of the
#' # PPHPC model.
#'
#' plot(cmpoutput("All", 0.95, pphpc_ok$data[["All"]], pphpc_ok$factors))
#'
plot.cmpoutput <- function(cmpout, col = micompr:::plotcols(), ...) {

  par(mfrow = c(2,2))

  # Score plot (first two PCs)
  plot.default(cmpout$scores[,1], cmpout$scores[,2],
               col = col[as.numeric(cmpout$factors)],
               xlab = paste("PC1 (", round(cmpout$varexp[1] * 100, 2), "%)",
                          sep = ""),
               ylab = paste("PC2 (", round(cmpout$varexp[2] * 100, 2), "%)",
                          sep = ""),
               main = "Score plot", ...)

  # Explained variance bar plot
  barplot(cmpout$varexp[1:cmpout$npcs], names.arg = as.character(1:cmpout$npcs),
          main = "Explained variance by PC",
          xlab = "PC", ylab = "Var. exp. (%)", ...)

  # Parametric p-values bar plot
  barplot(cmpout$p.values$parametric, names.arg = as.character(1:cmpout$npcs),
          main = "Parametric p-values by PC",
          xlab = "PC", ylab = "Prob.", ...)

  # Non-parametric p-values bar plot
  barplot(cmpout$p.values$nonparametric,
          names.arg = as.character(1:cmpout$npcs),
          main = "Non-parametric p-values by PC",
          xlab = "PC", ylab = "Prob.", ...)

  invisible(NULL)
}

#' Get assumptions for parametric tests performed on output comparisons.
#'
#' Get assumptions for parametric tests performed on output comparisons (i.e.
#' from objects of class \code{\link{cmpoutput}}).
#'
#' @param cmpout Object of class \code{cmpoutput}.
#'
#' @return Object of class \code{assumptions_cmpoutput} containing the
#' assumptions for parametric tests performed on an output comparisons
#' Basically a list containing the assumptions for the MANOVA (object of class
#' \code{\link{assumptions_manova}}) and univariate parametric tests for each
#' principal component (object of class \code{\link{assumptions_paruv}}).
#'
#' @export
#'
#' @examples
#'
#' # Create a cmpoutput object from the provided datasets
#' cmp <- cmpoutput("All", 0.9, pphpc_ok$data[["All"]], pphpc_ok$factors)
#'
#' # Get the assumptions for the parametric tests performed in cmp
#' acmp <- assumptions(cmp)
#'
assumptions.cmpoutput <- function(cmpout) {

  # Create assumptions object
  assumptions <- list()
  class(assumptions) <- "assumptions_cmpoutput"

  npcs <- cmpout$npcs
  factors <- cmpout$factors
  scores <- cmpout$scores

  # Manova
  if (npcs > 1) {

    # Can only use manova if more than one variable
    assumptions$manova <- assumptions_manova(scores[, 1:npcs], factors)

  } else {

    # Only one variable, can't use manova
    assumptions$manova <- NULL

  }

  # Parametric test (t-test) for each PC
  assumptions$ttest <- assumptions_paruv(scores[, 1:npcs], factors)

  # Return assumptions
  assumptions

}

#' Plot \emph{p}-values for testing the assumptions of the parametric tests used
#' in simulation output comparison
#'
#' Plot method for objects of class \code{assumptions_cmpoutput}
#' containing \emph{p}-values produced by testing the assumptions of the
#' parametric tests used for comparing simulation output.
#'
#' Several bar plots are presented, showing the \emph{p}-values yielded by the
#' Shapiro-Wilk (\code{\link{shapiro.test}}) and Royston tests
#' (\code{\link[MVN]{roystonTest}}) for univariate and multivariate normality,
#' respectively, and for the Bartlett (\code{\link{bartlett.test}}) and Box's M
#' (\code{\link[biotools]{boxM}}) for testing homogeneity of variances and of
#' covariance matrices, respectively. The following bar plots are shown:
#'
#' \itemize{
#'  \item One bar plot for the \emph{p}-values of the Bartlett test, one bar
#'        (\emph{p}-value) per individual principal component.
#'  \item \emph{s} bar plots for \emph{p}-values of the Shapiro-Wilk test, where
#'        \emph{s} is the number of groups being compared. Individual bars in
#'        each plot are associated with a principal component.
#'  \item One bar plot for the \emph{p}-values of the Royston test with \emph{s}
#'        bars, where \emph{s} is the number of groups being compared. This plot
#'        will not show if there is only one principal component being
#'        considered.
#' }
#'
#' @param cmpoass Objects of class \code{assumptions_cmpoutput}.
#' @param ... Extra options passed to \code{\link{plot.default}}.
#'
#' @return None.
#'
#' @export
#'
#' @examples
#'
#' # Create a cmpoutput object from the provided datasets
#' cmp <- cmpoutput("All", 0.9, pphpc_ok$data[["All"]], pphpc_ok$factors)
#'
#' # Display a bar plot with the p-values of the assumptions for the parametric
#' # tests performed in cmp
#' plot(assumptions(cmp))
#'
plot.assumptions_cmpoutput <- function(cmpoass, ...) {

  if (exists("manova", where = cmpoass)) {
    plot(cmpoass$ttest, extra = 1, ...)
    plot(cmpoass$manova, ...)
  } else {
    # No extra for multivariate assumptions, just plot univariate stuff
    plot(cmpoass$ttest, ...)
  }

  invisible(NULL)

}
