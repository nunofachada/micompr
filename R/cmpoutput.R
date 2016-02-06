#' Compares simulation output
#'
#' Compares one output from several runs of two or more model implementations.
#'
#' @param name Comparison name (useful when calling this function to perform
#' multiple comparisons).
#' @param ve Percentage (between 0 and 1) of variance explained by the \emph{q}
#' principal components (i.e. number of dimensions) used in MANOVA. Can be a
#' vector, in which case the MANOVA test will be applied multiple times, one per
#' number of principal components required to explain each of the variance
#' percentages passed in the vector.
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
#'      \item{manova}{List of \emph{p}-values for the MANOVA test for each
#'            principal component in \code{npcs}.}
#'      \item{parametric}{Vector of \emph{p}-values for the parametric test
#'            applied to groups along each principal component (\emph{t}-test
#'            for 2 groups, ANOVA for more than 2 groups).}
#'      \item{nonparametric}{Vector of \emph{p}-values for the non-parametric
#'            test applied to groups along each principal component
#'            (Mann-Whitney U test for 2 groups, Kruskal-Wallis test for more
#'            than 2 groups).}
#'      \item{parametric_adjusted}{Same as field \code{parametric}, but
#'            \emph{p}-values are adjusted using weighted Bonferroni procedure.
#'            Percentages of explained variance are used as weights.}
#'      \item{nonparametric_adjusted}{Same as field \code{nonparametric}, but
#'            \emph{p}-values are adjusted using weighted Bonferroni procedure.
#'            Percentages of explained variance are used as weights.}
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
  if (ve < 0 || ve >= 1)
    stop("'ve' parameter must be in the interval [0, 1[.")
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
  nve <- length(ve)

  # Pre-allocate vectors for Manova test
  npcs = vector(mode = "integer", length = nve)
  mnvtest = vector(mode = "list", length = nve)
  mnvpval = vector(mode = "numeric", length = nve)

  # Perform a Manova test for each specified ve (variance to explain)
  for (i in 1:nve) {

    # Number of PCs required to explain the specified percentage of variance
    npcs[i] <- which(cumvar > ve[i])[1]

    # Manova
    if (npcs[i] > 1) {
      # Can only use Manova with more than one dimension
      mnvtest[[i]] <- manova(pca$x[, 1:npcs[i]] ~ factors)
      mnvpval[i] <- summary(mnvtest[[i]])$stats[1, 6]
    } else {
      # Only one dimension, can't use Manova
      mnvtest[[i]] <- NULL
      mnvpval[i] <- NA
    }
  }

  # Total number of PCs returned by PCA operation
  tpcs <-length(eig);

  # Univariate tests
  parpvals <- vector(mode = "numeric", length = tpcs)
  parpvals_adjusted <- vector(mode = "numeric", length = tpcs)
  partests <- list()
  nonparpvals <- vector(mode = "numeric", length = tpcs)
  nonparpvals_adjusted <- vector(mode = "numeric", length = tpcs)
  nonpartests <- list()

  if (nlevels(factors) == 2) {
    # Use two-group tests

    # Cycle through each PC
    for (i in 1:tpcs) {

      # Parametric test (t-test) for each PC
      partests[[i]] <- t.test(pca$x[, i] ~ factors, var.equal = T)
      parpvals[i] <- partests[[i]]$p.value

      # Non-parametric test (Mann-Whitney) for each PC
      nonpartests[[i]] <- wilcox.test(pca$x[, i] ~ factors)
      nonparpvals[i] <- nonpartests[[i]]$p.value

    }

  } else {
    # Use multi-group tests (npcs > 2)

    # Cycle through each PC
    for (i in 1:tpcs) {

      # Parametric test (ANOVA) for each PC
      partests[[i]] <- aov(pca$x[, i] ~ factors)
      parpvals[i] <- summary(partests[[i]])[[1]]$"Pr(>F)"[1]

      # Non-parametric test (Kruskal-Wallis) for each PC
      nonpartests[[i]] <- kruskal.test(pca$x[, i] ~ factors)
      nonparpvals[i] <- nonpartests[[i]]$p.value

    }

  }

  # Determine adjusted univariate p-values using the weighted Bonferroni
  # procedure, explained variances used as weights
  parpvals_adjusted <- pmin(parpvals / varexp, 1)
  nonparpvals_adjusted <- pmin(nonparpvals / varexp, 1)

  # Return
  cmpout <- list(scores = pca$x,
                 factors = factors,
                 varexp = varexp,
                 npcs = npcs,
                 ve = ve,
                 name = name,
                 p.values = list(manova = mnvpval,
                                 parametric = parpvals,
                                 nonparametric = nonparpvals,
                                 parametric_adjusted = parpvals_adjusted,
                                 nonparametric_adjusted = nonparpvals_adjusted),
                 tests = list(manova = mnvtest,
                              parametric = partests,
                              nonparametric = nonpartests))
  class(cmpout) <- "cmpoutput"
  cmpout

}

#' Print information about comparison of simulation output
#'
#' Print information about objects of class \code{cmpoutput}.
#'
#' @param x Object of class \code{cmpoutput}.
#' @param ... Currently ignored.
#'
#' @return The argument \code{x}, invisibly, as for all \code{\link{print}}
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
#' ## P-Value for MANOVA along 2 dimensions: 5.53309e-08
#' ## P-Value for t-test (1st PC): 1.823478e-08
#' ## P-Value for Mann-Whitney U test (1st PC): 1.082509e-05
#' ## Adjusted p-Value for t-test (1st PC): 3.144908e-08
#' ## Adjusted p-Value for Mann-Whitney U test (1st PC): 1.866977e-05
#'
print.cmpoutput <- function(x, ...) {

  if (length(unique(x$factors)) == 2) {
    test_names <- c("t-test", "Mann-Whitney U test")
  } else {
    test_names <- c("ANOVA test", "Kruskal-Wallis test")
  }

  if (length(x$npcs) == 1) {
    chopen <- ""
    chclose <- ""
  } else {
    chopen <- "["
    chclose <- "]"
  }

  cat("Output name:", x$name, "\n")
  cat("Number of PCs which explain ", chopen,
      paste(x$ve * 100, collapse = ", "), chclose,
      "% of variance: ", chopen, paste(x$npcs, collapse = ", "), chclose,
      "\n", sep = "")
  cat("P-Value for MANOVA along ", chopen, paste(x$npcs, collapse = ", "),
      chclose, " dimensions: ", chopen,
      paste(sprintf("%g", x$p.values$manova), collapse = ", "),
      chclose, "\n", sep = "")
  cat("P-Value for", test_names[1], "(1st PC):",
      x$p.values$parametric[1], "\n")
  cat("P-Value for", test_names[2], "(1st PC):",
      x$p.values$nonparametric[1], "\n")
  cat("Adjusted p-Value for", test_names[1], "(1st PC):",
      x$p.values$parametric_adjusted[1], "\n")
  cat("Adjusted p-Value for", test_names[2], "(1st PC):",
      x$p.values$nonparametric_adjusted[1], "\n")

  invisible(x)

}

#' Summary method for comparison of simulation outputs
#'
#' Summary method for objects of class \code{cmpoutput}.
#'
#' @param object Object of class \code{cmpoutput}.
#' @param ... Currently ignored.
#'
#' @return A list with the following components:
#' \describe{
#'  \item{output.name}{Output name.}
#'  \item{num.pcs}{Number of principal components which explain \code{var.exp}
#'        percentage of variance.}
#'  \item{var.exp}{Minimum percentage of variance which must be explained by the
#'        number of principal components used for the MANOVA test.}
#'  \item{manova.pval}{\emph{P}-value of the MANOVA test.}
#'  \item{parametric.test}{Name of the used parametric test.}
#'  \item{parametric.pvals}{Vector of $p$-values returned by applying the
#'        parametric test to each principal component.}
#'  \item{parametric.pvals.adjusted}{Vector of $p$-values returned by applying
#'        the parametric test to each principal component, adjusted with the
#'        weighted Bonferroni procedure, percentage of explained variance used
#'        as weight.}
#'  \item{nonparametric.test}{Name of the used non-parametric test.}
#'  \item{nonparametric.pvals}{Vector of $p$-values returned by applying the
#'        non-parametric test to each principal component.}
#'  \item{nonparametric.pvals.adjusted}{Vector of $p$-values returned by
#'        applying the non-parametric test to each principal component, adjusted
#'        with the weighted Bonferroni procedure, percentage of explained
#'        variance used as weight.}
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
#' ## [1] 0.001073006 0.788283742 0.172055145 0.015365691 0.773125277 0.583483779
#' ## [7] 0.280891952 0.401184318 0.762945000 0.819349320 0.836000197 0.896432613
#' ## [13] 0.866970948 0.984995459 0.868931537 0.734285342 0.748539168 0.913470280
#' ## [19] 0.994646303 0.774178244
#' ##
#' ## $parametric.pvals.adjusted
#' ## [1] 0.002592554 1.000000000 1.000000000 0.210669418 1.000000000 1.000000000
#' ## [7] 1.000000000 1.000000000 1.000000000 1.000000000 1.000000000 1.000000000
#' ## [13] 1.000000000 1.000000000 1.000000000 1.000000000 1.000000000 1.000000000
#' ## [19] 1.000000000 1.000000000
#' ##
#' ## $nonparametric.test
#' ## [1] "Mann-Whitney"
#' ##
#' ## $nonparametric.pvals
#' ## [1] 0.0007252809 0.9117971811 0.3149992422 0.0185433761 0.7393643508 0.6842105263
#' ## [7] 0.2474506917 0.3526813744 0.7393643508 1.0000000000 0.7959362619 0.6305289138
#' ## [13] 0.6305289138 0.7959362619 0.9705124597 0.5787416917 0.6305289138 0.9705124597
#' ## [19] 0.8534283054 0.9117971811
#' ##
#' ## $nonparametric.pvals.adjusted
#' ## [1] 0.001752394 1.000000000 1.000000000 0.254236683 1.000000000 1.000000000
#' ## [7] 1.000000000 1.000000000 1.000000000 1.000000000 1.000000000 1.000000000
#' ## [13] 1.000000000 1.000000000 1.000000000 1.000000000 1.000000000 1.000000000
#' ## [19] 1.000000000 1.000000000
#'
summary.cmpoutput <- function(object, ...) {

  if (length(unique(object$factors)) == 2) {
    test_names <- c("t-test", "Mann-Whitney")
  } else {
    test_names <- c("ANOVA", "Kruskal-Wallis")
  }

  list(output.name = object$name,
       num.pcs = object$npcs,
       var.exp = object$ve,
       manova.pval = object$p.values$manova,
       parametric.test = test_names[1],
       parametric.pvals = object$p.values$parametric,
       parametric.pvals.adjusted = object$p.values$parametric_adjusted,
       nonparametric.test = test_names[2],
       nonparametric.pvals = object$p.values$nonparametric,
       nonparametric.pvals.adjusted = object$p.values$nonparametric_adjusted)
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
#' @param x Object of class \code{cmpoutput}.
#' @param ... Extra options passed to \code{\link{plot.default}}. The \code{col}
#' option determines the colors to use on observations of different groups
#' (scatter plot only).
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
plot.cmpoutput <- function(x, ...) {

  # Was a color specified?
  params <- list(...)
  if (exists("col", where = params)) {
    col <- params$col
    params$col <- NULL # We don't want any color in the barplots
  } else {
    col <- plotcols()
  }

  # Set mfrow graphical parameter to setup subplots
  par(mfrow = c(3,2))

  # Total number of PCs
  tpcs <- length(x$varexp)

  # Score plot (first two PCs)
  params_sp <- params
  params_sp$x <- x$scores[, 1]
  params_sp$y <- x$scores[, 2]
  params_sp$col <- col[as.numeric(x$factors)]
  params_sp$xlab <- paste("PC1 (", round(x$varexp[1] * 100, 2), "%)", sep = "")
  params_sp$ylab <- paste("PC2 (", round(x$varexp[2] * 100, 2), "%)", sep = "")
  params_sp$main <- "Score plot"
  do.call("plot.default", params_sp)

  # Explained variance bar plot
  params_ve <- params
  params_ve$height <- x$varexp
  params_ve$names.arg <- as.character(1:tpcs)
  params_ve$main <- "Explained variance by PC"
  params_ve$xlab <- "PC"
  params_ve$ylab <- "Var. exp. (%)"
  do.call("barplot", params_ve)

  # Parametric p-values bar plot
  params_ppv <- params
  params_ppv$height <- x$p.values$parametric
  params_ppv$names.arg <- as.character(1:tpcs)
  params_ppv$main <- "Parametric p-values by PC"
  params_ppv$xlab <- "PC"
  params_ppv$ylab <- "Prob."
  do.call("barplot", params_ppv)

  # Non-parametric p-values bar plot
  params_nppv <- params
  params_nppv$height <- x$p.values$nonparametric
  params_nppv$names.arg <- as.character(1:tpcs)
  params_nppv$main <- "Non-parametric p-values by PC"
  params_nppv$xlab <- "PC"
  params_nppv$ylab <- "Prob."
  do.call("barplot", params_nppv)

  # Parametric adjusted p-values bar plot
  params_papv <- params
  params_papv$height <- x$p.values$parametric_adjusted
  params_papv$names.arg <- as.character(1:tpcs)
  params_papv$main <- "Parametric p-values by PC (adjusted)"
  params_papv$xlab <- "PC"
  params_papv$ylab <- "Prob."
  do.call("barplot", params_papv)

  # Non-parametric adjusted p-values bar plot
  params_npapv <- params
  params_npapv$height <- x$p.values$nonparametric_adjusted
  params_npapv$names.arg <- as.character(1:tpcs)
  params_npapv$main <- "Non-parametric p-values by PC (adjusted)"
  params_npapv$xlab <- "PC"
  params_npapv$ylab <- "Prob."
  do.call("barplot", params_npapv)

  invisible(NULL)
}

#' Get assumptions for parametric tests performed on output comparisons.
#'
#' Get assumptions for parametric tests performed on output comparisons (i.e.
#' from objects of class \code{\link{cmpoutput}}).
#'
#' @param obj Object of class \code{cmpoutput}.
#'
#' @return Object of class \code{assumptions_cmpoutput} containing the
#' assumptions for parametric tests performed on an output comparison.
#' Basically a list containing the assumptions for the MANOVA (list of objects
#' of class \code{\link{assumptions_manova}}, one per explained variance) and
#' univariate parametric tests for each principal component (object of class
#' \code{\link{assumptions_paruv}}).
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
assumptions.cmpoutput <- function(obj) {

  # Create assumptions object
  assumptions <- list()
  class(assumptions) <- "assumptions_cmpoutput"

  # Get stuff from cmpoutput object
  npcs <- obj$npcs
  tpcs <- length(obj$varexp)
  factors <- obj$factors
  scores <- obj$scores
  nve <- length(npcs)

  # Allocate vector for Manova assumptions
  assumptions$manova <- vector(mode = "list", length = nve)

  # Determine Manova assumptions
  for (i in 1:nve) {
    if (npcs[i] > 1) {

      # Can only use manova if more than one variable
      assumptions$manova[[i]] <-
        assumptions_manova(scores[, 1:npcs[i]], factors)

    } else {

      # Only one variable, can't use manova
      assumptions$manova[[i]] <- NULL

    }
  }

  # Parametric test (t-test) for each PC
  assumptions$ttest <- assumptions_paruv(scores[, 1:tpcs], factors)

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
#'  \item \emph{t} bar plot for the \emph{p}-values of the Royston test with
#'        \emph{s} bars each, where \emph{t} is the number of unique MANOVA
#'        tests performed (one per requested explained variances) and \emph{s}
#'        is the number of groups being compared. These plots will not show if
#'        there is only one principal component being considered.
#'  \item One plot for the \emph{p}-values of the Box's M test, one bar
#'        (\emph{p}-value) per unique MANOVA tests performed  (one per requested
#'        explained variances).
#' }
#'
#' @param x Objects of class \code{assumptions_cmpoutput}.
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
plot.assumptions_cmpoutput <- function(x, ...) {

  # Multivariate assumptions
  if (exists("manova", where = x)) {

    # How many PCs did each MANOVA test?
    npcs <- sapply(x$manova,
                   function(y) {
                     if (!is.null(y)) {
                       dim(y$mvntest$NLOK@dataframe)[2]
                     } else {
                       1
                     }
                   })

    # We don't need to re-plot for the same number of PCs
    npcs[duplicated(npcs)] <- 1

    # How many are greater than 1?
    nmnvmvplt <- sum(npcs > 1)

    # Filter number of MANOVA multivariate assumptions to plot
    mnvmv <- x$manova[npcs > 1]

    # More than one? Then plot also Box's M p-values for different number of PCs
    if (length(mnvmv) > 1) {

      nmnvboxplt <- 1
      mnvboxp <- sapply(mnvmv, function(x) x$vartest$p.value)

    } else {

      # No Box's M p-values plot
      nmnvboxplt <- 0

    }

  } else {

    # No MANOVA assumptions plots
    nmnvmvplt <- 0
    nmnvboxplt <- 0

  }

  # How many plots?
  nplots <- length(x$uvntest) + 1 + nmnvmvplt + nmnvboxplt

  # Determine layout matrix side dimension
  side_dim <- ceiling(sqrt(nplots))

  # Setup subplot layout
  par(mfrow = c(side_dim, side_dim))

  # Plot univariate assumptions
  plot(x$ttest, ...)

  # Plot multivariate assumptions
  for (amnv in mnvmv) {
    plot(amnv, ...)
  }

  # Plot Box's M in case there are several p-values to plot
  if (nmnvboxplt) {

    # Was a color specified?
    params <- list(...)
    if (exists("col", where = params)) {
      col <- params$col
      params$col <- NULL
    } else {
      col <- c("darkgreen", "yellow", "red")
    }

    # Plot the p-values in a bar plot
    params$height <- mnvboxp
    params$main <- "Box's M test p-values"
    params$sub <- "Homogeneity of Covariance Matrices"
    params$xlab <- "Number of PCs"
    params$names.arg <- npcs[npcs > 1]
    params$ylab <- "Probability"
    params$col <- pvalcol(mnvboxp, col)
    do.call("barplot", params)

  }

  invisible(NULL)

}
