#' Model-independent comparison of simulation output
#'
#' Perform multiple model-independent comparisons of simulation outputs.
#'
#' @param outputs A vector with the labels of each output, or an integer with
#' the number of outputs (in which case output labels will be assigned
#' automatically).
#' @param ve Percentage (between 0 and 1) of variance explained by the \emph{q}
#' principal components (i.e. number of dimensions) used in MANOVA.
#' @param comps A list of lists, where each list contains information regarding
#' an individual comparison. Each list can have one of two configurations:
#' \enumerate{
#'   \item Lists with the first configuration are used to load data from files,
#'         and require the following fields:
#'     \describe{
#'       \item{name}{A string specifying the comparison name.}
#'       \item{folders}{Vector of folder names where to read files from. These
#'             are recycled if \code{length(folders) < length(files)}.}
#'       \item{files}{Vector of filenames (with wildcards) to load in each
#'             folder.}
#'       \item{lvls}{Vector of factor (group) names, must be the same length as
#'             \code{files}, i.e. each file set will be associated with a
#'             different group. If not given, default group names will be set.}
#'     }
#'   \item Lists with the second configuration are used to load data from
#'         environment variables, and require the following fields:
#'     \describe{
#'       \item{name}{A string specifying the comparison name.}
#'       \item{grpout}{Either an object of class \code{\link{grpoutputs}} or a
#'             list with the following two fields:
#'         \describe{
#'           \item{data}{List of all outputs, where tags correspond to output
#'                 names and values correspond to the output data. Output data
#'                 is a \emph{n} x \emph{m} matrix, where \emph{n} is the total
#'                 number of output observations and \emph{m} is the number of
#'                 variables (i.e. output length). }
#'           \item{factors}{Factors (or groups) associated with each
#'                 observation.}
#'         }
#'       }
#'     }
#' }
#' @param concat Create an additional, concatenated output?
#' @param ... Options passed to \code{\link{read.table}}, which is used to read
#' the files specified in lists using the first configuration in the
#' \code{comp} parameter.
#'
#' @return An object of class \code{\link{micomp}}, which is a multi-dimensional
#' list of \code{cmpoutput} objects. Rows are associated with individual
#' outputs, while columns are associated with separate comparisons.
#'
#' @export
#'
#' @examples
#' NULL
micomp <- function(outputs, ve, comps, concat = F, ...) {

  # Determine number of comparisons
  ncomp <- length(comps)
  cmp_names <- vector(mode = "character", length = ncomp)

  # Did the user specify output names? Or should we provide some default names?
  if (length(outputs) == 1) {
    nout <- outputs
    outputs <- paste("out", 1:nout, sep = "")
  } else {
    nout <- length(outputs)
  }

  # List of grouped outputs for each comparison
  grpd_outputs <- list()

  # Results of each comparison for each output
  comp_res <- vector("list", length = nout * ncomp)
  dim(comp_res) <- c(nout, ncomp)

  # Group outputs for each comparison
  for (i in 1:ncomp) {

    # What kind of configuration does the current comparison have? File or
    # matrix?
    if (exists("name", where = comps[[i]]) &&
        exists("folders", where = comps[[i]]) &&
        exists("files", where = comps[[i]]) &&
        exists("lvls", where = comps[[i]])) {

      # First configuration: load data from files

      grpd_outputs[[i]] <-
        grpoutputs(outputs = outputs,
                   folders = unlist(comps[[i]]$folders),
                   files = unlist(comps[[i]]$files),
                   lvls = comps[[i]]$lvls,
                   concat = concat,
                   ...)

    } else if (exists("name", where = comps[[i]]) &&
               exists("grpout", where = comps[[i]])) {

      # Second configuration: load data from environment variables

      # Is the object in the grpout field of class grpoutputs?
      if (is(comps[[i]], "grpoutputs")) {

        # Yes, so use it directly
        grpd_outputs[[i]] <- comps[[i]]

      } else {

        # No, so build it using the provided data
        fcts <- comps[[i]]$grpout$factors
        grpd_outputs[[i]] <- structure(
          list(data = comps[[i]]$grpout$data,
               groups = sapply(unique(fcts),
                               function(onefac, facts) sum(facts == onefac),
                               fcts),
               factors = fcts,
               lvls = levels(unique(fcts)),
               concat = F),
          class = "grpoutputs")

      }

    } else {

      # Unknown configuration, throw error
      stop("Invalid object passed as ...")

    }

    # Keep comparison names
    cmp_names[i] <-
      if (!is.null(comps[[i]]$name)) {
        comps[[i]]$name
      } else {
        paste("Comp", i)
      }


  }

  # Cycle through each output
  for (i in 1:nout) {

    # Cycle through comparisons
    for (j in 1:ncomp) {

      comp_res[[i, j]] <-
        cmpoutput(outputs[i], ve,
                  grpd_outputs[[j]]$data[[i]],
                  grpd_outputs[[j]]$factors)
    }

  }

  colnames(comp_res) <- cmp_names
  rownames(comp_res) <- outputs
  class(comp_res) <- "micomp"
  comp_res

}

#' Print information about multiple comparisons of simulation output
#'
#' Print information about objects of class \code{\link{micomp}}.
#'
#' @param mcmp Object of class \code{\link{micomp}}.
#'
#' @return The argument \code{mcmp}, invisibly, as for all \code{\link{print}}.
#' methods.
#'
#' @export
#'
#' @examples
#' NULL
#'
print.micomp <- function(mcmp) {

  # Use summary to get the info to be printed
  smic <- summary(mcmp)

  # Cycle through comparisons
  for (cmpname in names(smic)) {

    cat("====", cmpname, "====\n")
    print(smic[[cmpname]], digits = 5, print.gap = 2)

  }

  invisible(mcmp)

}

#' Summary method for multiple comparisons of simulation output
#'
#' Summary method for objects of class \code{\link{micomp}}.
#'
#' @param mcmp Object of class \code{\link{micomp}}.
#'
#' @return A list in which each component is associated with a distinct
#' comparison. Each component contains a data frame, in which columns represent
#' individual simulation outputs and rows have information about the outputs.
#' More specifically, each data frame has four rows with the following
#' information:
#' \describe{
#'  \item{#PCs}{Number of principal components required to explain the
#'        percentage of variance specified when creating the
#'        \code{\link{micomp}} object.}
#'  \item{MNV}{\emph{P}-value for the MANOVA test (\code{#PCs}).}
#'  \item{par.test}{\emph{P}-value for the parametric test (first principal
#'        component).}
#'  \item{nonpar.test}{\emph{P}-value for the non-parametric test (first
#'        principal component).}
#' }
#'
#' @export
#'
#' @examples
#' NULL
#'
summary.micomp <- function(mcmp) {

  dims <- dim(mcmp)
  ncomp <- dims[2]
  cmpnames <- colnames(mcmp)

  smic <- list()

  # Cycle through comparisons
  for (i in 1:ncomp) {

    npcs <- sapply(mcmp[, i], function(mc) return(mc$npcs))
    p_mnv <- sapply(mcmp[, i], function(mc) return(mc$p.values$manova))
    p_par <- sapply(mcmp[, i], function(mc) return(mc$p.values$parametric[1]))
    p_npar <- sapply(mcmp[, i],
                     function(mc) return(mc$p.values$nonparametric[1]))


    df <- data.frame(rbind(npcs,p_mnv,p_par,p_npar), stringsAsFactors = F,
                     row.names = c("#PCs", "MNV", "par.test", "nonpar.test"))
    names(df) <- rownames(mcmp)

    smic[[cmpnames[i]]] <- df

  }

  smic

}

#' Plot projection of output observations on the first two dimensions of the
#' principal components space
#'
#' For each comparison and output combination, draw a scatter plot containing
#' the projection of output observations on the first two dimensions of the
#' principal components space.
#'
#' @param mcmp An object of class \code{\link{micomp}}.
#' @param col Vector of colors to use on observations of different groups.
#' @param ... Extra options passed to \code{\link{plot.default}}.
#'
#' @return None.
#'
#' @export
#'
#' @examples
#' NULL
#'
plot.micomp <- function(mcmp, col = micompr:::plotcols(), ...) {

  dims <- dim(mcmp)
  nout <- dims[1]
  ncomp <- dims[2]
  nplots <- nout * ncomp

  m <- matrix(1:(nplots + ncomp), nrow = ncomp, ncol = nout + 1, byrow = T)
  layout(mat = m)

  for (i in 1:ncomp) {

    # Get factors from the first output of the current comparison
    facts <- mcmp[[1,i]]$factors

    # Set title and legend for current comparison
    plot(0, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1,1, pos = 1,labels = paste("Comp. ", i))
    legend("center", legend = unique(facts), fill = col, horiz = F)

    for (j in 1:nout) {

      # Get data
      scores <- mcmp[[j,i]]$scores
      varexp <- mcmp[[j,i]]$varexp

      # Score plot (first two PCs)
      plot.default(scores[,1], scores[,2], col = col[as.numeric(facts)],
                   xlab = paste("PC1 (", round(varexp[1] * 100, 2), "%)",
                                sep = ""),
                   ylab = paste("PC2 (", round(varexp[2] * 100, 2), "%)",
                                sep = ""),
                   main = mcmp[[j,i]]$name, ...)
    }
  }

  invisible(NULL)

}

#' Get assumptions for parametric tests performed on each comparisons.
#'
#' Get assumptions for parametric tests performed on multiple comparisons (i.e.
#' from objects of class \code{\link{micomp}}).
#'
#' @param mcmp Object of class \code{\link{micomp}}.
#' @param ... Currently ignored.
#'
#' @return Object of class \code{assumptions_micomp} containing the
#' assumptions for parametric tests performed for the multiple comparisons held
#' by the \code{mcmp} object. This object is a multi-dimensional list of
#' \code{assumptions_cmpoutput} objects. Rows are associated with individual
#' outputs, while columns are associated with separate comparisons.
#'
#' @export
#'
#' @examples
#' NULL
#'
assumptions.micomp <- function(mcmp, ...) {
  micas <- lapply(mcmp, function(x) x$assumptions)
  dim(micas) <- dim(mcmp)
  colnames(micas) <- colnames(mcmp)
  rownames(micas) <- rownames(mcmp)
  class(micas) <- "assumptions_micomp"
  micas
}

#' Print information about the assumptions concerning the parametric tests
#' performed on multiple comparisons of simulation output
#'
#' Print information about objects of class \code{assumptions_micomp}, which
#' represent the assumptions concerning the parametric tests performed on
#' multiple comparisons of simulation output.
#'
#' @param micas Object of class \code{assumptions_micomp}.
#' @param ... Currently ignored.
#'
#' @return The argument \code{micas}, invisibly, as for all \code{\link{print}}
#' methods.
#'
#' @export
#'
#' @examples
#' NULL
#'
print.assumptions_micomp <- function(micas, ...) {

  sm <- summary(micas)

  # Cycle through comparisons
  for (cmp in names(sm)) {

    cat("==== ", cmp, "====\n")
    print(sm[[cmp]], digits = 5, print.gap = 2)
    cat("\n")

  }

}

#' Plot \emph{p}-values for testing the assumptions of the parametric tests used
#' in multiple output comparison
#'
#' Plot method for objects of class \code{assumptions_cmpoutput}
#' containing \emph{p}-values produced by testing the assumptions of the
#' parametric tests used for multiple output comparisons.
#'
#' Several bar plots are presented, one for each comparison, showing the
#' \emph{p}-values yielded by the Shapiro-Wilk (\code{\link{shapiro.test}}) and
#' Royston tests (\code{\link[MVN]{roystonTest}}) for univariate and
#' multivariate normality of each group, respectively, and for the Bartlett
#' (\code{\link{bartlett.test}}) and Box's M (\code{\link[biotools]{boxM}}) for
#' testing homogeneity of variances and of covariance matrices, respectively.
#' \emph{P}-values are aggregated by output in each plot. Note that the
#' \emph{p-values} plotted for the Shapiro-Wilk and Bartlett tests correspond to
#' group observations along the first principal component.
#'
#' @param micas Object of class \code{assumptions_micomp}.
#' @param col Vector of colors to use for different tests (and groups in the
#' case of normality tests).
#' @param ... Extra options passed to \code{\link{barplot}}.
#'
#' @return None.
#'
#' @export
#'
#' @examples
#' NULL
plot.assumptions_micomp <- function(micas, col = micompr:::plotcols(), ...) {

  sm <- summary(micas)

  dims <- dim(micas)
  ncomp <- dims[2]
  # One plot for each output/comparison pair  + 1 for the legend
  nplots <- ncomp + 1

  # Plot matrix side dimension
  side_dim <- ceiling(sqrt(nplots))

  par(mfrow = c(side_dim, side_dim))

  # Cycle through comparisons
  for (cmp in names(sm)) {
    par(mar = rep(2, 4))
    barplot(sm[[cmp]], col = col, beside = T, main = cmp, ...)
  }

  # Show legend
  plot(0, type = "n", axes = FALSE, xlab = "", ylab = "")
  legend("top", legend = rownames(sm[[1]]), fill = col)

  invisible(NULL)

}

#' Summary method for the assumptions of parametric tests used in multiple
#' comparisons of simulation output
#'
#' Summary method for objects of class \code{assumptions_micomp}, which
#' contain the assumptions for the parametric tests used in multiple comparisons
#' of simulation output.
#'
#' @param micas Object of class \code{assumptions_micomp}.
#'
#' @return A list in which each component is associated with a distinct
#' comparison. Each component contains a data frame, in which columns represent
#' individual simulation outputs and rows have information about the assumptions
#' of the parametric tests used in each output. More specifically, each data
#' frame has rows with the following information:
#' \describe{
#'  \item{Royston(\emph{group})}{\emph{s} rows, one per group, with the
#'        \emph{p}-value yielded by the Royston test
#'        (\code{\link[MVN]{roystonTest}}) for the respective group.}
#'  \item{BoxM(Var.)}{One row with the \emph{p}-value yielded by Box's M test
#'        (\code{\link[biotools]{boxM}}).}
#'  \item{Shapiro-Wilk(\emph{group})}{\emph{s} rows, one per group, with the
#'        \emph{p}-value yielded by the Shapiro-Wilk test
#'        (\code{\link{shapiro.test}}) for the respective group.}
#'  \item{Bartlett(Var.)}{One row with the \emph{p}-value yielded by Bartlett's
#'        test (\code{\link{bartlett.test}}).}
#' }
#'
#' @export
#'
#' @examples
#' NULL
#'
summary.assumptions_micomp <- function(micas) {

  dims <- dim(micas)
  ncomp <- dims[2]

  all <- list()
  cmpnames <- colnames(micas)

  # Cycle through comparisons
  for (i in 1:ncomp) {

    # Get the p-values for the MANOVA assumptions
    mnv <- lapply(micas[,i], function(ma) {
      # Was MANOVA performed?
      if (exists("manova", where = ma)) {
        # Get the Royston test p-values
        pvals <- sapply(ma$manova$mvntest, function(x) return(x@p.value))
        names(pvals) <- paste("Royston(", names(pvals), ")", sep = "")
        # Get the Box test p-values
        pvals <- c(pvals, `BoxM(Var.)` = ma$manova$vartest$p.value)
      } else {
        # Number of compared models
        ncmpmod <- length(ma$ttest$uvntest)
        # Set a vector of NAs (+1 for the Box test p-value)
        pvals <- rep(NA, ncmpmod + 1)
      }
      return(pvals)
    })

    # Get the p-values for for t-test assumptions
    ttst <- lapply(micas[, i], function(ma) {
      pvals <- sapply(ma$ttest$uvntest, function(x) return(x[[1]]$p.value))
      names(pvals) <- paste("Shapiro-Wilk(", names(pvals), ")", sep = "")
      pvals <- c(pvals, `Bartlett(Var.)` = ma$ttest$vartest[[1]]$p.value)
      return(pvals)
    })

    # Merge...
    mrgd <- mapply(function(x,y) c(x,y), mnv, ttst)
    #... and save to list
    all[[cmpnames[i]]] <- mrgd

  }

  all
}
