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
#' @param concat Create an additional, concatenated output? Ignored for sublists
#' passed in the \code{comps} which follow the second configuration.
#' @param centscal Method for centering and scaling outputs if \code{concat} is
#' TRUE. It can be one of "center", "auto", "range" (default), "iqrange",
#' "vast", "pareto" or "level". Centering and scaling is performed by the
#' \code{\link{centerscale}} function.
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
#'
#' # Create a micomp object from existing files and folders
#'
#' dir_nl_ok <-
#'   system.file("extdata", "nl_ok", package = "micompr")
#' dir_jex_ok <-
#'   system.file("extdata", "j_ex_ok", package = "micompr")
#' dir_jex_noshuff <-
#'   system.file("extdata", "j_ex_noshuff", package = "micompr")
#' dir_jex_diff <-
#'   system.file("extdata", "j_ex_diff", package = "micompr")
#' files <- "stats400v1*.tsv"
#'
#' mic <- micomp(7, 0.8,
#'               list(list(name = "NLOKvsJEXOK",
#'                         folders = c(dir_nl_ok, dir_jex_ok),
#'                         files = c(files, files),
#'                         lvls = c("NLOK", "JEXOK")),
#'                    list(name = "NLOKvsJEXNOSHUFF",
#'                         folders = c(dir_nl_ok, dir_jex_noshuff),
#'                         files = c(files, files),
#'                         lvls = c("NLOK", "JEXNOSHUFF")),
#'                    list(name = "NLOKvsJEXDIFF",
#'                         folders = c(dir_nl_ok, dir_jex_diff),
#'                         files = c(files, files),
#'                         lvls = c("NLOK", "JEXDIFF"))),
#'               concat = TRUE)
#'
#' # Create a micomp object from package datasets (i.e. grpoutputs objects)
#' # directly
#'
#' mic <- micomp(c("o1", "o2", "o3", "o4"), 0.9,
#'               list(list(name = "NLOKvsJEXOK", grpout = pphpc_ok),
#'                    list(name = "NLOKvsJEXNOSHUFF", grpout = pphpc_noshuff),
#'                    list(name = "NLOKvsJEXDIFF", grpout = pphpc_diff)))
#'
#' # Create a micomp object using manually inserted data
#'
#' mic <- micomp(6, 0.5,
#'               list(list(name = "NLOKvsJEXOK",
#'                         grpout = list(data = pphpc_ok$data,
#'                         factors = pphpc_ok$factors)),
#'                    list(name = "NLOKvsJEXNOSHUFF",
#'                         grpout = list(data = pphpc_noshuff$data,
#'                         factors = pphpc_noshuff$factors)),
#'                    list(name = "NLOKvsJEXDIFF",
#'                         grpout = list(data = pphpc_diff$data,
#'                         factors = pphpc_diff$factors))))
#'
micomp <- function(outputs, ve, comps, concat = F, centscal = "range", ...) {

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
        exists("files", where = comps[[i]])) {

      # First configuration: load data from files

      grpd_outputs[[i]] <-
        grpoutputs(outputs = outputs,
                   folders = unlist(comps[[i]]$folders),
                   files = unlist(comps[[i]]$files),
                   lvls = comps[[i]]$lvls,
                   concat = concat,
                   centscal = centscal,
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
      stop("Invalid 'comps' parameter")

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
#' @param x Object of class \code{\link{micomp}}.
#' @param ... Currently ignored.
#'
#' @return The argument \code{x}, invisibly, as for all \code{\link{print}}.
#' methods.
#'
#' @export
#'
#' @examples
#'
#' # A micomp object from package datasets (i.e. grpoutputs objects) directly
#'
#' micomp(c("outA", "outB", "outC", "outD"), 0.9,
#'               list(list(name = "Comp1", grpout = pphpc_ok),
#'                    list(name = "Comp2", grpout = pphpc_noshuff),
#'                    list(name = "Comp3", grpout = pphpc_diff)))
#'
#' ## ==== Comp1 ====
#' ##                        outA     outB     outC       outD
#' ## #PCs                3.00000  2.00000  4.00000  7.0000000
#' ## MNV                 0.26966  0.31754  0.31275  0.0022934
#' ## par.test            0.82070  0.46962  0.97096  0.4730614
#' ## nonpar.test         1.00000  0.57874  0.79594  0.3930481
#' ## par.test.adjust     1.00000  0.53452  1.00000  1.0000000
#' ## nonpar.test.adjust  1.00000  0.65873  1.00000  1.0000000
#' ## ==== Comp2 ====
#' ##                           outA       outB        outC        outD
#' ## #PCs                2.0000e+00  1.0000000  3.0000e+00  1.0000e+00
#' ## MNV                 3.1147e-10         NA  1.0851e-07          NA
#' ## par.test            2.5455e-06  0.0088667  1.1112e-03  2.0993e-18
#' ## nonpar.test         1.0825e-05  0.0089307  1.0500e-03  1.0825e-05
#' ## par.test.adjust     4.8876e-06  0.0095098  2.2202e-03  2.3114e-18
#' ## nonpar.test.adjust  2.0785e-05  0.0095784  2.0980e-03  1.1919e-05
#' ## ==== Comp3 ====
#' ##                           outA        outB        outC        outD
#' ## #PCs                1.0000e+00  1.0000e+00  2.0000e+00  3.0000e+00
#' ## MNV                         NA          NA  5.3807e-14  4.9585e-09
#' ## par.test            6.7272e-17  6.5777e-11  1.7047e-15  1.8089e-09
#' ## nonpar.test         1.0825e-05  1.0825e-05  1.0825e-05  1.0825e-05
#' ## par.test.adjust     6.8682e-17  6.6553e-11  2.1743e-15  2.1125e-09
#' ## nonpar.test.adjust  1.1052e-05  1.0953e-05  1.3807e-05  1.2642e-05
#'
print.micomp <- function(x, ...) {

  # Use summary to get the info to be printed
  smic <- summary(x)

  # Cycle through comparisons
  for (cmpname in names(smic)) {

    cat("====", cmpname, "====\n")
    print(smic[[cmpname]], digits = 5, print.gap = 2)

  }

  invisible(x)

}

#' Summary method for multiple comparisons of simulation output
#'
#' Summary method for objects of class \code{\link{micomp}}.
#'
#' @param object Object of class \code{\link{micomp}}.
#' @param ... Currently ignored.
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
#'  \item{par.test.adjust}{\emph{P}-value for the parametric test (first
#'        principal component), adjusted with the weighted Bonferroni procedure,
#'        percentage of explained variance used as weight.}
#'  \item{nonpar.test.adjust}{\emph{P}-value for the non-parametric test (first
#'        principal component), adjusted with the weighted Bonferroni procedure,
#'        percentage of explained variance used as weight.}
#' }
#'
#' @export
#'
#' @examples
#'
#' # A micomp object from package datasets (i.e. grpoutputs objects) directly
#'
#' summary(micomp(5, 0.85,
#'                list(list(name = "CompEq", grpout = pphpc_ok),
#'                     list(name = "CompNoShuf", grpout = pphpc_noshuff),
#'                     list(name = "CompDiff", grpout = pphpc_diff))))
#'
#' ## $CompEq
#' ##                         out1      out2      out3       out4      out5
#' ## #PCs               2.0000000 1.0000000 3.0000000 5.00000000 7.0000000
#' ## MNV                0.4122154        NA 0.4252224 0.04473072 0.5505714
#' ## par.test           0.8207035 0.4696199 0.9709602 0.47306136 0.3597001
#' ## nonpar.test        1.0000000 0.5787417 0.7959363 0.39304813 0.5787417
#' ## par.test.adjust    1.0000000 0.5345224 1.0000000 1.00000000 0.9293843
#' ## nonpar.test.adjust 1.0000000 0.6587251 1.0000000 1.00000000 1.0000000
#' ##
#' ## $CompNoShuf
#' ##                            out1        out2         out3         out4       out5
#' ## #PCs               2.000000e+00 1.000000000 2.000000e+00 1.000000e+00 7.00000000
#' ## MNV                3.114719e-10          NA 6.396187e-08           NA 0.18049297
#' ## par.test           2.545483e-06 0.008866718 1.111168e-03 2.099258e-18 0.05728007
#' ## nonpar.test        1.082509e-05 0.008930698 1.050034e-03 1.082509e-05 0.07525601
#' ## par.test.adjust    4.887558e-06 0.009509753 2.220151e-03 2.311375e-18 0.13788432
#' ## nonpar.test.adjust 2.078515e-05 0.009578372 2.098003e-03 1.191890e-05 0.18115591
#' ##
#' ## $CompDiff
#' ##                            out1         out2         out3         out4         out5
#' ## #PCs               1.000000e+00 1.000000e+00 2.000000e+00 1.000000e+00 5.000000e+00
#' ## MNV                          NA           NA 5.380703e-14           NA 1.242782e-06
#' ## par.test           6.727250e-17 6.577743e-11 1.704694e-15 1.808875e-09 1.823478e-08
#' ## nonpar.test        1.082509e-05 1.082509e-05 1.082509e-05 1.082509e-05 1.082509e-05
#' ## par.test.adjust    6.868160e-17 6.655293e-11 2.174285e-15 2.112548e-09 3.144908e-08
#' ## nonpar.test.adjust 1.105183e-05 1.095271e-05 1.380707e-05 1.264240e-05 1.866977e-05
#'
summary.micomp <- function(object, ...) {

  dims <- dim(object)
  ncomp <- dims[2]
  cmpnames <- colnames(object)

  smic <- list()

  # Cycle through comparisons
  for (i in 1:ncomp) {

    npcs <- sapply(object[, i], function(mc) return(mc$npcs[1]))
    p_mnv <- sapply(object[, i], function(mc) return(mc$p.values$manova[1]))
    p_par <- sapply(object[, i],
                    function(mc) return(mc$p.values$parametric[1]))
    p_npar <- sapply(object[, i],
                     function(mc) return(mc$p.values$nonparametric[1]))
    p_apar <- sapply(object[, i],
                     function(mc) return(mc$p.values$parametric_adjusted[1]))
    p_anpar <- sapply(object[, i],
                      function(mc)
                        return(mc$p.values$nonparametric_adjusted[1]))

    df <- data.frame(rbind(npcs, p_mnv, p_par, p_npar, p_apar, p_anpar),
                     stringsAsFactors = F,
                     row.names = c("#PCs", "MNV", "par.test", "nonpar.test",
                                   "par.test.adjust", "nonpar.test.adjust"))
    names(df) <- rownames(object)

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
#' @param x An object of class \code{\link{micomp}}.
#' @param ... Extra options passed to \code{\link{plot.default}}. The \code{col}
#' option determines the colors to use on observations of different groups
#'
#' @return None.
#'
#' @export
#'
#' @examples
#'
#' plot(micomp(c("SheepPop", "WolfPop", "GrassQty"), 0.95,
#'             list(list(name = "I", grpout = pphpc_ok),
#'                  list(name = "II", grpout = pphpc_noshuff),
#'                  list(name = "III", grpout = pphpc_diff))))
#'
plot.micomp <- function(x, ...) {

  # Was a color specified?
  params <- list(...)
  if (!exists("col", where = params)) {
    params$col <- plotcols()
  }
  col <- params$col

  # Useful variables
  dims <- dim(x)
  nout <- dims[1]
  ncomp <- dims[2]
  nplots <- nout * ncomp

  # Layout of subplots
  m <- matrix(1:(nplots + ncomp), nrow = ncomp, ncol = nout + 1, byrow = T)
  layout(mat = m)

  for (i in 1:ncomp) {

    # Get factors from the first output of the current comparison
    facts <- x[[1, i]]$factors

    # Set title and legend for current comparison
    plot(0, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, pos = 1, labels = paste("Comp. ", i))
    legend("center", legend = unique(facts), fill = col, horiz = F)

    for (j in 1:nout) {

      # Get data
      scores <- x[[j, i]]$scores
      varexp <- x[[j, i]]$varexp

      # Score plot (first two PCs)
      params$x <- scores[, 1]
      params$y <- scores[, 2]
      params$col <- col[as.numeric(facts)]
      params$xlab <- paste("PC1 (", round(varexp[1] * 100, 2), "%)", sep = "")
      params$ylab <- paste("PC2 (", round(varexp[2] * 100, 2), "%)", sep = "")
      params$main <- x[[j, i]]$name
      do.call("plot.default", params)

    }
  }

  invisible(NULL)

}

#' Get assumptions for parametric tests performed on each comparisons.
#'
#' Get assumptions for parametric tests performed on multiple comparisons (i.e.
#' from objects of class \code{\link{micomp}}).
#'
#' @param obj Object of class \code{\link{micomp}}.
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
#'
#' # Create a micomp object, use provided dataset
#' mic <- micomp(6, 0.8,
#'               list(list(name = "NLOKvsJEXOK", grpout = pphpc_ok),
#'                    list(name = "NLOKvsJEXNOSHUFF", grpout = pphpc_noshuff),
#'                    list(name = "NLOKvsJEXDIFF", grpout = pphpc_diff)))
#'
#' # Create an object containing the statistic tests evaluating the assumptions
#' # of the comparisons performed in the mic object
#' a <- assumptions(mic)
#'
assumptions.micomp <- function(obj) {
  micas <- lapply(obj, function(x) assumptions(x))
  dim(micas) <- dim(obj)
  colnames(micas) <- colnames(obj)
  rownames(micas) <- rownames(obj)
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
#' @param x Object of class \code{assumptions_micomp}.
#' @param ... Currently ignored.
#'
#' @return The argument \code{x}, invisibly, as for all \code{\link{print}}
#' methods.
#'
#' @export
#'
#' @examples
#'
#' # Create a micomp object, use provided dataset
#' mic <- micomp(c("SheepPop", "WolfPop", "GrassQty"), 0.7,
#'               list(list(name = "NLOKvsJEXOK", grpout = pphpc_ok),
#'                    list(name = "NLOKvsJEXNOSHUFF", grpout = pphpc_noshuff),
#'                    list(name = "NLOKvsJEXDIFF", grpout = pphpc_diff)))
#'
#' # Print the results (p-values) of the statistic tests evaluating the
#' # assumptions of the comparisons performed in the mic object
#' assumptions(mic)
#'
print.assumptions_micomp <- function(x, ...) {

  # Cycle through comparisons
  for (cmp in colnames(x)) {

    # Cycle through outputs
    for (out in rownames(x)) {

      # Show assumptions for current comparison/output

      cat("### Comp.:", cmp, "\n")
      cat("### Output:", out, "\n\n")

      toshow <- capture.output(x[[out, cmp]])

      cat(paste0("    ", toshow, "\n", collapse = ""))
      cat("\n")
    }

  }

  invisible(x)

}

#' Plot \emph{p}-values for testing the assumptions of the parametric tests used
#' in multiple output comparison
#'
#' Plot method for objects of class \code{assumptions_cmpoutput}
#' containing \emph{p}-values produced by testing the assumptions of the
#' parametric tests used for multiple output comparisons.
#'
#' Several bar plots are presented, one for each comparison and output
#' combination, showing the several statistical tests employed to verify
#' the assumptions of the parametric tests.
#'
#' @param x Object of class \code{assumptions_micomp}.
#' @param ... Extra options passed to \code{\link{barplot}}.
#'
#' @return None.
#'
#' @export
#'
#' @examples
#'
#' # Create a micomp object, use provided dataset
#' mic <- micomp(6, 0.65,
#'               list(list(name = "NLOKvsJEXOK", grpout = pphpc_ok),
#'                    list(name = "NLOKvsJEXNOSHUFF", grpout = pphpc_noshuff),
#'                    list(name = "NLOKvsJEXDIFF", grpout = pphpc_diff)))
#'
#' # Plot the p-values of the statistic tests evaluating the assumptions of the
#' # comparisons performed in the mic object
#' plot(assumptions(mic))
#'
plot.assumptions_micomp <- function(x, ...) {

  # Get the assumptions summary
  sm <- summary(x)

  # Set layout for plots
  par(mfcol = dim(x))

  # Cycle through comparisons
  for (cmp in colnames(sm)) {

    # Cycle through outputs
    for (out in rownames(sm)) {

      # Current output comparison
      sco <- sm[[out, cmp]]
      par(mar = c(2, 4, 2, 4))

      # Determine names and values of bars in plot
      b1 <- NULL
      if (!is.null(sco$manova)) {
        b1 <- sapply(sco$manova, function(x) x)
        names(b1) <- mapply(function(x, y) paste(x, y),
                            row(sco$manova, T), col(sco$manova, T))
      }
      b2 <- sapply(sco$ttest, function(x) x)
      names(b2) <- mapply(function(x, y) paste(x, y),
                          row(sco$ttest, T), col(sco$ttest, T))
      b <- c(b1, b2)

      # Set bar plot parameters
      params <- list()
      params$height <- b
      params$col <- pvalcol(b, c("darkgreen", "yellow", "red"))
      params$beside <- T
      params$main <- paste(cmp, out)
      params$las = 2
      params$horiz = TRUE
      do.call("barplot", params)
    }
  }

  invisible(NULL)

}

#' Summary method for the assumptions of parametric tests used in multiple
#' comparisons of simulation output
#'
#' Summary method for objects of class \code{assumptions_micomp}, which
#' contain the assumptions for the parametric tests used in multiple comparisons
#' of simulation output.
#'
#' @param object Object of class \code{assumptions_micomp}.
#' @param ... Currently ignored.
#' @param tnpcs Number of principal components to summarize for the
#' \emph{t}-test.
#'
#' @return A multi-dimensional list of \code{assumptions_cmpoutput} summaries.
#' Rows are associated with individual outputs, while columns are associated
#' with separate comparisons.
#'
#' @export
#'
#' @examples
#'
#' # Create a micomp object, use provided dataset
#' mic <- micomp(5, 0.8,
#'               list(list(name = "NLOKvsJEXOK", grpout = pphpc_ok),
#'                    list(name = "NLOKvsJEXNOSHUFF", grpout = pphpc_noshuff)),
#'               concat = TRUE)
#'
#' # Get the assumptions summary
#' sam <- summary(assumptions(mic))
#'
summary.assumptions_micomp <- function(object, ..., tnpcs = 1) {

  # Get summaries from all underlying assumptions_cmpoutput objects
  assum <- lapply(object, function(cmp) summary(cmp, tnpcs = tnpcs))

  # Set the dimensions of the output object to be the same as the input object
  dim(assum) <- dim(object)

  # As well as the row and column names
  rownames(assum) <- rownames(object)
  colnames(assum) <- colnames(object)

  assum

}
