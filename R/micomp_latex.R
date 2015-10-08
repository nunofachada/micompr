#' Concatenate strings without any separator characters
#'
#' Concatenate strings without any separator characters.
#'
#' This function simply calls \code{\link{paste}} with the \code{sep} and
#' \code{collapse} options set to \code{""}.
#'
#' @param ... one or more \R objects, to be converted to character vectors.
#'
#' @return A character vector of the concatenated values without any separator
#' characters.
#'
#' @keywords internal
#'
#' @examples
#' micomp:::pst("a","b","c",c("a","b","c"))
#' # [1] "abcaabcbabcc"
#'
pst <- function(...) {
  paste(..., sep = "", collapse = "")
}

#' Format a \emph{p}-value for printing in a \code{LaTeX} table.
#'
#' Format a \emph{p}-value for printing in a \code{LaTeX} table. Requires the
#' \emph{ulem} \code{LaTeX} package for underlining the \emph{p}-values.
#'
#' @param pval Numeric value between 0 and 1.
#'
#' @return A string representing the formatted \code{pval}.
#'
#' @keywords internal
#'
#' @examples
#' micomp:::pvalf(0.1)
#' # [1] "0.100"
#' micomp:::pvalf(0.000001)
#' # [1] "\\uuline{1e-06}"
#'
pvalf <- function(pval) {

  fval <- ifelse(pval > 0.0005,
                 formatC(pval, format = "f", digits = 3),
                 formatC(pval, format = "e", digits = 0))
  fval <- ifelse(pval < 0.01,
                 paste("\\uuline{", fval, "}", sep = ""),
                 ifelse(pval < 0.05,
                        paste("\\uline{", fval, "}", sep = ""),
                        fval))
  fval
}

#' Simple \code{TikZ} scatter plot
#'
#' Create a simple 2D \code{TikZ} scatter plot, useful for plotting PCA data.
#'
#' This function creates a simple \code{TikZ} 2D scatter plot within a
#' \code{tikzpicture} environment. The points are plotted on a normalized
#' figure with \emph{x} and \emph{y} axes bounded between [-1,1].
#'
#' @param data Data to plot, \emph{m} x 2 numeric matrix, where \emph{m} is the
#' number of observations or points to plot.
#' @param factors Factors determining to which group observations in \code{data}
#' belong to.
#' @param marks Character vector determining how to draw the points in
#' \code{TikZ}, for example: \code{
#' c("mark=square*,mark options={color=red},mark size=0.8pt",
#'   "mark=*,mark size=0.6pt",
#'   "mark=o,mark size=0.7pt")}.
#' @param tscale The \code{scale} property of the \code{TikZ} figure.
#' @param axes.color Axes color (must be a \code{LaTeX}/\code{TikZ} color).
#'
#' @return A string containing the \code{TikZ} figure code for plotting the
#' specified \code{data}.
#'
#' @export
#'
#' @examples
#' tikzscat(rbind(c(1.5, 2), c(0.5, 1)), factor(c(1,2)),
#'          c("mark=square*,mark options={color=red},mark size=0.8pt",
#'            "mark=*,mark size=0.6pt"),
#'          6)
#' # [1] "\\begin{tikzpicture}[scale=6] \\path (-1.2,-1.2) (1.2,1.2);
#' # \\draw[very thin,color=gray] (0,1.1)--(0,-1.1);
#' # \\draw[very thin,color=gray] (1.1,0)--(-1.1,0);
#' # \\path plot[mark=square*,mark options={color=red},mark size=0.8pt]
#' # coordinates { (0.750,1.000)};
#' # \\path plot[mark=*,mark size=0.6pt] coordinates { (0.250,0.500)};
#' # \\end{tikzpicture}"
#'
tikzscat <- function(data, factors, marks, tscale, axes.color = "gray") {

  # Only two first dimensions
  data <- data[, 1:2]

  # Normalize
  data <- data / max(abs(data))

  # Unique groups
  ugrps <- unique(factors)

  # Begin tikzfigure
  figstr <- paste("\\begin{tikzpicture}[scale=", tscale, "] ",
                  "\\path (-1.2,-1.2) (1.2,1.2);",
                  "\\draw[very thin,color=", axes.color, "] ",
                  "(0,1.1)--(0,-1.1); ",
                  "\\draw[very thin,color=", axes.color, "] ",
                  "(1.1,0)--(-1.1,0);",
                  sep = "");

  # Cycle
  for (i in 1:length(ugrps)) {

    # Get points in group (make sure result is a matrix if only one point
    # available)
    pts_in_grp <- matrix(data = data[factors == ugrps[i], ],
                         nrow = sum(factors == ugrps[i]))

    # Begin plotting points in current group
    figstr <- sprintf("%s \\path plot[%s] coordinates {", figstr, marks[i])

    # Cycle points in group
    for (j in 1:dim(pts_in_grp)[1]) {
      figstr <- sprintf("%s (%5.3f,%5.3f)", figstr, pts_in_grp[j, 1],
                        pts_in_grp[j, 2]);
    }

    # End current group
    figstr <- sprintf("%s}; ", figstr);
  }


  # Close tikzpicture environment
  figstr <- sprintf("%s \\end{tikzpicture}", figstr);

  # Return figure
  figstr

}

tscat_apply <- function(cmps, marks, tscale) {

  scores <- lapply(cmps, function(x) x$scores)
  factors <- lapply(cmps, function(x) x$factors)
  plts <- mapply(tikzscat, scores, factors, MoreArgs = list(marks, tscale))
  paste("\\raisebox{-.5\\height}{\\resizebox {1.2cm} {1.2cm} {", plts, "}}")

}

#' Title
#'
#' @param mic
#' @param data.show
#' @param table.placement
#' @param latex.environments
#' @param booktabs
#' @param btalign
#' @param col.width
#' @param digits
#' @param pvalformat
#' @param marks
#' @param ...
#'
#' @return todo
#' @export
#'
#' @examples
#' NULL
toLatex.micomp <-
  function(mic,
           data.show = c("npcs", "mnvp", "parp", "nparp", "scoreplot"),
           table.placement = "ht",
           latex.environments = c("center"),
           booktabs = F,
           booktabs.cmalign = "l",
           caption = NULL,
           label = NULL,
           col.width = F,
           digits = 3,
           pvalformat = pvalf,
           scoreplot.marks = c(
             "mark=square*,mark options={color=red},mark size=0.8pt",
             "mark=*,mark size=0.6pt",
             "mark=o,mark size=0.7pt"),
           scoreplot.scale = 6,
           ...) {


    ndata <- length(data.show)
    hlines <- if (booktabs) {
      list(top = "\\toprule", mid = "\\midrule", bot = "\\bottomrule",
           c = pst("\\cmidrule(", booktabs.cmalign, ")"))
    } else {
      list(top = "\\hline", mid = "\\hline", bot = "\\hline", c = "\\cline")
    }

    nout <- dim(mic)[1]
    smic <- summary(mic)

    ltxtab <- list()
    idx <- 1

    ltxtab[[idx]] <- pst("\\begin{table}[", table.placement, "]")
    idx <- idx + 1

    ltxtab[[idx]] <- pst("\\begin{", latex.environments ,"}")
    idx <- idx + 1

    if (col.width) {
      ltxtab[[idx]] <- "\\resizebox{\\columnwidth}{!}{%"
      idx <- idx + 1
    }

    ltxtab[[idx]] <- pst("\\begin{tabular}{cl", pst(rep("r", nout)), "}")
    idx <- idx + 1

    ltxtab[[idx]] <- hlines$top
    idx <- idx + 1

    ltxtab[[idx]] <- pst("\\multirow{2}{*}{Comp.} & \\multirow{2}{*}{Test} & ",
                         "\\multicolumn{", nout, "}{c}{Outputs} \\\\")
    idx <- idx + 1

    ltxtab[[idx]] <- pst(hlines$c, "{3-", 2 + nout, "}")
    idx <- idx + 1

    ltxtab[[idx]] <- pst(" & & ", paste(rownames(mic), collapse = " & ",
                                        sep = ""),
                         "\\\\")
    idx <- idx + 1

    # Cycle through comparisons
    for (cmp in colnames(mic)) {

      # Put a midrule
      ltxtab[[idx]] <- hlines$mid
      idx <- idx + 1

      # Adjust formatting for specific number of significant digits
      smicf <- smic[[cmp]]

      # Multi-row with comparison name
      ltxtab[[idx]] <- pst("\\multirow{", ndata,"}{*}{", cmp, "}")
      idx <- idx + 1

      # Rows with comparison data
      for (cdata in data.show) {
        ltxtab[[idx]] <-
          switch(cdata,
                 npcs = pst(" & $\\#$PCs ",
                            pst(" & ", as.integer(smicf["#PCs",])), "\\\\"),
                 mnvp = pst(" & MNV ",
                            pst(" & ", pvalf(unlist(smicf["MNV",]))), "\\\\"),
                 parp = pst(" & $t$-test ",
                            pst(" & ",
                                pvalf(unlist(smicf["Par.test",]))), "\\\\"),
                 nparp = pst(" & MW  ",
                             pst(" & ",
                                 pvalf(unlist(smicf["NonParTest",]))), "\\\\"),
                 scoreplot = pst(" & PCS ",
                                 pst(" & ",
                                     tscat_apply(mic[,cmp], scoreplot.marks,
                                                 scoreplot.scale)),
                                 "\\\\"))
        idx <- idx + 1
      }

    }

    ltxtab[[idx]] <- hlines$bot
    idx <- idx + 1

    ltxtab[[idx]] <- "\\end{tabular}"
    idx <- idx + 1

    if (col.width) {
      ltxtab[[idx]] <- "} % resize box"
      idx <- idx + 1
    }


    if (!is.null(caption)) {
      ltxtab[[idx]] <- pst("\\caption{", caption,"}")
      idx <- idx + 1
    }

    if (!is.null(label)) {
      ltxtab[[idx]] <- pst("\\label{", label,"}")
      idx <- idx + 1
    }

    ltxtab[[idx]] <- pst("\\end{", rev(latex.environments) ,"}")
    idx <- idx + 1

    ltxtab[[idx]] <- "\\end{table}"
    idx <- idx + 1

    ltxtab <- unlist(ltxtab)
    class(ltxtab) <- "Latex"

    ltxtab

  }
