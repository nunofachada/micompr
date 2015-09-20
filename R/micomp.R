#' micomp...
#'
#' @param nout
#' @param nvars
#' @param ve
#' @param comps
#'
#' @return Some stuff
#' @export
#'
#' @examples #' micomp()
micomp <- function(outputs, nvars, ve, ..., concat=F) {

  # Put comparisons in a list
  comps <- list(...)

  # Determine number of comparions
  ncomp <- length(comps)
  cmp_names = vector(mode="character", length=ncomp)

  #
  if (length(outputs) == 1) {
    nout <- outputs
    outputs <- paste("out", 1:nout, sep="")
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

    grpd_outputs[[i]] <-
      grpoutputs(outputs=outputs,
                 nvars=nvars,
                 folders=unlist(comps[[i]]$folders),
                 files=unlist(comps[[i]]$files),
                 lvls=comps[[i]]$lvls,
                 concat=concat)

    cmp_names[i] <-
      if (!is.null(comps[[i]]$name)) { comps[[i]]$name }
      else { paste("Comp", i) }

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

#' Title
#'
#' @param mcmp
#'
#' @return todo
#' @export
#'
#' @examples #' todo()
print.micomp <- function(mcmp) {

  smic <- summary(mcmp)

  # Cycle through comparisons
  for (cmpname in names(smic)) {

    cat("====", cmpname, "====\n")
    print(smic[[cmpname]], digits=5, print.gap=2)

  }

}

#' Title
#'
#' @param mcmp
#'
#' @return todo
#' @export
#'
#' @examples #' todo()
summary.micomp <- function(mcmp) {

  dims <- dim(mcmp)
  nout <- dims[1]
  ncomp <- dims[2]
  cmpnames <- colnames(mcmp)

  smic <- list()

  # Cycle through comparisons
  for (i in 1:ncomp) {

    npcs <- sapply(mcmp[,i], function (mc) return (mc$npcs))
    p_mnv <- sapply(mcmp[,i], function (mc) return (mc$p.values$manova))
    p_par <- sapply(mcmp[,i], function (mc) return (mc$p.values$parametric[1]))
    p_npar <- sapply(mcmp[,i], function (mc) return (mc$p.values$nonparametric[1]))


    df <- data.frame(rbind(npcs,p_mnv,p_par,p_npar), stringsAsFactors = F,
                     row.names = c("#PCs", "MNV", "Par.test", "NonParTest"))
    names(df) <- rownames(mcmp)

    smic[[cmpnames[i]]] <- df

  }

  smic

}


# TODO The color vector could be some kind of global
#' Title
#'
#' @param mcmp
#' @param col
#' @param ...
#'
#' @return todo
#' @export
#'
#' @examples #' todo()
plot.micomp <- function(mcmp, col=micomp:::plotcols(), ...) {

  dims <- dim(mcmp)
  nout <- dims[1]
  ncomp <- dims[2]
  nplots = nout * ncomp

  m <- matrix(1:(nplots + ncomp), nrow=ncomp, ncol=nout+1, byrow=T)
  layout(mat=m)

  for (i in 1:ncomp) {

    # Get factors from the first output of the current comparison
    facts <- mcmp[[1,i]]$factors

    # Set title and legend for current comparison
    plot(0, type = "n", axes=FALSE, xlab="", ylab="")
    text(1,1, pos=1,labels=paste("Comp. ", i))
    legend("center", legend=unique(facts), fill=col, horiz=F)

    for (j in 1:nout) {

      # Get data
      scores <- mcmp[[j,i]]$scores
      varexp <- mcmp[[j,i]]$varexp

      # Score plot (first two PCs)
      #par(mar = rep(2, 4))
      plot.default(scores[,1], scores[,2], col=col[as.numeric(facts)],
                   xlab=paste("PC1 (", round(varexp[1] * 100, 2), "%)", sep = ""),
                   ylab=paste("PC2 (", round(varexp[2] * 100, 2), "%)", sep = ""),
                   main=mcmp[[j,i]]$name, ...)
    }
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
assumptions.micomp <- function(obj, ...) {
  micas <- lapply(obj, function(x) x$assumptions)
  dim(micas) <- dim(obj)
  colnames(micas) <- colnames(obj)
  rownames(micas) <- rownames(obj)
  class(micas) <- "assumptions_micomp"
  micas
}

#' Title
#'
#' @param micas
#' @param ...
#'
#' @return todo
#' @export
#'
#' @examples #' todo
print.assumptions_micomp <- function(micas, ...) {

  sm <- summary(micas)

  # Cycle through comparisons
  for (cmp in names(sm)) {

     cat("==== ", cmp, "====\n")
     print(sm[[cmp]], digits=5, print.gap=2)
     cat("\n")

  }

}

#' Title
#'
#' @param micas
#' @param ...
#'
#' @return todo
#' @export
#'
#' @examples #' todo
plot.assumptions_micomp <- function(micas, col=micomp:::plotcols(), ...) {

  sm <- summary(micas)

  dims <- dim(micas)
  nout <- dims[1]
  ncomp <- dims[2]
  # One plot for each output/comparison pair  + 1 for the legend
  nplots = ncomp + 1

  # Plot matrix side dimension
  side_dim <- ceiling(sqrt(nplots))

  par(mfrow=c(side_dim, side_dim))

  # Cycle through comparisons
  for (cmp in names(sm)) {
    par(mar = rep(2, 4))
    barplot(sm[[cmp]], col=col, beside=T, main=cmp, ...)
  }

  plot(0, type = "n", axes=FALSE, xlab="", ylab="")
  legend("top", legend=rownames(sm[[1]]), fill=col)

}

#' Title
#'
#' @param micas
#' @param ...
#'
#' @return todo
#' @export
#'
#' @examples #' todo
summary.assumptions_micomp <- function(micas, ...) {

  dims <- dim(micas)
  nout <- dims[1]
  ncomp <- dims[2]

  all = list()
  cmpnames <- colnames(micas)

  # Cycle through comparisons
  for (i in 1:ncomp) {

    # Get the p-values for the MANOVA assumptions
    mnv <- lapply(micas[,i], function (ma) {
      # Was MANOVA performed?
      if (exists('manova', where=ma)) {
        # Get the Royston test p-values
        pvals <- sapply(ma$manova$mvntest, function(x) return(x@p.value))
        names(pvals) <- paste("Royston(",names(pvals),")", sep="")
        # Get the Box test p-values
        pvals <- c(pvals, `BoxM(Var.)`=ma$manova$vartest$p.value)
      } else {
        # Number of compared models
        ncmpmod <- length(ma$ttest$uvntest)
        # Set a vector of NAs (+1 for the Box test p-value)
        pvals <- rep(NA, ncmpmod + 1)
      }
      return(pvals)
    })

    # Get the p-values for for t-test assumptions
    ttst <- lapply(micas[,i], function (ma) {
      pvals <- sapply(ma$ttest$uvntest, function(x) return(x[[1]]$p.value))
      names(pvals) <- paste("Shapiro-Wilk(",names(pvals),")", sep="")
      pvals <- c(pvals, `Bartlett(Var.)`=ma$ttest$vartest[[1]]$p.value)
      return(pvals)
    })

    # Merge...
    mrgd <- mapply(function(x,y) c(x,y), mnv, ttst)
    #... and save to list
    all[[cmpnames[i]]] <- mrgd

  }

  all
}
