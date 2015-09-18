pst <- function(...) paste(..., sep="", collapse="")

pvalf <- function(pval) {
  
  fval <- ifelse(pval > 0.0005, 
                 formatC(pval, format="f", digits=3), 
                 formatC(pval, format="e", digits=0))
  fval <- ifelse(pval < 0.01, 
                 paste("\\uuline{", fval, "}", sep=""),
                 ifelse(pval < 0.05,
                        paste("\\uline{", fval, "}", sep=""),
                        fval))
}

tikscat <- function(data, factors, marks) {
  
  # Only two first dimensions
  data <- data[,1:2]
  
  # Normalize
  data <- data / max(abs(data))
  
  # Unique groups
  ugrps <- unique(factors)
  
  # Begin figure
  figstr <- "\\begin{tikzpicture}[scale=6] \\path (-1.2,-1.2) (1.2,1.2); \\draw[very thin,color=gray] (0,1.1)--(0,-1.1); \\draw[very thin,color=gray] (1.1,0)--(-1.1,0);"
  
  # Cycle 
  for (i in 1:length(ugrps)) {
    
    # Get points in group
    pts_in_grp = data[factors==ugrps[i],]
    
    # Begin plotting points in current group
    figstr <- sprintf("%s \\path plot[%s] coordinates {", figstr, marks[i])
    
    # Cycle points in group
    for (j in 1:dim(pts_in_grp)[1]) {
      figstr <- sprintf("%s (%5.3f,%5.3f)", figstr, pts_in_grp[j, 1], pts_in_grp[j, 2]);
    }
    
    # End current group
    figstr <- sprintf("%s}; ", figstr);
  }
  

  figstr <- sprintf("%s \\end{tikzpicture}", figstr);
  
}

tscat_apply <- function(cmps, marks) {
  
  scores <- lapply(cmps, function(x) x$scores)
  factors <- lapply(cmps, function(x) x$factors)
  plts <- mapply(tikscat, scores, factors, MoreArgs=list(marks))
  paste("\\raisebox{-.5\\height}{\\resizebox {1.2cm} {1.2cm} {", plts, "}}")
  
}

toLatex.micomp <- 
  function(mic,
           data.show=c("npcs", "mnvp", "parp", "nparp", "scoreplot"),
           table.placement="ht", 
           latex.environments=c("center"), 
           booktabs=F,
           btalign="l",
           col.width=F,
           digits=3,
           pvalformat=pvalf,
           marks=c('mark=square*,mark options={color=red},mark size=0.8pt', 
                   'mark=*,mark size=0.6pt',
                   'mark=o,mark size=0.7pt'),
           ...) {
    

    ndata <- length(data.show)
    hlines <- if (booktabs) {
      list(top="\\toprule", mid="\\midrule", bot="\\bottomrule", 
           c=pst("\\cmidrule(", btalign, ")"))
    } else {
      list(top="\\hline", mid="\\hline", bot="\\hline", c="\\cline")
    }
    
    nout <- dim(mic)[1]
    ncmp <- dim(mic)[2]
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
    
    ltxtab[[idx]] <- pst(hlines$c, "{3-", 2+nout, "}")
    idx <- idx + 1 
    
    ltxtab[[idx]] <- pst(" & & ", paste(rownames(mic), collapse=" & ", sep=""), 
                         "\\\\")
    idx <- idx + 1   
    
    # Cycle through comparisons
    for (cmp in colnames(mic)) {
      
      # Put a midrule
      ltxtab[[idx]] <- hlines$mid
      idx <- idx + 1 
      
      # Adjust formating for specific number of significant digits
      smicf <- smic[[cmp]]

      # Multi-row with comparison name
      ltxtab[[idx]] <- pst("\\multirow{", ndata,"}{*}{", cmp, "}")
      idx <- idx + 1
      
      # Rows with comparison data
      for (cdata in data.show) {
        ltxtab[[idx]] <- 
          switch(cdata,
                 npcs=pst(" & $\\#$PCs ", pst(" & ", as.integer(smicf["#PCs",])), "\\\\"), 
                 mnvp=pst(" & MNV ", pst(" & ", pvalf(unlist(smicf["MNV",]))), "\\\\"), 
                 parp=pst(" & $t$-test ", pst(" & ", pvalf(unlist(smicf["Par.test",]))), "\\\\"), 
                 nparp=pst(" & MW  ", pst(" & ", pvalf(unlist(smicf["NonParTest",]))), "\\\\"),
                 scoreplot=pst(" & PCS ", pst(" & ", tscat_apply(mic[,cmp], marks)), "\\\\"))
        idx <- idx + 1
      }
      
    }

    ltxtab[[idx]] <- hlines$bot
    idx <- idx + 1 
    
    ltxtab[[idx]] <- "\\end{tabular}"
    idx <- idx + 1 
    
    # TODO: Caption
    
    if (col.width) {
      ltxtab[[idx]] <- "} % resize box"
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