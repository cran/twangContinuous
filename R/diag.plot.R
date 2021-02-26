diag.plot <- function(x, plots, subset, ...){

  treat <- x$treat
  #propScores <- x$ps
  weights <- x$w
  # if (!all(weights[,1]==1)){
  #   weights   <- cbind(unw=rep(1,nrow(weights)),weights)
  #   propScores <- cbind(unw=rep(0.5,nrow(propScores)),propScores)
  # }

  n.tp <- length(x$desc)
  n.var <- nrow(x$desc$unw$bal.tab$results)

  whichVar <- pVal <- weighted <- varb <- NULL

  if (plots == "optimize") {

    longBal <- matrix(t(x$balance))
    optDat <- data.frame(balance = longBal, iteration = rep(x$iters, each = n.tp-1), stopRule = names(x$desc)[-1])


    if(is.null(subset))
      subset <- 1:length(levels(as.factor(optDat$stopRule)))

    pt1 <- xyplot(balance ~ iteration | stopRule, data = optDat,
                  ylab = "Balance measure", xlab = "Iteration",
                  scales = list(alternating = 1),
                  subset = as.factor(optDat$stopRule) %in% levels(as.factor(optDat$stopRule))[subset], ...)


  }

  if (plots == "es")	{ ## es plot


  desc.unw <- x$desc$unw$bal.tab$results$wcor
  desc.AAC <- x$desc[[2]]$bal.tab$results$wcor

  hld <- c(desc.AAC, desc.unw)

  weighted = as.factor(rep(rep(c("Weighted", "Unweighted"), each = n.var)))
  whichVar = factor(rep(rep(1:n.var), 2))

  esDat <- data.frame(effectSize = abs(hld),
                      weighted = weighted, whichVar = whichVar)

  yMax <- min(3, max(esDat$effectSize, na.rm=TRUE)) + .05

  pt1 <- xyplot(effectSize ~ weighted, groups=whichVar, data = esDat,
                  scales = list(alternating = 1),
                  ylim = c(-.05, yMax), type = "l", col = "lightblue",
                  #subset = subsetHold,
                  ylab = "Absolute Correlation", xlab = NULL,
                  panel = function(...){
                    panel.abline(h=c(.1), col="gray80")
                    panel.xyplot(...)
                  })
  }


  if(!(plots %in% c("optimize","es")))
    stop("plots must be one of 'optimize' or 'es' \n")

  return(pt1)

}
