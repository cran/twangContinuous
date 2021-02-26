desc.wts.cont <- function(treat.var, covs, w, which.tree=NA) {

  desc <- setNames(vector("list", 7),
                   c("ess", "n", "max.wcor", "mean.wcor", "rms.wcor", "bal.tab", "n.trees"))
  desc[["bal.tab"]][["results"]] <- data.frame(wcor = apply(covs, 2, function(c) wcor(y=treat.var, x=c, wts = w)),
                                               row.names = colnames(covs))

  desc[["ess"]] <- sum(w)^2 / sum(w^2)
  desc[["n"]] <- length(treat.var)
  desc[["max.wcor"]] <- max(abs(desc[["bal.tab"]][["results"]][["wcor"]]))
  desc[["mean.wcor"]] <- mean(abs(desc[["bal.tab"]][["results"]][["wcor"]]))
  desc[["rms.wcor"]] <- sqrt(mean(desc[["bal.tab"]][["results"]][["wcor"]]^2))
  desc[["n.trees"]] <- which.tree

  return(desc)
}

