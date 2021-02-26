### calculate weighted balance statistic
bal.stat <- function(data, vars=NULL, treat.var, w.all, sampw,
                     get.wcor=TRUE,
                     get.tstat=FALSE,
                     #na.action="level",
                     fillNAs = FALSE)
{
  if(is.null(vars)) vars<-names(data)[names(data)!=treat.var]

  is.fac   <- sapply(data[,vars,drop=FALSE],is.factor)
  fac      <- vars[is.fac]
  not.fac  <- vars[!is.fac]

  ret <- vector("list",length(vars))
  names(ret) <- vars

  sampW <- sampw

  ##### Calculate stats
  ret <- lapply(data[,vars,drop=FALSE], ps.summary.cont,
                         t=data[,treat.var], w=w.all, sampw = sampW,
                         get.wcor=get.wcor, get.tstat=get.tstat,
                         #na.action=na.action,
                         collapse.by.var=FALSE, fillNAs = fillNAs)

  # this keeps the variables in the same order as vars
  n.rows <- sapply(ret,nrow)
  var.levels <- unlist(sapply(ret, rownames))
  var.names <- rep(names(ret),n.rows)
  var.names[var.levels!=""] <- paste(var.names[var.levels!=""],
                                     var.levels[var.levels!=""],sep=":")

  res <- data.frame(matrix(0,nrow=length(var.names), ncol=ncol(ret[[1]])))
  names(res) <- colnames(ret[[1]])
  rownames(res) <- var.names

  # populate the results table
  i.insert <- 1
  for(i in 1:length(ret))
  {
    res[i.insert:(i.insert+nrow(ret[[i]])-1),] <- ret[[i]]
    i.insert <- i.insert+nrow(ret[[i]])
  }

  res <- list(results=data.frame(res))
  return(res)
}

