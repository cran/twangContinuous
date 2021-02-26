aac <- function(i, data, treat.var, covs, ps.model, ps.num, sampw) {

  # i: number of iterations (trees)
  # data: dataset containing the treatment and the covariates
  # ps.model: the boosting model to estimate p(T_i|X_i)
  # ps.num: the estimated p(T_i)

  GBM.fitted <- predict(ps.model, newdata = data, n.trees = floor(i), type = "response")
  ps.den <- dnorm(treat.var, mean = GBM.fitted, sd = sd(treat.var - GBM.fitted))
  w <- ps.num/ps.den

  corr_ <- apply(covs, 2, function(c) wcor(x=c, y = treat.var, wts = w * sampw))

  return(mean(abs(corr_)))
}
