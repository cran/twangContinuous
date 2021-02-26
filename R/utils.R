# weighted functions --------------
wmean <- function(wts, x){sum(x * wts) / sum(wts)}
wvar <- function(wts, x){
  wm <- wmean(wts, x)
  wv <- sum(wts * (x - wm)^2) / sum(wts)
  wv
}
wcor <- function(wts, x, y){
  wmx <- wmean(wts, x)
  wmy <- wmean(wts, y)
  wvx <- wvar(wts, x)
  wvy <- wvar(wts, y)
  topval <- sum(wts * (x - wmx) * (y - wmy)) / sum(wts)
  topval / sqrt(wvy * wvx)
}

make_cont_wts <- function(ta, preds){
  wts <- dnorm(ta, mean = mean(ta), sd = sd(ta)) /
    dnorm(ta, mean = preds, sd = sd(ta - preds))
  wts
}

inv_logit <- function(x) {exp(x) / (1 + exp(x))}
