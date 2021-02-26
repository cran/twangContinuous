#' Gradient boosted propensity score estimation for continuous exposures
#'
#' `ps.cont` calculates propensity scores using gradient boosted
#' regression and provides diagnostics of the resulting propensity scores.
#'
#' @param formula An object of class [formula]: a symbolic
#'   description of the propensity score model to be fit with the treatment
#'   variable on the left side of the formula and the potential confounding
#'   variables on the right side.
#' @param data A dataset that includes the treatment as well as the
#'   potential confounding variables.
#' @param n.trees Number of gbm iterations passed on to [gbm]. Default: 10000.
#' @param interaction.depth A positive integer denoting the tree depth used in
#'   gradient boosting. Default: 3.
#' @param shrinkage A numeric value between 0 and 1 denoting the learning rate.
#'   See [gbm] for more details. Default: 0.01.
#' @param bag.fraction A numeric value between 0 and 1 denoting the fraction of
#'   the observations randomly selected in each iteration of the gradient
#'   boosting algorithm to propose the next tree. See [gbm] for
#'   more details. Default: 1.0.
#' @param print.level The amount of detail to print to the screen. Default: 2.
#' @param verbose If `TRUE`, lots of information will be printed to monitor the
#'   the progress of the fitting. Default: `FALSE`.
#' @param stop.method A method or methods of measuring and summarizing balance
#'   across pretreatment variables. Current options are `wcor`, the weighted Pearson
#'   correlation, summarized by using the mean across the pretreatment variables.
#'   Default: `wcor`.
#' @param sampw Optional sampling weights.
#' @param treat.as.cont Used as a check on whether the exposure has greater than
#'    five levels. If it does not and treat.as.cont=FALSE, an error will be
#'    produced. Default: FALSE
#' @param ... Additional arguments that are passed to ps function.
#'
#' @return Returns an object of class `ps.cont`, a list containing
#'
#'   * `gbm.obj` The returned [gbm] object.
#'
#'   * `treat` The treatment variable.
#'
#'   * `desc` A list containing balance tables for each method selected in
#'     `stop.methods`. Includes a component for the unweighted
#'     analysis names \dQuote{unw}. Each `desc` component includes
#'     a list with the following components
#'
#'     - `ess` The effective sample size.
#'
#'     - `n` The number of subjects.
#'
#'     - `max.wcor` The largest weighted correlation across the covariates.
#'
#'     - `mean.wcor` The average weighted correlation across the covariates.
#'
#'     - `rms.wcor` The root mean square of the absolute weighted correlations across the
#'        covariates.
#'
#'     - `bal.tab` a (potentially large) table summarizing the quality of the
#'       weights for balancing the distribution of the pretreatment covariates.
#'       This table is best extracted using the [bal.table] method.
#'       See the help for [bal.table] for details.
#'
#'     - `n.trees` The estimated optimal number of [gbm]
#'       iterations to optimize the loss function.
#'
#'  * `ps.den` Denominator values for the propensity score weights.
#'
#'  * `ps.num` Numerator values for the propensity score weights.
#'
#'  * `w` The propensity score weights. If sampling weights are given then these
#'        are incorporated into these weights.
#'
#'  * `datestamp` Records the date of the analysis.
#'
#'  * `parameters` Saves the `ps.cont` call.
#'
#'  * `alerts` Text containing any warnings accumulated during the estimation.
#'
#'  * `iters` A sequence of iterations used in the GBM fits used by `plot` function.
#'
#'  * `balance` The balance measures for the pretreatment covariates used in plotting.
#'
#'  * `sampw` The sampling weights as specified in the `sampw` argument.
#'
#'  * `preds` Predicted values based on the propensity score model.
#'
#'  * `covariates` Data frame containing the covariates used in the propensity score model.
#'
#'  * `n.trees` Maximum number of trees considered in GBM fit.
#'
#'  * `data` Data as specified in the `data` argument.
#'
#' @examples
#'   \dontrun{test.mod <- ps.cont(tss_0 ~ sfs8p_0 + sati_0 + sp_sm_0
#'           + recov_0 + subsgrps_n + treat, data=dat}
#'
#' @seealso [gbm], [plot.ps.cont], [bal.table], [summary.ps.cont]
#'
#' @references Zhu, Y., Coffman, D. L., & Ghosh, D. (2015). A boosting algorithm for
#' estimating generalized propensity scores with continuous treatments.
#' *Journal of Causal Inference*, 3(1), 25-40. \doi{doi:10.1515/jci-2014-0022}
#'
#' @export

# suppressBindingNotes <- function(variablesMentionedInNotes) {
#   for(variable in variablesMentionedInNotes) {
#     assign(variable, NULL, envir = .GlobalEnv)
#   }
# }
#
# suppressBindingNotes(c("sampw","alert","sampW"))

ps.cont <- function(formula,
                    data,                          # data
                    n.trees=10000,                 # gbm options
                    interaction.depth=3,
                    shrinkage=0.01,
                    bag.fraction = 1.0,
                    sampw=NULL,
                    print.level=2,
                    verbose=FALSE,
                    stop.method = "wcor",
                    treat.as.cont = FALSE, ...){

  type <- alert <- NULL

  if (missing(formula)) stop("A formula must be supplied.", call. = FALSE)
  formula <- formula(formula)

  terms <- match.call()

  # Checking if the formula has a response
  if(!attr(terms(formula, data=data), 'response'))
    stop('Please supply a treatment variable on the left side of the formula');

  # Dropping the intercept term
  if(attr(terms(formula, data=data), 'intercept')){
    formula <- update(terms(formula, data=data), . ~ . -1)
  }

  # Collecting the data and making a model.frame object to create the design matrix
  mf <- model.frame(formula, data = data)

  treat.var <- model.response(mf, 'numeric')

  # Stopping if the variable is not continuous
  if(length(unique(treat.var)) < 5 & treat.as.cont == FALSE)
    stop('Please supply a continuous treatment variable');

  if (any(is.na(treat.var))) stop("Missingness is not allowed in the treatment variable.", call. = FALSE)

  if (is.null(sampw)) sampW <- rep(1, length(treat.var)) else sampW <- sampw

  designX <- model.matrix(formula, data=mf)

  new.data <- data.frame(mf = mf, sampW =sampW)
  names(new.data) <- c(names(mf), "sampW")

  #######
  # all this is just to extract the variable names
  # mf <- match.call(expand.dots = FALSE)
  # m <- match(c("formula", "data"), names(mf), 0)
  # mf <- mf[c(1, m)]
  # mf[[1]] <- as.name("model.frame")
  # mf$na.action <- na.pass
  # mf$subset <- rep(FALSE, nrow(data)) # drop all the data
  # mf <- eval(mf, parent.frame())
  # Terms <- attr(mf, "terms")
  # var.names <- attributes(Terms)$term.labels
  #
  # if(length(var.names) < 2) stop("At least two variables are needed in the
  # right-hand side of the formula.\n")
  ###########

  # create the desc object. This holds information on variable balance
  # stop.method.names <- sapply(stop.method,function(x){x$name})
  stop.method.names <- "AAC"
  desc <- vector("list", 1 + length(stop.method))
  names(desc) <- c("unw", stop.method.names)

  # allocate space for the propensity scores and weights
  # p.s        <- data.frame(matrix(NA_real_, nrow=nrow(data),
  #                                 ncol=length(stop.method)))
  # names(p.s) <- stop.method.names
  w          <- data.frame(matrix(NA_real_, nrow=nrow(new.data),
                                  ncol=length(stop.method)))
  names(w)   <- stop.method.names

  # alert.stack collects all the warnings
  alerts.stack <- textConnection("alert","w")
  # fit the propensity score model
  if(verbose) cat("Fitting gbm model\n")

  gbm_mod <- gbm::gbm(formula,
                      data = new.data,
                      weights=sampW,
                      shrinkage = shrinkage,
                      interaction.depth = interaction.depth,
                      distribution = 'gaussian',
                      n.trees = n.trees,
                      bag.fraction = bag.fraction,
                      n.minobsinnode = 10,
                      train.fraction = 1,
                      verbose = verbose,
                      keep.data = FALSE)

  if(verbose) cat("Diagnosis of unweighted analysis\n")

  desc$unw <- desc.wts.cont(treat.var=treat.var,
                            covs=designX,
                            w=sampW)


  if (verbose) cat("Estimating marginal density of the treatment ")

  num.mod <- lm(treat.var ~ 1, data=new.data, weights=sampW)
  ps.num <- dnorm(num.mod$residuals, 0, sd=summary(num.mod)$sigma)

  if(verbose) cat("Optimizing stopping rule\n")

  # get optimal number of iterations
  # Step #1: evaluate at 25 equally spaced points
  #iters <- round(seq(1, gbm_mod$n.trees, length=25))
  #balance <- matrix(NA, ncol = length(stop.method), nrow = 25)

  nintervals <- round(1+sqrt(2*n.trees))
  iters <- round(seq(1, n.trees, length = nintervals))
  bal <- rep(0, length(iters))
  balance <- matrix(NA, ncol = length(stop.method), nrow = nintervals,
                    dimnames = list(iters, stop.method))

  for (j in 1:length(iters)) {

    bal[j] <- aac(iters[j], data = new.data, treat.var = treat.var, covs = designX,
                              ps.model = gbm_mod,
                              ps.num = ps.num,
                              sampw = sampW)

    balance[,1] <- bal #right now there is only one stop method
  }

    # Step #2: find the interval containing the approximate minimum
    interval <- which.min(bal) + c(-1,1)
    interval[1] <- max(1, interval[1])
    interval[2] <- min(length(iters), interval[2])

  # Step #3: refine the minimum by searching with the identified interval

  opt <- optimize(aac, interval = iters[interval], data = new.data,
                  treat.var = treat.var, covs = designX,
                  ps.model = gbm_mod,
                  ps.num = ps.num,
                  sampw = sampW, tol = .Machine$double.eps)

  if(verbose) cat("Optimized at",round(opt$minimum),"\n")
  if(gbm_mod$n.trees-opt$minimum < 100)
    warning("Optimal number of iterations is close to the specified n.trees.
            n.trees is likely set too small and better balance might be obtainable by
            setting n.trees to be larger.")

  # compute propensity score weights

  preds <- predict(gbm_mod, newdata=data,
                         n.trees=round(opt$minimum),
                         type="response")

  ps.den <- dnorm(treat.var, mean=preds, sd=sd(treat.var - preds))
  w <- ps.num/ps.den
  w <- w * sampW


######################
  # n_tree_test <- seq(50, n.trees, 50)
  # corr_bal <- matrix(NA, ncol = ncol(designX)+1, nrow = length(n_tree_test))
  # ess_vals <- matrix(NA, ncol = 2, nrow = length(n_tree_test))
  # for(nt in 1:length(n_tree_test)){
  #   ntree <- n_tree_test[nt]
  #   gbm_m <- predict(gbm_mod, n.trees = ntree)
  #   w <- make_cont_wts(treat.var, gbm_m)
  #   ess <- sum(w)^2 / sum(w^2)
  #   corr_bal[nt,] <- c(ntree, apply(designX, 2, function(x) wcor(w, x, treat.var)))
  #   ess_vals[nt,] <- c(ntree, ess)
  # }
  #
  # bal <- apply(abs(corr_bal[,2:ncol(corr_bal)]), 1, mean)
  # best_bal <- min(which(bal == min(bal)))
  # best_w <- make_cont_wts(treat.var, predict(gbm_mod, n.trees = corr_bal[best_bal, 1]))
########################

   if(verbose) cat("Diagnosis of weights\n")

   desc$AAC <- desc.wts.cont(treat.var=treat.var, covs=designX, w=w,
                             which.tree=round(opt$minimum))

  #move to separate file?
  # if(plot_balance){
  #   bal_loess <- loess(bal ~ corr_bal[,1], span = 0.1)
  #   par(mfrow = c(1,2))
  #   plot(0, xlim = c(0, n.trees), ylim = c(0,max(bal)),
  #        pch = 19, col = rgb(0,0,0,0),
  #        xlab = 'Number of Trees', ylab = 'Mean Absolute Weighted Correlation',
  #        main = 'Balance')
  #   points(corr_bal[,1], bal, pch = 19, col = rgb(0,0,0,0.25))
  #   lines(bal_loess$x, bal_loess$fitted, lwd = 3, col = rgb(0.5, 0,0,0.5))
  #   points(corr_bal[best_bal, 1], bal[best_bal], pch = 19, col = rgb(0,0.75,0,1))
  #   plot(0, xlim = c(0, n.trees), ylim = c(0,max(ess_vals[,2])),
  #        pch = 19, col = rgb(0,0,0,0),
  #        xlab = 'Number of Trees', ylab = 'Effective Sample Size',
  #        main = 'Effective Sample Size')
  #   points(ess_vals[,1], ess_vals[,2], pch = 19, col = rgb(0,0,0,0.25))
  # }

  close(alerts.stack)
  if(verbose) cat(alert,sep="\n")


  result <- list(gbm.obj    = gbm_mod,
                 treat      = treat.var,
                 desc       = desc,
                 ps.den     = ps.den,
                 ps.num     = ps.num,
                 w          = w,
                 sampw      = sampW,
                 datestamp  = date(),
                 parameters = terms,
                 alerts     = alert,
                 iters = iters,
                 balance = balance,
                 n.trees = n.trees,
                 covariates = designX,
                 #min_bal = bal[best_bal],
                 preds = preds,
                 data = data)

  class(result) <- c("ps.cont", "ps")
  return(result)
}
