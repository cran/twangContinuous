## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
library(twangContinuous)
set.seed(1234)

## ---- message=FALSE-----------------------------------------------------------
data(dat)

## -----------------------------------------------------------------------------
test.mod <- ps.cont(tss_0 ~ sfs8p_0 + sati_0 + sp_sm_0 
                            + recov_0 + subsgrps_n + treat,
                    data=dat,
                    n.trees = 500,
                    shrinkage = 0.01,
                    interaction.depth = 3,
                    verbose = FALSE)

## -----------------------------------------------------------------------------
plot(test.mod, plots="optimize")

## -----------------------------------------------------------------------------
summary(test.mod)

## -----------------------------------------------------------------------------
bal.table(test.mod, digits = 3)

## -----------------------------------------------------------------------------
plot(test.mod, plots="es")

## ---- message=FALSE-----------------------------------------------------------
library(survey)

## -----------------------------------------------------------------------------
dat$wts <- get.weights(test.mod) 
design.ps <- svydesign(ids=~1, weights=~wts, data=dat) 

## -----------------------------------------------------------------------------
outcome.model <- svyglm(sfs8p_3 ~ tss_0, design = design.ps, 
                        family = gaussian())
summary(outcome.model)

