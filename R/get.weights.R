get.weights <- function(ps1, stop.method = "wcor", withSampW = TRUE)
{
    w <- with(ps1, ps.num/ps.den)
    if(withSampW) w <- w * ps1$sampw
    return(w)

  if(!(class(ps1) %in% c('ps.cont'))) stop("The object 'ps1' must be of class 'ps.cont'.")
}
