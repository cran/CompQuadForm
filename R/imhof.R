`imhof` <-
function(q, lambda, h = rep(1, length(lambda)), delta = rep(0, length(lambda)), epsabs = 10^(-6), epsrel = 10^(-6), limit = 10000) {

    r <- length(lambda)
    if (any(delta < 0)) stop("All non centrality parameters in 'delta' should be positive!") 
    if (length(h) != r) stop("'lambda' and 'h' should have the same length!")
    if (length(delta) != r) stop("'lambda' and 'delta' should have the same length!")
    if (limit >= 99999999) stop("'limit' is too large!")
  
    Qq <- 0

    out <- .C("probQsupx", as.double(q), as.double(lambda), r = as.integer(r), as.double(h), as.double(delta), Qq = as.double(Qq), abserr = as.double(epsabs), as.double(epsrel), as.integer(limit), PACKAGE = "CompQuadForm")

  if ((out$Qq < 0) && (out$Qq + out$abserr > 0)) warning("Note that Qq + abserr is positive.") 

    return(list(Qq = out$Qq, abserr = out$abserr))

}

