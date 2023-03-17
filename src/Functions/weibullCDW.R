weibullCDW <- function(p, quantile = 0.05) {
    xminuser <- 0
    xmaxuser <- 365
    # extract parameters if p is a whole cardiFit object
    if (inherits(p, "cardiFit")) { p <- p$p }
   
    # For 6 point distribution
    pp <- p
    aweibull <- aweibull7
    fweibull <- fweibull7
    
    # Extract xmin and max
    xmin <- p[2]  # p[2] = left turnpoint, attention: overwrites user input
    xmax <- p[5]  # p[5] = right turnpoint, attention: overwrites user input
  
    if(xmin > xmax) {
      ############# CHANGE HERE -> if xmin is picking up a second peak, set to zero ###############
      xmin = 0
      xmax = p[5]
    }
    
    fzero <- function(x, q, p, x0=0) {
      q - aweibull(lower=x0, upper=x, p)
    }                                                                
    opt <- optimize(f = fweibull, p = c(p, 0), lower = xmin, upper = xmax, maximum=TRUE)
    tMid <- opt$maximum
    ymax <- fweibull(tMid, p)
    
    # Not symmetric 
    q      <- c(quantile, 1 - quantile)
    left   <- aweibull(lower = 0,    upper = tMid, p = c(pp, (p[4]+1) * (1-p[1])))
    right  <- aweibull(lower = tMid, upper = xmaxuser,  p = c(pp,  p[4]))
    tBegin <- uniroot(fzero, interval=c(0, tMid),   q = q[1] * left,  p = c(pp, (p[4]+1) * (1-p[1])))$root
    tEnd   <- uniroot(fzero, interval=c(tMid, xmaxuser), q = q[2] * right, p = c(pp,  p[4]), x0 = tMid)$root
    
    list(x = c(tMid=tMid, tBegin=tBegin, tEnd=tEnd),
         y  = fweibull(c(tMid, tBegin, tEnd), p),
         p  = p
    )
}
