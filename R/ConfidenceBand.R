# {{{ function to compute confidence band
PerturbIIDQuantConfBand <- function(Estimate,iid,level=0.95,N.Monte.Carlo=2000){
  #browser()
  n.vis <- length(Estimate)
  n.subj <- nrow(iid)
  vect.Delta <- rep(NA,n.vis)
  Conf.band <- matrix(NA,nrow=2,ncol=n.vis)
  s.e <- apply(iid,2,sd)/sqrt(n.subj)
  # genrate the quantile
  for (i in 1:N.Monte.Carlo){
    temp1 <- iid*rnorm(n.subj)
    temp2 <- t(t(temp1)/s.e)
    vect.Delta[i] <- max(abs(colMeans(temp2)))   
  }
  C.alpha <- quantile(vect.Delta,level)
  return(C.alpha=C.alpha)
}
# }}}
