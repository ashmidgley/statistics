
data("faithful")
erupt <- faithful$eruptions

kurtosis = function(data){
  x = mean(data)
  n = length(data)
  
  p1 = ((n+1)*n*(n-1))/((n-2)*(n-3)) 
  
  s = sum(data-x)
  
  p2 = (s^4)/((s^2)^2)-3
  p3 = ((n-1)^2)/((n-2)*(n-3))
  t.x = (p1*p2*p3)
  return(t.x)
}








q1 = function(func, data, Q, alpha){
  
  t.q <- rep(NA, Q)
  
  for(q in 1:Q){
    ysamp <- sample(data, replace = T)
    t.q[q] <- quantile(ysamp, 0.75, names=F)
  }
  
  se <- sd(t, q)
  
  ci.norm <- t.star + qnorm(quantiles.use)*se
  ci.t <- t.star + qt(quantiles.use,n-1)*se
  ci.perc <- quantile(t.q,quantiles.use,names=F)
  
  stat_t = func(data)
  est_bias = mean(t.q) - stat_t
  result <- list(t.q, stat_t, est_bias, ci.norm, ci.t, ci.perc)
  
  return(result);
}

bootstrp = q1(kurtosis(erupt), erupt, 999, 0.05)

t.q = bootstrp[[1]]

est_bias = bootstrp[[3]]

t.q
est_bias

