
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
    
  result <- list(t.q, stat_t, ci.norm, ci.t, ci.perc, est_bias)
  
  return(result);
}


