
ci.int = function(xbar,n,sigma){
  tmp = cbind(xbar-1.96*sigma/sqrt(n), xbar+1.96*sigma/sqrt(n)) # finding the confidence interval
  return(tmp)
}

q1 = function(){
  parameter = 1/3
  mu = (1-parameter)/parameter
  nvec = c(10,100,200,300,400,500,600,700,800,900,1000)
  nlength = length(nvec)
  m = 10000
  covvec = rep(NA, nlength)
  
  for (j in 1:nlength) {
    mat = matrix(rgeom(m*nvec[j],parameter), m,nvec[j])
    xbar = rowMeans(mat)
    sig = apply(mat, 1, sd)
    confval = ci.int(xbar, nvec[j], sig)
    inOrOut = (confval[,1] < mu) & (confval[,2] > mu)
    covvec[j] = mean(inOrOut)
  }

  plot(nvec, covvec, pch=20, xlab="n", ylab="coverage value", main="geometric distribution of n")
}

q1()
