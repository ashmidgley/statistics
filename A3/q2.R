
q2 = function(){
  #female
  meanf = 200
  sigmaf = 10
  nf = 20
  
  #male
  sigmam = 15
  nm = 20
  meanm = seq(175, 225)
  meanmlength = length(meanm)
  #print(meanm)
  #print(meanmlength)
  
  m = 1000
  
  #t test matrix
  tmat = matrix(NA, meanmlength, m)

  #wilcox test matrix
  wilcmat = matrix(NA, meanmlength, m)
  
  wilcPowCCount=0
  tPowCount = 0
  
  for(i in 1:meanmlength){
    for(j in 1:m){
      fvalue = rnorm(nf, meanf, sigmaf);
      mvalue = rnorm(nm, meanm[i], sigmam);
      ttest = t.test(fvalue, mvalue)
      wilctest = wilcox.test(fvalue, mvalue)
      
      ttpVal = ttest$p.value
      wilctpVal = wilctest$p.value
      
      tmat[i,j] = ttpVal
      wilcmat[i,j] = wilctpVal
      
      if(ttpVal < 0.05){
        tPowCount = tPowCount+1
      }
      if(wilctpVal < 0.05){
          wilcPowCCount = wilcPowCCount + 1
      }
    }
  }
  tPow = tPowCount/m
  wPow = wilcPowCCount/m
  
  print(tPow)
  print(wPow)
}

q2()