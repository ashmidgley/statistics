
aWinsBoth = function() {
  bestCase = 0
  n = 100000
  
  for (i in 1:n) {
    if (sample(c("A","B"), 1) == "A" &
        sample(c("B","C"), 1) == "B" & sample(c("A","C"), 1) == "A") {
      bestCase = bestCase + 1
    }
  }
  r = bestCase / n
  return(r)
}

differentProb = function(){
  bestCase = 0
  n = 100000
  
  for (i in 1:n) {
    if (sample(c("A","B"), 1, replace = FALSE, prob=c(0.6,0.4)) == "A" &
               sample(c("B","C"), 1, replace = FALSE, prob=c(0.25,0.75)) == "B" & sample(c("A","C"), 1) == "A") {
      bestCase = bestCase + 1
    }
  }
  r = bestCase / n
  
  return(r)
}

vegGarden = function() {
  potato = 0
  n = 100000
  for (i in 1:n) {
    if(sample(c("pea, no-pea",replace = FALSE,prob = c(0.7,0.3)) == "pea")){
      if(sample(c("potato, no-potato") == "potato")){
        potato = potato+1
      }
    }else{
      if(sample(c("potato, no-potato"),replace = FALSE,prob = c(0.9,0.1)) == "potato"){
        potato = potato+1
      } 
    }
    
    r = potato/n
    return(potato)
  }
}