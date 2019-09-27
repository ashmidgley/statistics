
library(plyr)

findWinner = function(i, j){
  p = plogis(-(i-j)/8)
  win = sample(c("i","j"),1, replace = TRUE, prob=c(p, 1-p))
  #print(win)
  if(win == "i"){
    return(i)
  }else{
    return(j)
  }
}

q3 = function(){
  i = 1

  m=10000

  rounds = c(8,4,2,1)
  
  winvec = rep(NA, m)
  
  for(x in 1:m) {
    seedVec = c(1, 16, 8, 9, 4, 13, 5, 12, 11, 6, 14, 3, 10, 7, 15, 2)
    for (i in 1:length(rounds)) {
      nextRound = rep(NA, rounds[i])
      for (j in 1:length(nextRound)) {
        winner = findWinner(seedVec[j * 2 - 1], seedVec[j * 2])
        nextRound[j] = winner
      }
      seedVec = rep(nextRound)
    }
    winvec[x] = seedVec[1]
  }
  
  winTable = table(winvec)
  
  t4WinP = sum(winTable[4]/m)
  print(t4WinP)
  
  t10HigherWinP = sum(winTable[10:16]/m)
  print(t10HigherWinP)
  
  hist(winvec,breaks=0.5:16.5,freq=FALSE,xlab="Team's seed value",ylab="Probability of winning tournament", main = "Probability of team's winning tournament")
  
}

q3()
