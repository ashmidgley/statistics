my_data = read.table('vole380.txt', header=T, sep="&", na.strings = c("?"));

xvals = my_data[1:31]
zvals = my_data[32:60]
zvalsNum = apply(zvals, 1, as.numeric)

xones = c(1:173)
zmeans = c(1:173)

xrows = nrow(xvals) 
xcols = ncol(xvals)

for(i in 1:xrows){
  onecount = 0
  for(k in 1:xcols){
    if(xvals[i, k] == 1){
      onecount = onecount+1
    }
  }
  xones[i] = onecount
}

zrows = nrow(zvals)
zcols = ncol(zvals)

for(i in 1:zrows){
  total=0
  numVals=0
  for(k in 1:zcols){
    if(!is.na(zvals[i,k])){
      total = total + zvals[i, k]
      numVals = numVals+1
    }
  }
  zmeans[i] = total
}

xones
zmeans

plot(xones ,zmeans, pch=20, main="Average weight VS. no of captures", ylab="Total number of captures",xlab="Average weight at capture",col="blue")

hist(zvalsNum ,main="Weights at capture",xlab="Weight (grams)")

m1 = lm(xones~zmeans)
m1

resid1 = rstudent(m1)

confint(m1, level=0.95)

plot(zmeans,resid1, pch=20, main="Residual plot (vole)", xlab="Average weight at capture (grams)", ylab="Studentised residual")
abline(h=0)



