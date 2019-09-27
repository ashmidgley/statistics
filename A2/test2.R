AIS_data = read.table('AIS.txt', header=T)

d1 = AIS_data[AIS_data$Sex=="male",]
d1

boxplot(d1$Bfat~d1$Sport, main="Percentage body fat of males in different sports",xlab="Sports", ylab="Percentage body fat of men")

bmi = data[s]