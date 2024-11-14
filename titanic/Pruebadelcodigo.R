m <-table(train$Pclass,train$Survived)
v<-c(sum(m[1,]),sum(m[2,]),sum(m[3,]))
r<-m/v

m1 <-table(test.survived$Pclass,test.survived$Survived)
m1[1]
v01<-0:1
sample(v01,m1[1],replace = T,prob =r[1,] )
