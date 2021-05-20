# Yyl88221
Randforest prediction R codes:

library(randomForest)

library(caret)

library(lattice)

library(ggplot2)

library(MASS)

setwd("H:/")

getwd()

data<-data.table(read_excel("H:/"))

new<-data.table(read_excel("H:/"))

set.seed(12345)

train_data<-sample(nrow(data),7/10*nrow(data))

train<-data[train_data,]

test<-data[-train_data,]

fit.forest<-randomForest(GN/SN~.,train,importance=TRUE,mtry=2)

fit.forest

importance(fit.forest)

train_pred<-predict(fit.forest,newdata=train)

test_pred<-predict(fit.forest,newdata=test)

new_pred<-predict(fit.forest,newdata=new)

write.csv(train,"train.csv")

write.csv(train_pred,"train_pred.csv")

write.csv(test,"test.csv")

write.csv(test_pred,"test_pred.csv")

write.csv(new_pred,"new_pred.csv")




Structural equation model R codes:

library(lavaan)

library(haven)

USA_data <-data.table(read_excel("H:/"))

SEM <- 'Climate=~Rainfall+ET+Solar.Radiation

Soil=~SOM+TN

Soil+Climate~Temperature

Soil~Rainfall

GN~Climate+Soil+GD+NFR

Soil~NFR

GD~Climate
'
fit1<-sem(SEM,data=USA_data)

fit1

resid(fit1, type = "cor")

summary(fit1,standardized=TRUE)

modindices(fit1,sort.=TRUE)

inspect(fit1,what="std")

fitmeasures(fit1)

fitmeasures(fit1,c("nnfi", "cfi","mc", "rmsea", "srmr"))

library(semPlot)

semPaths(fit1,"std",edge.label.cex=0.8,fade=FALSE, layout = "tree",optimizeLatRes = FALSE, residuals = FALSE)




Meta-anlysis random effects model R codes:

setwd("H:/")

d1 <- read.csv("GN_sd.csv")

library(metafor)

library(ggplot2)

library(glmulti)

d2<-escalc(measure="ROM",data=d1,m2i=SN_CK,sd2i=SD_CK,n2i=N_CK,m1i=SN_T,sd1i=SD_T,n1i=N_T)

random1<-rma(yi,vi, data=d2, method="REML")

summary(random1)

funnel(random1)

regtest(random1, model="rma")

fsn(yi,vi, data=d2)
