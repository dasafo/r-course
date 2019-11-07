library(randomForest)
library(caret)
bh <- read.csv("../data/tema4/BostonHousing.csv")

set.seed(2018)
t.id <- createDataPartition(bh$MEDV, p = 0.7, list = F)

mod <- randomForest(x = bh[t.id, 1:13], y = bh[t.id, 14],
                    ntree = 1000, 
                    xtest = bh[-t.id, 1:13], ytest = bh[-t.id, 14],
                    importance = T, keep.forest = T)

mod

mod$importance

#Entrenamiento
plot(bh[t.id,]$MEDV, predict(mod, newdata = bh[t.id,]),
     xlab = "Actual", ylab = "Predichos", col="red")
abline(0,1)

#Prediccion
plot(bh[-t.id,]$MEDV, predict(mod, newdata = bh[-t.id,]),
     xlab = "Actual", ylab = "Predichos", col="red")
abline(0,1)

#mtry = m/3, donde m = # de predictores
#nodesize = 5
#maxnodes 