## REd Neuronal

install.packages("nnet")
library(nnet)
library(caret)

bn <- read.csv("../data/tema3/banknote-authentication.csv")
bn$class <- factor(bn$class)

t.id <- createDataPartition(bn$class, p= 0.7, list = F)

mod <- nnet(class ~ ., data = bn[t.id,], 
            size = 3, maxit = 10000, decay = .001, rang = 0.05, #3 capas ocultas
            na.action = na.omit, skip = T)
#Para elegir el rango(rang) que es el peso minimo, se puede calcular como:
#rang * max(|variables|) ~ 1
apply(bn, 2, max) #2=por filas , donde vemos que el max(|variables|) es aprox 17.92

pred <- predict(mod, newdata = bn[-t.id,], type = "class")

table(bn[-t.id,]$class, pred,dnn = c("Actual", "Predichos") )

## Modelo RoC visto en otros scripts (vemos que nos sale perfecta)
library(ROCR)
pred2 <- predict(mod, newdata = bn[-t.id,], type = "raw")
perf <- performance(prediction(pred2, bn[-t.id,"class"]), 
                    "tpr", "fpr")
plot(perf)
