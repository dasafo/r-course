## K-Nearest Neighbours(knn)
#Exige que las variables predictoras sean numericas y las predictoras categoricas

install.packages("class")
library(class)
library(caret)

vac <- read.csv("../data/tema3/vacation-trip-classification.csv")

#Vemos que en ingresos hay mucha diferencia que hace que las grandes cifras dominen
#sobre las pequeñas, pudiendo falsear nuestro resultados, para ello primero 
#estaderizaremos escaland los datos de Income y Falily_size, olo que es lo mismo,
# lo normalizaremos restaremos cada valor por su media y lo dividimos por 
#la desviacion tipica
vac$Income.z <- scale(vac$Income)
vac$Family_size.z <- scale(vac$Family_size)

#Ahora ya podemos realizar el testing y validation con las nuevas columnas normalizadas
set.seed(2018)
#50% de testing, y del otro 50%, el 50% para realizar otro testing y el 50% para validation
t.ids <- createDataPartition(vac$Result, p=0.5, list = F)
train <- vac[t.ids, ]
temp <- vac[-t.ids, ]
v.ids <- createDataPartition(temp$Result, p=0.5, list = F)
val <- temp[v.ids,]
test <- temp[-v.ids,]

#Aplicamos el knn y escogemos los 5 vecinos mas cercanos(k=5) usando los de validacion
pred1 <- knn(train[,4:5], val[,4:5], train[,3], k = 5)
#MAtriz de confusion para ver el resultado
errmat1 <- table(val$Result, pred1, dnn = c("Actual", "Predichos"))
errmat1

#Aplicamos el knn y escogemos el vecino mas cercano(k=1) usando los de testing
pred2 <- knn(train[,4:5], test[,4:5], train[,3], k = 1)
errmat2 <- table(test$Result, pred2, dnn = c("Actual", "Predichos"))
errmat2

## Queremos automatizar el proceso, que introduciendo un intervalo de k(vecinos proximos)
#nos diga cual es el mejor para nuestra clasificación
knn.automate <- function(tr_predictors, val_predictors, tr_target,
                         val_target, start_k, end_k){
  for (k in start_k:end_k) {
    pred <- knn(tr_predictors, val_predictors, tr_target, k)
    tab <- table(val_target, pred, dnn = c("Actual", "Predichos") )
    cat(paste("Matriz de confusión para k = ",k,"\n"))
    cat("==============================\n")
    print(tab)
    cat("------------------------------\n")
  }
}
#k entre 1 y 8
knn.automate(train[,4:5], val[,4:5], train[,3], val[,3], 1,8)

#También podemos hacer los mismo usando trainControl del paquete caret
trcntrl <- trainControl(method="repeatedcv", number = 10, repeats = 3) #usamos la validacion cruzda
caret_knn_fit <- train(Result ~ Family_size + Income, data = train,
                       method = "knn", trControl = trcntrl,
                       preProcess = c("center", "scale"), #que le reste la media(center) y la sdt(scale)
                       tuneLength = 10)

caret_knn_fit


pred5 <- knn(train[,4:5], val[,4:5], train[,3], k=15, prob = T)
pred5
