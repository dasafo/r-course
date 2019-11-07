## Metodo de Super Vector Machine (SVM)

install.packages("e1071")
library(caret)
library(e1071)

banknote <- read.csv("../data/tema3/banknote-authentication.csv")
banknote$class = factor(banknote$class)

## Creamos modelo de entrenamiento (70%) y validacion (30%)

set.seed(2018)

#Conjunto de entrenamiento(70%)
t.ids <- createDataPartition(banknote$class, p = 0.7, list = F)
#Aplicamos el Super Vector Machine(svm)
mod <- svm(class ~ ., data = banknote[t.ids, ], #class en funcion de todas las variables(~.)
           class.weights = c("0"=0.3, "1"=0.7), #podemos controlar los pesos de las variables
           cost=1000) #ajuste de la calisficacion(cuanto mas alto, mejor ajustado)
# Vemos la matriz de confusion del conjkunto de entrenamiento(vemos que acierta al 100%)
table(banknote[t.ids,"class"], fitted(mod), dnn = c("Actual", "Predicho"))
#REpresentamos graficamente respecto a skew frente a variance
plot(mod, data = banknote[t.ids,], skew ~ variance)

#HAcemos lo mismo pero para el conjunto de prediccion(30%)
pred <- predict(mod, banknote[-t.ids,])
#Matriz de confusion de los datos de prediccion(vemos que también acierta al 100%)
table(banknote[-t.ids, "class"], pred, dnn = c("Actual", "Predicho"))
#REpresentamos graficamente respecto a skew frente a variance
plot(mod, data = banknote[-t.ids,], skew ~ variance)

#Vamos a ver como se puede ajustar un svm con gamma y cost
#Nos dira cuales son los mejores valores para realizar un buen ajuste
#Para ello le damos un intervalo de gamma y otro para cost
tuned <- tune.svm(class ~ ., data = banknote[t.ids,], 
                  gamma = 10^(-6:-1), cost = 10^(1:2))
summary(tuned)
