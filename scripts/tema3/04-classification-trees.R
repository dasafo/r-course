install.packages(c("rpart", "rpart.plot", "caret"))
library(caret)
library(rpart)
library(rpart.plot)

banknote <- read.csv("../data/tema3/banknote-authentication.csv")

set.seed(2018)

training.ids <- createDataPartition(banknote$class, p = 0.7, list = F)

## _______1- Creamos el modelo del arbol con rpart_____________

#class ~ . <-> class ~ variance + skew + curtosis + entropy
mod <- rpart(class ~ . , 
             data = banknote[training.ids,],
             method = "class", #class de metodo de clasificacion
             control = rpart.control(minsplit = 20, cp = 0.01))
#class ~ . <-> class ~ variance + skew + curtosis + entropy
#método 'class' de clasificacion(no tiene nada que ver con la columna class)
#el arbol solo tiene que considerar nodos con al menos 20 casos en su interior
# cp es el parametro de complejidad

mod

## PAra verlo mejor, creamos un diagrama de arbol con prp
prp(mod, type = 2, extra = 104, nn = TRUE, 
    fallen.leaves = T, faclen = 4, varlen = 8,
    shadow.col = "lightblue")
## type=2 para que cada uno de los nodos quede etiquetado, 
# y que la divixion ocurra debajo del mismo
## extra = 104 muestra la rpobabilidad de cada clase en el nodo respecto al 
# nodo anteior(el de arriba) y los casos en los que caemos en forma de %
## nn=True es el numero del nodo
## fallen.leaves = TRUE nos muestra los nodos hoja(los finales)
# con True los muestra abajo del todo
## faclen = 4 se utiliza para abreviar los nombres de las calses con 
# una longitud especifica(no hemos asignado nombres en nuestro caso así que da igual)
## varlen = 8 lo mismo que faclen pero para las variables. En nuestro caso si
# que tenemos variables(nombres de las columnas)
## shadow.col = "gray" es la sombra de los nodos


## ________2- Ahora queremos podar el árbol_________________

mod$cptable

mod.pruned <- prune(mod, mod$cptable[8, "CP"])
#pillamos la 8a componente principal(CP) ya que cogiendo el xerror menor(0.1194379)
#sumado a la desviacion tipica(xstd) supera a xerror en la 8a CP
#0.1194379+0.01627482>0.1194379 (de la 8)

prp(mod.pruned, type = 2, extra = 104, nn = TRUE,
    fallen.leaves = TRUE, faclen = 4, varlen = 8,
    shadow.col = "gray")


pred.pruned <- predict(mod.pruned, banknote[-training.ids,], type="class")

table(banknote[-training.ids,]$class, pred.pruned, 
      dnn = c("Actual", "Predicho"))

pred.pruned2 <- predict(mod.pruned, banknote[-training.ids,], type = "prob")

head(pred.pruned)
head(pred.pruned2)

library(ROCR)

pred <- prediction(pred.pruned2[,2], banknote[-training.ids, "class"])
perf <- performance(pred, "tpr", "fpr")
plot(perf)
