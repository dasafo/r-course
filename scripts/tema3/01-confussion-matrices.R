cp <- read.csv("../data/tema3/college-perf.csv")

#Organizamos en niveles los factores de las notas reales(Perf) y las predecidas(Pred)
#O lo que es lo mismo, le indicamos que Low<Medium<High
cp$Perf <- ordered(cp$Perf, 
                   levels = c("Low", "Medium", "High"))
cp$Pred <- ordered(cp$Pred,
                   levels = c("Low", "Medium", "High"))


#Creamos una tabla para ver la matriz de confusión
table <- table(cp$Perf, cp$Pred, 
               dnn =  c("Actual", "Predecido"))
table
#En dicha tabla de confución vemos la cantidad de veces que ha acertado las obtenidas
#respecto a las predichas por nuestro modelo.

#Nos da lo mismo que en table, pero divide los valores por los totales
prop.table(table)

round(prop.table(table, 1)*100, 2) #el '1' para decirle que queremos las filas sumen el 100%
#vemos que la suma de los elementos de cada fila da 100(%). Vemos los aciertos.


round(prop.table(table, 2)*100, 2) #el '2' para decirle que queremos las columnas sumen el 100%
#lo mismo que antes pero en columnas. Vemos como serían nuestras predicciones.

barplot(table, legend = TRUE, 
        xlab = "Nota predecida por el modelo")

mosaicplot(table, main = "Eficiencia del modelo")

summary(table)
