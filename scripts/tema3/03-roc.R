install.packages("ROCR")
library(ROCR)

data1 <- read.csv("../data/tema3/roc-example-1.csv")
data2 <- read.csv("../data/tema3/roc-example-2.csv")
#la columna de la tabla nos dice por ejemplo la probabilidad(prob)
#de que un cliente compre algo (class= 1 o buyer)
#0 -> fallo
#1 -> éxito

#creamos el objeto prediccion con prediction(de ROCR)
pred1 <- prediction(data1$prob, data1$class)

#vememos que eficiencia tiene esta predicción
perf1 <- performance(pred1, "tpr", "fpr") #true positive rate(tpr) y false positive rate(fpr)
plot(perf1)
lines(par()$usr[1:2], par()$usr[3:4])#añadimos la diagonal

prob.cuts.1 <- data.frame(cut = perf1@alpha.values[[1]],
                          fpr = perf1@x.values[[1]],
                          tpr = perf1@y.values[[1]])
head(prob.cuts.1)

tail(prob.cuts.1)

prob.cuts.1[prob.cuts.1$tpr>=0.8,]


#Para el segundao data
pred2 <- prediction(data2$prob, data2$class, label.ordering = c("non-buyer", "buyer"))
perf2 <- performance(pred2, "tpr", "fpr")
plot(perf2)
lines(par()$usr[1:2], par()$usr[3:4])
