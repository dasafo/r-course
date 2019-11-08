protein <- read.csv("../data/tema5/protein.csv")

#Vamos a normalizar y escalar todas menos los paises para que 
#esten todas en el mismo rango
data <- as.data.frame(scale(protein[,-1])) #nos quedamos con todas menos los paises
data$Country = protein$Country #Añadimos ahora los paises a data

## ________________Clusterings jeraquicos Aglomerativos__________

#Aplicamos un clustering jerarquico usando ward.D2
# que es un clustering de la minima varianza, clustering aglomerativo
hc <- hclust(dist(data, method = "euclidean"), #distancia euclidia
             method = "ward.D2")
hc
rownames(data) = data$Country #para que en la representacion salgan los nombres
plot(hc, hang = -0.01, cex = 0.7)


fit <- cutree(hc, k=4)
table(fit)
rect.hclust(hc, k=4, border="red")


#Aplicamos un clustering jerarquico usando single
hc2 <- hclust(dist(data, method = "euclidean"),
              method = "single")
plot(hc2, hang=-0.01, cex = 0.7)

#Aplicamos un clustering jerarquico usando complete
hc3 <- hclust(dist(data, method = "euclidean"),
              method = "complete")
plot(hc3, hang=-0.01, cex = 0.7)
hc3$merge #Para ver las uniones de los clusters

#Aplicamos un clustering jerarquico usando average
hc4 <- hclust(dist(data, method = "euclidean"),
              method = "average")
plot(hc4, hang=-0.01, cex = 0.7)
hc4$merge

d <- dist(data, method = "euclidean")
d 


alb<-data["Albania",-10]
aus<-data["Austria",-10]
#Esto es lo que hace la distancia euclidea(ej: albania y austria)
sqrt(sum((alb-aus)^2))
#Esto es lo que hace la distancia manhattan(ej: albania y austria)
sum(abs(alb-aus))

## ________________Clusterings jeraquicos Divisitivos(diana)__________

install.packages("cluster")
library(cluster)
dv <- diana(data, metric = "euclidean")
par(mfrow=c(1,2))
plot(dv)

