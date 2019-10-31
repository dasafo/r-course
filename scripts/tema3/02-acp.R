
#------ Análisis de componentes principales (acp)-------
#Es una técnica estadística que se usa para reducir la dimensionalidad,
#que transforma los datos que tienen muchas dimensiones en espacios que tengan
#menos dimensiones. En definitiva, para quedarnos con menos variables
#que sean más significativas.

usarrests <- read.csv("../data/tema3/USArrests.csv", stringsAsFactors = F)
#asignamos los nombres de las filas a us Estado(en vez de  1,2,3,4,...)
rownames(usarrests) <- usarrests$X 
usarrests$X <- NULL #la columna X nos sobra, y la quitamos
head(usarrests)

apply(usarrests, 2, var) #aplicamos la varianza por columnas(=2)

acp <- prcomp(usarrests, 
              center = TRUE, scale = TRUE)
#con 'center' le restamos la media, 'scale' para que lo divida por la desviacion tipica
print(acp)
#PC1,PC2,PC3,PC4 con las componentes principales

plot(acp, type = "l")

summary(acp)

biplot(acp, scale = 0)


pc1 <- apply(acp$rotation[,1]*usarrests, 1, sum)
pc2 <- apply(acp$rotation[,2]*usarrests, 1, sum)

usarrests$pc1 <- pc1 
usarrests$pc2 <- pc2
usarrests[,1:4] <- NULL
