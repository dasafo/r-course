data <- read.csv("../data/tema2/auto-mpg.csv", 
                 header = TRUE,
                 stringsAsFactors = FALSE)

#Nos interesa que la variable numerica de los cilindros pase a categórica
#para poder clasificar los coches por su número de cilindros
data$cylinders <- factor(data$cylinders, 
                         levels = c(3,4,5,6,8),
                         labels = c("3cil", "4cil", "5cil", "6cil", "8cil"))


summary(data) #vemos que en cilidnros no da el número por cada tipo de cilindros

str(data) #Para ver la estrucura del df


summary(data$cylinders)
summary(data$mpg)
str(data$cylinders)



install.packages("BiocManager")
library("BiocManager")
BiocManager::install("genefilter")
install.packages(c("modeest", "raster", "moments"))
library(modeest) #moda
library(raster) #quantiles, cv
library(moments) # asimetría, curtosis

X = data$mpg

#### Medidas de Centralización
mean(X) #sum(X)/length(X)
median(X)
mfv(X) #Most Frequence Value (la Moda), el valor que mas aparece
quantile(X) #Lso cuartiles

#### Medidas de Dispersión
var(X) #Varianza = s^2
sd(X) #Desviación típica sqrt(s^2) = s
cv(X) #Coeficiente de variación Cv = sd(X)/mean(X) * 100
#raster::cv(X) #otra forma de determinar el coeficiente de variación Cv

#### Medidas de asimetría
skewness(X) #Asimetria de Fisher(por ejemplo). 
#Forma de la curva(gaussiana por ejemplo): 
#Asimetria negativa (<0) --> ladeada ala derecha de la media
#Asimetria positiva (>0) --> curva ladeada a la izquierda de la media
#Simétrica (=0) --> Curva centrada en la media. Perfectamente simétrica

moments::kurtosis(X) #Cómo de 'picuda' es la curva:
#Mesocúrtica (=0) --> Bien balanceda, Valores bien distribuidos tanto en el pico como en 
#                     las colas(extremos de la curva)
#Leptocúrtica (>0) --> Valores muy concetrados en el pico y sin apenas en las colas
#Platicúrtica (<0) --> Valores muy concentrados en las colas y poco en el pico
#---------------------------------------------------

par(mfrow = c(1,1))
hist(X)

