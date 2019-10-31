housing.data <- read.csv("../data/tema1/housing-with-missing-value.csv",
                         header = TRUE, stringsAsFactors = FALSE)


summary(housing.data)

##ELiminar todas las observaciones que contengan algún NA
housing.data.1 <- na.omit(housing.data)
summary(housing.data.1)

##Eliminar las NAs de ciertas columnas
drop_na <- c("rad")
housing.data.2 <- housing.data[ #nos quedamos con las filas que no tienen NA y todas las columnas
  complete.cases(housing.data[,!(names(housing.data))%in% drop_na]),]
#complete.cases devuelve los casos completos de las variables que 
#no se encuentran dentro(%in%) de drop_na

summary(housing.data.2)

##Eliminar toda una columna
housing.data$rad <- NULL
summary(housing.data)

drops <- c("rad", "ptratio")
housing.data.3 <- housing.data[,!(names(housing.data) %in% drops)]
summary(housing.data.3)

#----------------------------------------------------------------

install.packages("Hmisc")
library(Hmisc)

#hacemos una copia del df
housing.data.copy1 <- housing.data 

#con impute() cambiamos NA por constantes, en este caso la media
housing.data.copy1$ptratio <- impute(housing.data.copy1$ptratio, mean)
housing.data.copy1$rad <- impute(housing.data.copy1$rad, mean)
summary(housing.data.copy1)

housing.data.copy2 <- housing.data

#con impute() cambiamos NA por constantes, en este caso la mediana
housing.data.copy2$ptratio <- impute(housing.data.copy2$ptratio, median)
housing.data.copy2$rad <- impute(housing.data.copy2$rad, median)
summary(housing.data.copy2)

housing.data.copy3 <- housing.data

#con impute() cambiamos NA por constantes, en este caso valores fijos
housing.data.copy3$ptratio <- impute(housing.data.copy3$ptratio, 18)
housing.data.copy3$rad <- impute(housing.data.copy3$rad, 7)
summary(housing.data.copy3)

#-------------Para ver el patrón de las variables que faltan-----------------

#_____1ª forma con MICE:______

install.packages("mice")
library(mice)
md.pattern(housing.data)

#Lo que vemos por filas:
#tenemos 431 en los que todo es conocido (fila 431 todo a 1)
#35 valores que conoce todo menos ptratio(habrá un NA aqui)
#35 valores que conoce todo menos rad(habrá un NA aqui)
#5 valores que tiene todo menos rad y ptratio(habrá un NA en cada una)

#Lo que vemos por columnas:
#No falta ningún dato en ninguna variable salvo en rad(40 datos) y pratio(40 datos)


#_____2ª forma con VIM:_______
install.packages("VIM")
library(VIM)

aggr(housing.data,
     col= c('green', 'red'),
     numbers = TRUE, 
     sortVars = TRUE,
     labels = names(housing.data),
     cex.axis = 0.75, #tamaño de las letras
     gap = 1, #para acercar ambas gráficas
     ylab = c("Histograma de NAs", "Patrón")
)     
