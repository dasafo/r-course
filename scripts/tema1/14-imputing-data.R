
housing.data <- read.csv("../data/tema1/housing-with-missing-value.csv",
                         header = TRUE,
                         stringsAsFactors = FALSE)

#_________________________Rellenar NAs__________________________________

#-------------------------Con mice y complete()-----------------------

library(mice)
columns <- c("ptratio", "rad")

imputed_data <- mice(housing.data[,names(housing.data) %in% columns],
                     m = 5, #numero de imputaciones(calculos) que queremos hacer
                     maxit = 50, #Numero máximo de iteraciones
                     method = "pmm", #predictid mean machine(comparacion predictiva de medias)
                     seed = 2018)

## Tipos de algoritmos para el method:
## pmm - comparación predictiva de medias
## logreg - regresión logistica
## polyreg - regresión logística politómica
## polr - modelo de probabilidades proporcionales. 

summary(imputed_data)

#rellenamos las columnas ptratio y rad con la funcón generada arriba para los NA
complete.data <- mice::complete(imputed_data) 
#como la función Curl también tiene un paquete llamado complete() necesitamos
#especificar el paquete (mice::complete())

#le pasamos esas columnas al arvhico original
housing.data$ptratio <- complete.data$ptratio
housing.data$rad <- complete.data$rad

anyNA(housing.data) #como vemos, ya no tenemos NA



#---------------------Con aregImpute() de HMisc-------------------------------

library(Hmisc)

impute_arg <- aregImpute(~ptratio + rad, data = housing.data, n.impute = 5)

impute_arg

impute_arg$imputed$rad

