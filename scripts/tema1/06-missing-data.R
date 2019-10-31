#cargamos y convertimos los espaicos en blanco en NA
data <- read.csv("../data/tema1/missing-data.csv", na.strings = "") 

#quitamos las filas donde tengamos NA
data.cleaned <- na.omit(data)


is.na(data[4,2]) #nos dice si la fila 4 columna dos del df data es NA
is.na(data[4,1])
is.na(data$Income)

#Limpiar NA de solamente la variable Income (limpieza por filas)
data.income.cleaned <- data[!is.na(data$Income),] 

#Filas completas para un data frame. Devuelve booleano True con las filas sin NA(y vicervesa)
complete.cases(data)
data.cleaned.2 <- data[complete.cases(data), ]

#Convertir los ceros de ingresos en NA
data$Income[data$Income == 0] <- NA

#Medidas de centralización y dispersión
mean(data$Income)
mean(data$Income, na.rm = TRUE)

sd(data$Income)
sd(data$Income, na.rm = TRUE)
