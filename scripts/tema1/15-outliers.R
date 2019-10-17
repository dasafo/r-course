ozone.data <- read.csv("../data/tema1/ozone.csv", stringsAsFactors = F)

boxplot(ozone.data$pressure_height, 
        main = "Pressure Height",
        boxwex = 0.5)$out #out para que nos muestre en consola los outliers


summary(ozone.data$pressure_height)


boxplot(pressure_height ~ Month, 
        data = ozone.data,
        main = "Presure Height per Month"
        )

boxplot(ozone_reading ~Month,
        data = ozone.data,
        main = "Ozone reading per Month")$out

mtext("Hola")

#---------------------Trabajar con los outliers-----------------------------

#________________1- Cambiarlos por el promedio o la mediana_________________

impute_outliers <- function(x, removeNA = TRUE){
  quantiles <- quantile(x, c(0.05, 0.95), na.rm = removeNA)
  x[x<quantiles[1]] <- mean(x, na.rm = removeNA) #si es menor que el 1er(5%=0.05) cuantil 
  x[x>quantiles[2]] <- median(x, na.rm = removeNA)#si es menor que del 2er(95%=0.95) cuantil
  x
}

imputed_data <- impute_outliers(ozone.data$pressure_height)

par(mfrow = c(1,2)) #con par() estructuramos los datos

boxplot(ozone.data$pressure_height, main = "Presión con outliers")
boxplot(imputed_data, main = "Presión sin outliers")



#______________________2- LLevar los outliers a los bigotes____________________

replace_outliers <- function(x, removeNA = TRUE){
  qrts <- quantile(x, probs = c(0.25, 0.75), na.rm = removeNA)
  caps <- quantile(x, probs = c(.05, .95), na.rm = removeNA)
  iqr <- qrts[2]-qrts[1] #rango intercuartilico
  h <- 1.5 * iqr
  x[x<qrts[1]-h] <- caps[1] #Lleva los que esten por debajo del 1er cuartil a un 5%
  x[x>qrts[2]+h] <- caps[2] #Lleva los que estén por encima del 3 cuartil
  x
}

capped_pressure_height <- replace_outliers(ozone.data$pressure_height)

par(mfrow = c(1,2))
boxplot(ozone.data$pressure_height, main = "Presión con outliers")
boxplot(capped_pressure_height, main = "Presión sin outliers")


