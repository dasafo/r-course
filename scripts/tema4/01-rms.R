## Root means square error(Error cuadratico medio)
#restamos la prediccion al real, lo elevamos al cuadrado sumamos todoy
#lo dividimos  por el numero de muestras. Luego hacemos la raiz cuadrada

dat <- read.csv("../data/tema4/rmse.csv")

rmse<-sqrt(mean((dat$price - dat$pred)^2))
rmse #Da +/-2.934995

plot(dat$price, dat$pred, xlab = "Actual", ylab="Predicho", col='red')
abline(0,1)

#Creamos una funcion que calcule el rms
rmse <- function(actual, predicted){
  return(sqrt(mean((actual-predicted)^2)))
}

rmse(dat$price, dat$pred)
