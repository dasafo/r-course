housing <- read.csv("../data/tema1/BostonHousing.csv")

#--------------------------Normalización(Gaussiana)----------------------------
#Z=(X-mu)/sigma

#siendo mu, la media(mean). sigma la desviación tipica(sd), y Z la normalización

#sd = sqrt(sum(x^2)/(n-1))
#-------------------------------------------------------------------------------

#scale para normalizar, pero solo funciona para variables numericas
#center se refiere a la media(mu) y escale a la desviación tipica(sigma)
housing.z <- scale(housing, center = TRUE, scale = TRUE) 
housing.mean <- scale(housing, center = TRUE, scale = FALSE) 
housing.sd <- scale(housing, center = FALSE, scale = TRUE)
housing.none <- scale(housing, center = FALSE, scale = FALSE) 


#funcion para reescalar varias variables de golpe(¡¡OJO: solo pueden ser numericas!!)
scale.many = function(dataframe, cols){
  names <- names(dataframe)
  for(col in cols){
    name <- paste(names[col], "z", sep = ".")
    dataframe[name] <- scale(dataframe[,col])
  }
  cat(paste("Hemos normalizado ", length(cols), " variable(s)"))
  dataframe
}

housing <- scale.many(housing, c(1, 3, 5:8))
