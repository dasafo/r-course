install.packages("scales")
library(scales)
students <- read.csv("../data/tema1/data-conversion.csv")

#El reescalado hace que el valor mas pequeño tome el valor 0 y el máximo 1,
#Es como normalizar, pero si ponemos el argumento to=, podemos elegir el intervalalo(ver mas abajo)
students$Income.rescaled <- rescale(students$Income)

#Forma manual de reescalar, sin usar rescale()
(students$Income - min(students$Income))/
  (max(students$Income) - min(students$Income))

rescale(students$Income, to = c(0, 100)) #reescalamos de 0 a 100(en vez de 0 a 1)


#Función para reescalar todas las variables y no solo Income
rescale.many <- function(dataframe, cols){
  names <- names(dataframe)
  for(col in cols){
    name <- paste(names[col], "rescaled", sep = ".")
    dataframe[name] <- rescale(dataframe[,col]) 
  }
  cat(paste("Hemos reescalado ", length(cols), " variable(s)"))
  dataframe
}

students <- rescale.many(students, c(1,4))
