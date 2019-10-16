data <- read.csv("../data/tema1/missing-data.csv", na.strings = "")

#1ª Forma:
#Creamos una nueva columna llamada Income.mean
data$Income.mean <- ifelse(is.na(data$Income), #si hay NA en Income...
                           mean(data$Income, na.rm = TRUE), #haz el prodmedio sin los NA...
                           data$Income # y sino dejalo como esta
                           )

#2ª Forma:
#Creamos una funcion de remplazo aleatorio para los NA
rand.impute <- function(x) { #x es un vector de datos que puede contener NA
  # missing contiene un vector de valores True/False dependiendo del NA de x
  missing <- is.na(x)
  #n.missing contiene cuantos valores son NA dentro de x
  n.missing <- sum(missing) #suma los True(los NA)
  #x.obs son los valores conocidos que tienen dato diferente de NA en x
  x.obs <- x[!missing]
  #por defecto, devolveré lo mismo que había entrado por parámetro
  imputed <- x
  #extremos una muestra aleatoria simple
  #en los valores que faltaban, los reemplazamos por una muestra
  #de los que si conocemos (MAS)
  imputed[missing] <- sample(x.obs, n.missing, replace = TRUE)
  return (imputed)
}


random.impute.data.frame <- function(dataframe, cols){
  names <- names(dataframe)
  for(col in cols){
    name <- paste(names[col], "imputed", sep = ".")
    dataframe[name] = rand.impute(dataframe[,col])
  }
  dataframe
}


data <- read.csv("../data/tema1/missing-data.csv", na.strings = "")
data$Income[data$Income==0]<-NA
data <- random.impute.data.frame(data, c(1,2))

