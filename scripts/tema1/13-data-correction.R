install.packages("tidyr")
library(tidyr)

crime.data <- read.csv("../data/tema1/USArrests.csv", 
                       stringsAsFactors = FALSE)

View(crime.data)

#Añadimos una columna adicional al dataset con cbind()
crime.data <- cbind(state = rownames(crime.data), crime.data)

#------------------------gather---------------------------------------

#Juntamos diferentes columnas en otro dataframe:
crime.data.1 <- gather(crime.data,
                       key = "crime_type", #nombre que llevara la columna que será clave
                       value = "arrest_estimate",
                       Murder : UrbanPop) #desde Murder hasta UrbanPop

#todas las variables deben ser traducidas a la columna calve-valor salvo una
crime.data.2 <- gather(crime.data,
                       key = "crime_type",
                       value = "arrest_estimate",
                       -state) #Todas salvo state

crimate.data.3 <- gather(crime.data,
                         key = "crime_type",
                         value = "arrest_estimate",
                         Murder, Assault) #solo pille estas dos

#-----------------------spread-------------------------------

#funcion contraria a gather
crime.data.4 <- spread(crime.data.2, 
                       key = "crime_type",
                       value = "arrest_estimate") 

#-----------------------unite-------------------------------

#unimos varias columnas en una sola
crime.data.5 <- unite(crime.data,
                      col = "Murder_Assault",
                      Murder, Assault, 
                      sep = "_")

#--------------------separate------------------------

#Lo contrario que unite, para separar
crime.data.6 <- separate(crime.data.5,
                         col= "Murder_Assault",
                         into = c("Murder", "Assault"),
                         sep = "_")
