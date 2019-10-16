install.packages("jsonlite")
library(jsonlite)


dat.1 <- fromJSON("../data/tema1/students.json")
dat.2 <- fromJSON("../data/tema1/student-courses.json")

#toJASON() #para guardar datos de un df en un JSON

install.packages('curl')
library(curl)
#cargamos una web en formato JSON que nos informa del cambio de las divisas
url <- "https://finance.yahoo.com/webservice/v1/symbols/allcurrencies/quote?format=json"
currencies <- fromJSON(url) #leemos el JSON

currency.data <- currencies$list$resources$resource$fields
currency.data[1:5,1:2]


head(dat.1, 3)
dat.1$Email
dat.1[c(2,5,8),]
dat.1[,c(2,5)]

head(dat.2,3)
