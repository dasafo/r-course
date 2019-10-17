install.packages("caret")
library(caret)
data <- read.csv("../data/tema2/BostonHousing.csv")

#------------- Vamos a crear una partición con variables numéricas----------

training.ids <- createDataPartition(data$MEDV, p = 0.8, list = F) #queremos el 80%(p=0.8) de los valores(aleatorios)
data.training <- data[training.ids,] #para entrenar
data.validation <- data[-training.ids,] #con el 20% restante(los que no están en trining.ids)

## Lo mismo que arriba pero dividiendo el de validation a su vez en otros 2(validation y training:
training.ids.2 <- createDataPartition(data$MEDV, p = 0.7, list = F) #nos quedamos con el 70%
data.training.2 <- data[training.ids.2,] #para entrenar
temp <- data[-training.ids.2,]
validation.ids.2 <- createDataPartition(temp$MEDV, p = 0.5, list = F)
data.validation <- temp[validation.ids.2,]
data.testing <- temp[-validation.ids.2,]


#------------- Vamos a crear una partición con variables categóricas----------

data2 <- read.csv("../data/tema2/boston-housing-classification.csv")
training.ids.3 <- createDataPartition(data2$MEDV_CAT, p = 0.7, list = F)
data.training.3 <- data2[training.ids.3,]
data.validation.3 <- data2[-training.ids.3,]


rda.cb.partition2 <- function(dataframe, target.index, prob){
  library(caret)
  training.ids <- createDataPartition(dataframe[,target.index], p=prob, list = FALSE)
  list(train = dataframe[training.ids,], val = dataframe[-training.ids,])
}

rda.cb.partition3 <- function(dataframe, target.index,
                              prob.train, prob.val){
  library(caret)
  training.ids <- createDataPartition(dataframe[,target.index], p = prob.train, list = FALSE)
  train.data <- dataframe[training.ids,]
  temp <- dataframe[-training.ids,]
  validation.ids <- createDataPartition(temp[,target.index], p = prob.val, list = FALSE)
  list(train = train.data, val = temp[validation.ids,], test = temp[-validation.ids,])
}



data1 <- rda.cb.partition2(data, 14, 0.8)
data2 <- rda.cb.partition3(data2, 14, 0.7, 0.5)

head(data1$val)
head(data2$val)

nrow(data)

sample1 <- sample(data$CRIM, 40, replace = F)
