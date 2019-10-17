#split / unsplit
data <- read.csv("../data/tema2/auto-mpg.csv", stringsAsFactors = F)

carslist <- split(data, data$cylinders) #para dividir segun los cilindros (=gruopby de Python)

#Accedemos al primer valor de carlist que son el de 3 cilindros
#Con un corchete acedemos al valor de la lista, y con 2 a su valor asociado(que es un df)
carslist[1] #no es un data frame, es una lista, por eso no hace falta indicar siempre [.. , ..]
carslist[[1]] #pero el valor interno si que es un df, que lo podemos llamar así

str(carslist[[1]]) #accedemos a la estructura del df
str(carslist[1]) #accedemos a la estructura de la lista

names(carslist[[1]])
names(carslist[1])
