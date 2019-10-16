install.packages("XML")
library(XML)

url <- "../data/tema1/cd_catalog.xml"

xmldoc <- xmlParse(url) #XMLInternalDocument Esto nos apunta al documento, un apuntador
rootnode <- xmlRoot(xmldoc) #nos coloca en el origen(prinicpio) del fichero
rootnode[2] #accedemos al segundo elemento del fichero

#Creamos un dataframe. Hay que pasarle el nodo raiz y undicarle una función 
cds_data <- xmlSApply(rootnode, function(x) xmlSApply(x, xmlValue) ) #vemos que el df no esta bien ordenado
cds.catalog <- data.frame(t(cds_data), row.names = NULL) #realizamos una transposicion para visualuzarla bien
head(cds.catalog, 2)
cds.catalog[1:5,]

#xpathSApply()
#getNodeSet() #para obtener conjunto de nodos


population_url <- "../data/tema1/WorldPopulation-wiki.htm"
tables <- readHTMLTable(population_url)

most_populated <- tables[[6]] #para acceder a los 10 paises mas poblados(ver tabla)
head(most_populated, 3)

custom_table <- readHTMLTable(population_url, which = 6) #otra forma de sacar esa tabla de poblaciones

