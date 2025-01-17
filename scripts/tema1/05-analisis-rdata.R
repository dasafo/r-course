clientes <- c("Juan Gabriel", "Ricardo", "Pedro")
fechas <- as.Date(c("2017-12-27", "2017-11-1", "2018-1-1")) #objetos tipo Date
pago <- c(315, 192.55, 40.15)
pedidos <- data.frame(clientes, fechas, pago)

clientes_vip <- c("Juan Gabriel", "Ricardo")

save(pedidos, clientes_vip, file = "../data/tema1/pedidos.Rdata") #guardamos el df
saveRDS(pedidos, file="../data/tema1/pedidos.rds")

remove(pedidos)


load("../data/tema1/pedidos.Rdata") #cargamos el fichero RDS

orders <- readRDS("../data/tema1/pedidos.rds") #leemos el fichero RDS

#para cargar datasets de R
data(iris)
data(cars)

save.image(file = "../data/tema1/alldata.Rdata") #para guardar todo


primes <- c(2,3,5,7,11,13)
pow2 <- c(2,4,8,16,32,64,128)
save(list = c("primes", "pow2"), file = "../data/tema1/primes_and_pow2.Rdata")

attach("../data/tema1/primes_and_pow2.Rdata")

data()

